/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */
use std::cmp::{self, Ordering};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::hash::{self, Hash};
#[cfg(feature = "image")]
use std::io::Cursor;
use std::io::Write;
use std::os::fd::AsFd;
use std::sync::atomic::{self, AtomicBool};
use std::sync::Arc;
#[cfg(feature = "image")]
use std::sync::RwLock;

use anyhow::Result;
use chrono::offset::{Local, TimeZone};
use chrono::{DateTime, FixedOffset, Local as LocalTz};
#[cfg(feature = "image")]
use image::io::Reader as ImageReader;
#[cfg(feature = "image")]
use sixel_image::SixelImage;
use terminus::{
    self, term_string_visible_len, Dimensions, MeasureSpec, MeasureSpecs, RequestedDimension,
    RequestedDimensions, Screen, View,
};
use termion::color;
use unicode_segmentation::UnicodeSegmentation as _;
use uuid::Uuid;
use xmpp_parsers::delay::Delay;
use xmpp_parsers::message::{Message as XmppParsersMessage, MessageType as XmppParsersMessageType};
use xmpp_parsers::oob::Oob;
use xmpp_parsers::{BareJid, Jid};

use crate::account::Account;
use crate::color::id_to_rgb;
#[cfg(feature = "image")]
use crate::core::Aparte;
use crate::core::AparteAsync;
#[cfg(feature = "image")]
use crate::core::Event;
use crate::i18n;
#[cfg(feature = "image")]
use crate::image::convert_to_sixel;

#[derive(Debug, Clone)]
pub struct XmppMessageVersion {
    pub id: String,
    pub timestamp: DateTime<FixedOffset>,
    pub bodies: HashMap<String, String>,
    pub oobs: Vec<Oob>,
}

impl Eq for XmppMessageVersion {}

impl PartialEq for XmppMessageVersion {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Ord for XmppMessageVersion {
    fn cmp(&self, other: &Self) -> Ordering {
        if self == other {
            Ordering::Equal
        } else {
            self.timestamp.cmp(&other.timestamp)
        }
    }
}

impl PartialOrd for XmppMessageVersion {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl XmppMessageVersion {
    pub fn get_best_body<'a>(&'a self, prefered_langs: Vec<&str>) -> &'a String {
        i18n::get_best(&self.bodies, prefered_langs).unwrap().1
    }
}

#[derive(Debug, Clone)]
pub struct VersionedXmppMessage {
    pub id: String,
    pub from: BareJid,
    pub from_full: Jid,
    pub to: BareJid,
    pub to_full: Jid,
    pub history: Vec<XmppMessageVersion>,
    pub type_: XmppMessageType,
    pub direction: Direction,
    pub archive: bool,
}

impl VersionedXmppMessage {
    pub fn get_last_bodies(&self) -> impl Iterator<Item = (&String, &String)> {
        let last = self.history.iter().max().unwrap();
        last.bodies.iter()
    }
    pub fn get_last_body(&self) -> &str {
        let last = self.history.iter().max().unwrap();
        last.get_best_body(vec![])
    }

    pub fn get_original_timestamp(&self) -> &DateTime<FixedOffset> {
        let first = self.history.iter().min().unwrap();
        &first.timestamp
    }

    pub fn add_version_from_xmpp(&mut self, message: &XmppParsersMessage) {
        let id = message
            .id
            .clone()
            .unwrap_or_else(|| Uuid::new_v4().to_string());
        let bodies: HashMap<String, String> = message
            .bodies
            .iter()
            .map(|(lang, body)| (lang.clone(), body.0.clone()))
            .collect();

        let oobs: Vec<Oob> = message
            .payloads
            .iter()
            .filter_map(|payload| Oob::try_from(payload.clone()).ok())
            .collect();

        let delay = message
            .payloads
            .iter()
            .find_map(|payload| Delay::try_from(payload.clone()).ok());
        let timestamp = delay
            .map(|delay| delay.stamp.0)
            .unwrap_or(LocalTz::now().into());

        self.history.push(XmppMessageVersion {
            id,
            timestamp,
            bodies,
            oobs,
        });
    }

    pub fn has_multiple_version(&self) -> bool {
        self.history.len() > 1
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum XmppMessageType {
    Chat,
    Channel,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Direction {
    Incoming,
    Outgoing,
}

#[derive(Debug, Clone)]
pub struct LogMessage {
    pub id: String,
    pub timestamp: DateTime<FixedOffset>,
    pub body: String,
}

#[derive(Debug, Clone)]
pub enum Message {
    Xmpp(VersionedXmppMessage),
    Log(LogMessage),
}

impl Message {
    pub fn from_xmpp(
        account: &Account,
        message: &XmppParsersMessage,
        delay: &Option<Delay>,
        archive: bool,
    ) -> Result<Self, ()> {
        let id = message
            .id
            .clone()
            .unwrap_or_else(|| Uuid::new_v4().to_string());
        if let Some(from) = message.from.clone() {
            let bodies: HashMap<String, String> = message
                .bodies
                .iter()
                .map(|(lang, body)| (lang.clone(), body.0.clone()))
                .collect();
            let oobs: Vec<_> = message
                .payloads
                .iter()
                .filter_map(|payload| Oob::try_from(payload.clone()).ok())
                .collect();
            let delay = match delay {
                Some(delay) => Some(delay.clone()),
                None => message
                    .payloads
                    .iter()
                    .find_map(|payload| Delay::try_from(payload.clone()).ok()),
            };
            let timestamp = delay
                .map(|delay| delay.stamp.0)
                .unwrap_or(LocalTz::now().into());
            let to = match message.to.clone() {
                Some(to) => to,
                None => account.clone().into(),
            };

            match message.type_ {
                XmppParsersMessageType::Chat => {
                    if from.clone().node() == account.node()
                        && from.clone().domain() == account.domain()
                    {
                        Ok(Message::outgoing_chat(
                            id,
                            timestamp,
                            &from,
                            &to,
                            bodies,
                            Some(oobs),
                            archive,
                        ))
                    } else {
                        Ok(Message::incoming_chat(
                            id,
                            timestamp,
                            &from,
                            &to,
                            bodies,
                            Some(oobs),
                            archive,
                        ))
                    }
                }
                XmppParsersMessageType::Groupchat => Ok(Message::incoming_channel(
                    id,
                    timestamp,
                    &from,
                    &to,
                    bodies,
                    Some(oobs),
                    archive,
                )),
                _ => Err(()),
            }
        } else {
            Err(())
        }
    }

    pub fn get_local_destination_from_xmpp<'a>(
        account: &Account,
        message: &'a XmppParsersMessage,
    ) -> Result<&'a Jid, String> {
        match Message::get_direction_from_xmpp(account, message)? {
            Direction::Incoming => message.from.as_ref().ok_or(String::from(
                "Missing 'from' attribute for incoming message",
            )),
            Direction::Outgoing => message
                .to
                .as_ref()
                .ok_or(String::from("Missing 'to' attribute for outgoing message")),
        }
    }

    pub fn get_direction_from_xmpp(
        account: &Account,
        message: &XmppParsersMessage,
    ) -> Result<Direction, String> {
        let from: Option<BareJid> = message.from.as_ref().map(|f| f.to_bare());
        let to: Option<BareJid> = message.to.as_ref().map(|f| f.to_bare());
        let bare_account: BareJid = account.to_bare();

        match (from.as_ref(), to.as_ref()) {
            (Some(from), Some(_to)) => {
                if from == &bare_account {
                    Ok(Direction::Outgoing)
                } else {
                    Ok(Direction::Incoming)
                }
            }
            (None, Some(to)) => {
                if to == &bare_account {
                    Ok(Direction::Incoming)
                } else {
                    Ok(Direction::Outgoing)
                }
            }
            (Some(from), None) => {
                if from == &bare_account {
                    Ok(Direction::Outgoing)
                } else {
                    Ok(Direction::Incoming)
                }
            }
            (None, None) => Err("Message as no 'from' nor 'to' attributes".to_string()),
        }
    }

    pub fn incoming_chat<I: Into<String>>(
        id: I,
        timestamp: DateTime<FixedOffset>,
        from: &Jid,
        to: &Jid,
        bodies: HashMap<String, String>,
        oobs: Option<Vec<Oob>>,
        archive: bool,
    ) -> Self {
        let id = id.into();

        let version = XmppMessageVersion {
            id: id.clone(),
            timestamp,
            bodies,
            oobs: oobs.unwrap_or_default(),
        };

        Message::Xmpp(VersionedXmppMessage {
            id,
            from: from.to_bare(),
            from_full: from.clone(),
            to: to.to_bare(),
            to_full: to.clone(),
            history: vec![version],
            type_: XmppMessageType::Chat,
            direction: Direction::Incoming,
            archive,
        })
    }

    pub fn outgoing_chat<I: Into<String>>(
        id: I,
        timestamp: DateTime<FixedOffset>,
        from: &Jid,
        to: &Jid,
        bodies: HashMap<String, String>,
        oobs: Option<Vec<Oob>>,
        archive: bool,
    ) -> Self {
        let id = id.into();

        let version = XmppMessageVersion {
            id: id.clone(),
            timestamp,
            bodies,
            oobs: oobs.unwrap_or_default(),
        };

        Message::Xmpp(VersionedXmppMessage {
            id,
            from: from.to_bare(),
            from_full: from.clone(),
            to: to.to_bare(),
            to_full: to.clone(),
            history: vec![version],
            type_: XmppMessageType::Chat,
            direction: Direction::Outgoing,
            archive,
        })
    }

    pub fn incoming_channel<I: Into<String>>(
        id: I,
        timestamp: DateTime<FixedOffset>,
        from: &Jid,
        to: &Jid,
        bodies: HashMap<String, String>,
        oobs: Option<Vec<Oob>>,
        archive: bool,
    ) -> Self {
        let id = id.into();

        let version = XmppMessageVersion {
            id: id.clone(),
            timestamp,
            bodies,
            oobs: oobs.unwrap_or_default(),
        };

        Message::Xmpp(VersionedXmppMessage {
            id,
            from: from.to_bare(),
            from_full: from.clone(),
            to: to.to_bare(),
            to_full: to.clone(),
            history: vec![version],
            type_: XmppMessageType::Channel,
            direction: Direction::Incoming,
            archive,
        })
    }

    pub fn outgoing_channel<I: Into<String>>(
        id: I,
        timestamp: DateTime<FixedOffset>,
        from: &Jid,
        to: &Jid,
        bodies: HashMap<String, String>,
        oobs: Option<Vec<Oob>>,
        archive: bool,
    ) -> Self {
        let id = id.into();

        let version = XmppMessageVersion {
            id: id.clone(),
            timestamp,
            bodies,
            oobs: oobs.unwrap_or_default(),
        };

        Message::Xmpp(VersionedXmppMessage {
            id,
            from: from.to_bare(),
            from_full: from.clone(),
            to: to.to_bare(),
            to_full: to.clone(),
            history: vec![version],
            type_: XmppMessageType::Channel,
            direction: Direction::Outgoing,
            archive,
        })
    }

    pub fn log(msg: String) -> Self {
        Message::Log(LogMessage {
            id: Uuid::new_v4().to_string(),
            timestamp: LocalTz::now().into(),
            body: msg,
        })
    }

    pub fn encryption_recipient(&self) -> Option<BareJid> {
        match self {
            Message::Log(_) => None,
            Message::Xmpp(message) => match message.direction {
                Direction::Outgoing => match message.type_ {
                    XmppMessageType::Chat => Some(message.to.clone()),
                    XmppMessageType::Channel => None, // TODO fetch all participants?
                },
                Direction::Incoming => None,
            },
        }
    }

    pub fn body(&self) -> &str {
        match self {
            Message::Xmpp(message) => message.get_last_body(),
            Message::Log(LogMessage { body, .. }) => body,
        }
    }

    pub fn id(&self) -> &str {
        match self {
            Message::Xmpp(VersionedXmppMessage { id, .. })
            | Message::Log(LogMessage { id, .. }) => id,
        }
    }

    pub fn timestamp(&self) -> &DateTime<FixedOffset> {
        match self {
            Message::Xmpp(message) => message.get_original_timestamp(),
            Message::Log(LogMessage { timestamp, .. }) => timestamp,
        }
    }
}

impl hash::Hash for Message {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.id().hash(state)
    }
}

impl PartialEq for Message {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl std::cmp::Eq for Message {}

impl Ord for Message {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.eq(other) {
            Ordering::Equal
        } else {
            self.timestamp().cmp(other.timestamp())
        }
    }
}

impl PartialOrd for Message {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl TryFrom<Message> for xmpp_parsers::Element {
    type Error = ();

    fn try_from(message: Message) -> Result<Self, Self::Error> {
        match message {
            Message::Log(_) => Err(()),
            Message::Xmpp(message) => match message.direction {
                Direction::Outgoing => match message.type_ {
                    XmppMessageType::Chat => {
                        let mut xmpp_message = xmpp_parsers::message::Message::new(Some(
                            Jid::from(message.to.clone()),
                        ));
                        xmpp_message.id = Some(message.id.clone());
                        xmpp_message.type_ = xmpp_parsers::message::MessageType::Chat;
                        xmpp_message.bodies = message
                            .get_last_bodies()
                            .map(|(lang, body)| {
                                (lang.clone(), xmpp_parsers::message::Body(body.clone()))
                            })
                            .collect();
                        Ok(xmpp_message.into())
                    }
                    XmppMessageType::Channel => {
                        let mut xmpp_message = xmpp_parsers::message::Message::new(Some(
                            Jid::from(message.to.clone()),
                        ));
                        xmpp_message.id = Some(message.id.clone());
                        xmpp_message.type_ = xmpp_parsers::message::MessageType::Groupchat;
                        xmpp_message.bodies = message
                            .get_last_bodies()
                            .map(|(lang, body)| {
                                (lang.clone(), xmpp_parsers::message::Body(body.clone()))
                            })
                            .collect();
                        Ok(xmpp_message.into())
                    }
                },
                Direction::Incoming => Err(()),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct MessageView {
    pub message: Message,
    dimensions: Option<Dimensions>,
    #[cfg(feature = "image")]
    image: Arc<RwLock<Option<SixelImage>>>,
    dirty: Arc<AtomicBool>,
}

impl Eq for MessageView {}

impl PartialEq for MessageView {
    fn eq(&self, other: &Self) -> bool {
        self.message.eq(&other.message)
    }
}

impl PartialOrd for MessageView {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for MessageView {
    fn cmp(&self, other: &Self) -> Ordering {
        self.message.cmp(&other.message)
    }
}

impl Hash for MessageView {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.message.hash(state)
    }
}

impl MessageView {
    #[cfg(not(feature = "image"))]
    pub fn new(_aparte: &mut AparteAsync, message: Message) -> Self {
        MessageView {
            message,
            dimensions: None,
            dirty: Arc::new(AtomicBool::new(true)),
        }
    }

    #[cfg(feature = "image")]
    pub fn new(aparte: &mut AparteAsync, message: Message) -> Self {
        let dirty = Arc::new(AtomicBool::new(true));
        let image = match &message {
            Message::Xmpp(message) => message
                .history
                .iter()
                .max()
                .and_then(|version| {
                    version
                        .oobs
                        .iter()
                        .find(|oob| oob.url.ends_with(".jpg"))
                        .map(|oob| {
                            let image: Arc<RwLock<Option<SixelImage>>> =
                                Arc::new(RwLock::new(None));
                            Aparte::spawn({
                                let url = oob.url.clone();
                                let image = Arc::clone(&image);
                                let mut aparte = aparte.clone();
                                let dirty = Arc::clone(&dirty);
                                async move {
                                    log::debug!("Loading OOB: {}", url);
                                    match Self::load_oob(&url).await {
                                        Ok(sixel) => {
                                            log::debug!("Loaded OOB from {}", url);
                                            let mut image = image.write().unwrap();
                                            *image = Some(sixel);
                                            dirty.store(true, atomic::Ordering::Relaxed);
                                            aparte.schedule(Event::UIRender(false));
                                        }
                                        Err(err) => log::error!("{}", err),
                                    }
                                }
                            });
                            image
                        })
                })
                .unwrap_or(Arc::new(RwLock::new(None))),
            Message::Log(_) => Arc::new(RwLock::new(None)),
        };
        MessageView {
            message,
            dimensions: None,
            image,
            dirty,
        }
    }

    #[cfg(feature = "image")]
    async fn load_oob(url: &str) -> Result<SixelImage> {
        let client = reqwest::Client::new();
        let response = client
            .get(url)
            .timeout(std::time::Duration::from_secs(180))
            .send()
            .await?;
        log::debug!(
            "Got http response, expected image size: {:?}",
            response.content_length()
        );
        let raw_image = response.bytes().await?;
        log::debug!("Got raw image, size: {}", raw_image.len());
        let image_reader = ImageReader::new(Cursor::new(raw_image)).with_guessed_format()?;
        log::debug!("Guessed image format: {:?}", image_reader.format());
        let image = image_reader.decode()?;
        let image = image.resize_to_fill(300, 300, image::imageops::FilterType::Nearest);

        log::debug!("Convert {} to oob", url);
        convert_to_sixel(image)
    }

    fn format_log(message: &LogMessage, max_width: Option<u16>) -> Vec<String> {
        let timestamp = Local.from_utc_datetime(&message.timestamp.naive_local());
        let mut lines = Vec::new();
        for line in message.body.lines() {
            lines.append(&mut Self::format_text(
                format!(
                    "{}{}{} - {}",
                    color::Bg(color::Reset),
                    color::Fg(color::Reset),
                    timestamp.format("%T"),
                    line
                ),
                max_width,
            ))
        }
        lines
    }

    fn format_header(message: &VersionedXmppMessage) -> String {
        let author = terminus::clean_str(&match &message.type_ {
            XmppMessageType::Channel => match &message.from_full.try_as_full() {
                Ok(full_jid) => full_jid.resource().to_string(),
                Err(bare_jid) => bare_jid.to_string(),
            },
            XmppMessageType::Chat => message.from.to_string(),
        });

        let timestamp = Local.from_utc_datetime(&message.get_original_timestamp().naive_local());
        let body = message.get_last_body();
        let me = body.starts_with("/me");

        let (r, g, b) = id_to_rgb(&author);

        let mut attributes = "".to_string();
        if message.has_multiple_version() {
            attributes.push_str("✎ ");
        }

        match me {
            true => format!(
                "{}{}{} - {}* {}{}{}",
                color::Bg(color::Reset),
                color::Fg(color::Reset),
                timestamp.format("%T"),
                attributes,
                color::Fg(color::Rgb(r, g, b)),
                author,
                color::Fg(color::Reset)
            ),
            false => format!(
                "{}{}{} - {}{}{}:{} ",
                color::Bg(color::Reset),
                color::Fg(color::Reset),
                timestamp.format("%T"),
                attributes,
                color::Fg(color::Rgb(r, g, b)),
                author,
                color::Fg(color::Reset)
            ),
        }
    }

    fn format_xmpp_text(message: &VersionedXmppMessage, max_width: Option<u16>) -> Vec<String> {
        let mut buffer = Self::format_header(message);

        let padding_len = buffer.len();
        let padding = " ".repeat(padding_len);

        let body = message.get_last_body();
        let mut iter = body.strip_prefix("/me").unwrap_or(body).lines();

        if let Some(line) = iter.next() {
            buffer.push_str(&terminus::clean_str(line));
        }
        for line in iter {
            buffer.push_str(format!("\n{}{}", padding, terminus::clean_str(line)).as_str());
        }

        Self::format_text(buffer, max_width)
    }

    fn format(&self, max_width: Option<u16>) -> Vec<String> {
        match &self.message {
            Message::Log(message) => Self::format_log(message, max_width),
            Message::Xmpp(message) => Self::format_xmpp_text(message, max_width),
        }
    }

    fn format_text(text: String, max_width: Option<u16>) -> Vec<String> {
        let mut buffers: Vec<String> = Vec::new();
        for line in text.lines() {
            let mut words = line.split_word_bounds();

            let mut line_len = 0;
            let mut chunk = String::new();
            while let Some(word) = words.next() {
                let visible_word;
                let mut remaining = String::new();

                // We can safely unwrap here because split_word_bounds produce non empty words
                let first_char = word.chars().next().unwrap();

                if first_char == '\x1b' {
                    // Handle Escape sequence: see https://www.ecma-international.org/publications/files/ECMA-ST/Ecma-048.pdf
                    // First char is a word boundary
                    //
                    // We must ignore them for the visible length count but include them in the
                    // final chunk that will be written to the terminal

                    if let Some("[") = words.next() {
                        // Control Sequence Introducer are accepted and can safely be
                        // written to terminal
                        let mut escape = String::from("\x1b[");
                        let mut end = false;

                        for word in words.by_ref() {
                            for c in word.chars() {
                                // Push all char belonging to escape sequence
                                // but keep remaining for wrap computation
                                if !end {
                                    escape.push(c);
                                    match c {
                                        '\x30'..='\x3f' => {} // parameter bytes
                                        '\x20'..='\x2f' => {} // intermediate bytes
                                        '\x40'..='\x7e' => {
                                            // final byte
                                            chunk.push_str(&escape);
                                            end = true;
                                        }
                                        _ => {
                                            // Invalid escape sequence, just ignore it
                                            end = true;
                                        }
                                    }
                                } else {
                                    remaining.push(c);
                                }
                            }

                            if end {
                                break;
                            }
                        }
                    } else {
                        // Nothing is following the escape char
                        // We can simply ignore it
                    }
                    visible_word = remaining.as_str();
                } else {
                    visible_word = word;
                }

                if visible_word.is_empty() {
                    continue;
                }

                let grapheme_count = visible_word.graphemes(true).count();

                if max_width.map_or(false, |max_width| {
                    line_len + grapheme_count > max_width as usize
                }) {
                    // Wrap line
                    buffers.push(chunk);
                    chunk = String::new();
                    line_len = 0;
                }

                chunk.push_str(visible_word);
                line_len += grapheme_count;
            }

            buffers.push(chunk);
        }

        buffers
    }

    fn render_text<W>(&self, screen: &mut Screen<W>)
    where
        W: Write + AsFd,
    {
        let dimensions = self.dimensions.as_ref().unwrap();

        let mut top = dimensions.top;
        let formatted = self.format(Some(dimensions.width));

        // Format as much as possible starting from bottom line
        for line in &formatted[formatted.len() - dimensions.height as usize..] {
            if dimensions.left == 1 {
                // Use fast erase if possible
                terminus::goto!(screen, dimensions.left + dimensions.width, top);
                terminus::vprint!(screen, "{}", "\x1B[1K");
                terminus::goto!(screen, dimensions.left, top);
                terminus::vprint!(screen, "{}", line);
            } else {
                terminus::goto!(screen, dimensions.left, top);
                let padding = dimensions.width - term_string_visible_len(line) as u16;
                terminus::vprint!(screen, "{: <1$}", line, padding as usize);
            }
            top += 1;
        }
    }

    #[cfg(feature = "image")]
    fn render_image<W>(&self, screen: &mut Screen<W>)
    where
        W: Write + AsFd,
    {
        let Message::Xmpp(message) = &self.message else {
            unreachable!()
        };
        let dimensions = self.dimensions.as_ref().unwrap();

        terminus::clear_screen(dimensions, screen);

        let header = Self::format_header(message);

        terminus::goto!(screen, dimensions.left, dimensions.top);
        terminus::vprint!(screen, "{}", header);
        if let Some(image) = self.image.read().unwrap().as_ref() {
            terminus::vprint!(screen, "{}", image.serialize());
        } else {
            terminus::vprint!(screen, "…");
        }
    }

    fn measure_text(&self, measure_specs: &MeasureSpecs) -> RequestedDimensions {
        match measure_specs.width {
            MeasureSpec::Unspecified => RequestedDimensions {
                height: RequestedDimension::Absolute(1),
                width: RequestedDimension::Absolute(
                    self.format(None)
                        .first()
                        .map_or(0, |line| term_string_visible_len(line) as u16),
                ),
            },
            MeasureSpec::AtMost(at_most_width) => {
                let formatted = self.format(Some(at_most_width));
                RequestedDimensions {
                    height: RequestedDimension::Absolute(formatted.len() as u16),
                    width: RequestedDimension::Absolute(cmp::min(
                        formatted.iter().map(|line| line.len()).max().unwrap_or(0) as u16,
                        at_most_width,
                    )),
                }
            }
        }
    }

    #[cfg(feature = "image")]
    fn measure_image(&self, _measure_specs: &MeasureSpecs) -> RequestedDimensions {
        let image = self.image.read().unwrap();
        let image = image.as_ref().unwrap();
        let term_size_pixel =
            termion::terminal_size_pixels().expect("Can't get terminal pixel size");
        let term_size = termion::terminal_size().expect("Can't get terminal size");

        let resolution = (
            term_size_pixel.0 / term_size.0,
            term_size_pixel.1 / term_size.1,
        );

        let (x, y) = image.pixel_size();

        RequestedDimensions {
            height: RequestedDimension::Absolute((y as u16).div_ceil(resolution.1)),
            width: RequestedDimension::Absolute((x as u16).div_ceil(resolution.0)),
        }
    }
}

impl<E, W> View<E, W> for MessageView
where
    W: Write + AsFd,
{
    fn measure(&self, measure_specs: &MeasureSpecs) -> RequestedDimensions {
        // TODO: we could avoid creating the real buffers
        #[cfg(feature = "image")]
        if self.image.read().unwrap().is_some() {
            self.measure_image(measure_specs)
        } else {
            self.measure_text(measure_specs)
        }

        #[cfg(not(feature = "image"))]
        self.measure_text(measure_specs)
    }

    fn layout(&mut self, dimensions: &Dimensions) {
        log::debug!("layout {} {:?}", std::any::type_name::<Self>(), dimensions);

        if self.dimensions.as_ref() != Some(dimensions) {
            self.dirty.store(true, atomic::Ordering::Relaxed);
            self.dimensions.replace(dimensions.clone());
        }
    }

    fn render(&self, screen: &mut Screen<W>) {
        log::debug!(
            "rendering {} at {:?}",
            std::any::type_name::<Self>(),
            self.dimensions
        );
        if self.dirty.swap(false, atomic::Ordering::Relaxed) {
            #[cfg(feature = "image")]
            if self.image.read().unwrap().is_some() {
                self.render_image(screen)
            } else {
                self.render_text(screen)
            }

            #[cfg(not(feature = "image"))]
            self.render_text(screen)
        }
    }

    fn event(&mut self, _event: &mut E) {}

    fn set_dirty(&mut self) {
        self.dirty.store(true, atomic::Ordering::Relaxed);
    }

    fn is_dirty(&self) -> bool {
        self.dirty.load(atomic::Ordering::Relaxed)
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::fs::File;
    use std::rc::Rc;
    use std::time::UNIX_EPOCH;

    use chrono::Utc;
    use test_log::test;

    use termion::raw::IntoRawMode as _;
    use termion::screen::IntoAlternateScreen as _;

    use terminus::BufferedScreen;

    use super::*;

    struct MockWriter {
        stdout: Rc<RefCell<Vec<u8>>>,
        inner: File,
    }

    impl std::io::Write for MockWriter {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.stdout.borrow_mut().extend(buf);
            Ok(buf.len())
        }

        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    impl AsFd for MockWriter {
        fn as_fd(&self) -> std::os::unix::prelude::BorrowedFd<'_> {
            self.inner.as_fd()
        }
    }

    fn log_mesasge_view(log: &str) -> (impl View<(), MockWriter>, DateTime<Local>) {
        let epoch: DateTime<Utc> = DateTime::from(UNIX_EPOCH);
        (
            MessageView {
                message: Message::Log(LogMessage {
                    id: String::from(""),
                    timestamp: epoch.into(),
                    body: String::from(log),
                }),
                dimensions: None,
                #[cfg(feature = "image")]
                image: Arc::new(RwLock::new(None)),
                dirty: Arc::new(AtomicBool::new(true)),
            },
            Local.from_utc_datetime(&epoch.naive_utc()),
        )
    }

    fn mock_screen() -> (Rc<RefCell<Vec<u8>>>, Screen<MockWriter>) {
        let stdout = Rc::new(RefCell::new(Vec::new()));
        let mock_writer = MockWriter {
            stdout: stdout.clone(),
            inner: File::open("/dev/ptmx").unwrap(),
        };
        let mut screen = BufferedScreen::new(
            mock_writer
                .into_raw_mode()
                .unwrap()
                .into_alternate_screen()
                .unwrap(),
        );

        let _ = screen.flush();
        let _ = stdout.take();

        (stdout, screen)
    }

    fn raw_formatted_log_message_line(
        timestamp: Option<DateTime<Local>>,
        top: u16,
        width: u16,
        log: &str,
    ) -> String {
        if let Some(timestamp) = timestamp {
            format!(
                "{}{}{}{}{}{} - {}",
                termion::cursor::Goto(width + 1, top),
                "\x1B[1K",
                termion::cursor::Goto(1, top),
                color::Bg(color::Reset),
                color::Fg(color::Reset),
                timestamp.format("%T"),
                log,
            )
        } else {
            format!(
                "{}{}{}{}",
                termion::cursor::Goto(width + 1, top),
                "\x1B[1K",
                termion::cursor::Goto(1, top),
                log,
            )
        }
    }

    #[test]
    fn test_render_single_line() {
        // Given
        let (mut message_view, timestamp) = log_mesasge_view("a log");
        let (stdout, mut screen) = mock_screen();

        // When
        message_view.layout(&Dimensions {
            top: 1,
            left: 1,
            height: 1,
            width: 100,
        });
        message_view.render(&mut screen);

        // Then
        let _ = screen.flush();
        let output = stdout.take();
        assert_eq!(
            output,
            raw_formatted_log_message_line(Some(timestamp), 1, 100, "a log").as_bytes()
        );
    }

    #[test]
    fn test_render_multiple_lines() {
        // Given
        // a log that should render in more than width
        // 00:00:00 - a very very long long message log
        // is 44 char long but we allow only 2 lines of 40.
        let (mut message_view, timestamp) = log_mesasge_view("a very very long long message log");
        let (stdout, mut screen) = mock_screen();

        // When
        message_view.layout(&Dimensions {
            top: 1,
            left: 1,
            height: 2,
            width: 40,
        });
        message_view.render(&mut screen);

        // Then
        // we should render:
        // 00:00:00 - a very very long long message
        //  log
        let _ = screen.flush();
        let output = stdout.take();
        assert_eq!(
            output,
            format!(
                "{}{}",
                raw_formatted_log_message_line(
                    Some(timestamp),
                    1,
                    40,
                    "a very very long long message"
                ),
                raw_formatted_log_message_line(None, 2, 40, " log")
            )
            .as_bytes()
        );
    }

    #[test]
    fn test_render_partial_lines() {
        // Given
        // a log that should render in more than width
        // 00:00:00 - a very very long long message log
        // is 44 char long but we allow only 1 line of 40.
        let (mut message_view, _timestamp) = log_mesasge_view("a very very long long message log");
        let (stdout, mut screen) = mock_screen();

        // When
        message_view.layout(&Dimensions {
            top: 1,
            left: 1,
            height: 1,
            width: 40,
        });
        message_view.render(&mut screen);

        // Then
        // we should only render:
        //  log
        let _ = screen.flush();
        let output = stdout.take();
        assert_eq!(
            output,
            raw_formatted_log_message_line(None, 1, 40, " log").as_bytes()
        );
    }
}
