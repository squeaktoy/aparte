/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */
use std::cell::{Cell, RefCell};
use std::io::Write;
use std::os::fd::AsFd;
use std::rc::Rc;

use crate::{clear_screen, RequestedDimension};

use super::{
    Dimensions, EventHandler, LayoutParam, LayoutParams, MeasureSpecs, RequestedDimensions, Screen,
    View,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Orientation {
    Horizontal,
    Vertical,
}

pub struct Child<E, W> {
    pub view: Box<dyn View<E, W>>,
    weight: u16,
}

/// Component with multiple ordered children sharing the same space.
/// Each child is placed according to the orientation of the component.
pub struct LinearLayout<E, W> {
    pub orientation: Orientation,
    pub children: Vec<Child<E, W>>,
    pub event_handler: Option<EventHandler<Self, E>>,
    pub dirty: Cell<bool>,
    layouts: LayoutParams,
    dimensions: Option<Dimensions>,
}

impl<E, W> LinearLayout<E, W>
where
    W: Write + AsFd,
{
    pub fn new(orientation: Orientation) -> Self {
        Self {
            orientation,
            children: Vec::new(),
            event_handler: None,
            dirty: Cell::new(true),
            layouts: LayoutParams {
                width: LayoutParam::MatchParent,
                height: LayoutParam::MatchParent,
            },
            dimensions: None,
        }
    }

    pub fn push<T>(&mut self, view: T, weight: u16)
    where
        T: View<E, W> + 'static,
    {
        self.children.push(Child {
            view: Box::new(view),
            weight,
        });
        self.dirty.set(true);
    }

    pub fn with_event<F>(mut self, event_handler: F) -> Self
    where
        F: FnMut(&mut Self, &mut E) + 'static,
    {
        self.event_handler = Some(Rc::new(RefCell::new(Box::new(event_handler))));
        self
    }

    pub fn with_layouts(mut self, layouts: LayoutParams) -> Self {
        self.layouts = layouts;
        self
    }

    pub fn iter_children_mut(&mut self) -> impl Iterator<Item = &mut Box<dyn View<E, W>>> {
        self.children.iter_mut().map(|child| &mut child.view)
    }

    pub fn iter_children(&self) -> impl Iterator<Item = &Box<dyn View<E, W>>> {
        self.children.iter().map(|child| &child.view)
    }

    fn layout_vertical(&mut self, dimensions: &Dimensions) {
        /* Layout with the following steps:
         *
         *  - Compute min dimension from children
         *  - Split remaining space based on children weight
         *  - Set dimension for each children
         *
         */

        // Gather requested sizes
        let mut width = 0;
        let mut min_height = 0;
        let mut match_height_weight_total = 0;

        log::debug!("layout with vertical orientation");
        for child in self.children.iter_mut() {
            let measure_specs = MeasureSpecs::from(dimensions);
            let requested_dimensions = child.view.measure(&measure_specs);

            match requested_dimensions.height {
                RequestedDimension::ExpandMax => match_height_weight_total += child.weight,
                RequestedDimension::Absolute(requested_height) => min_height += requested_height,
            };

            match requested_dimensions.width {
                RequestedDimension::ExpandMax => width = dimensions.width,
                RequestedDimension::Absolute(requested_width) => {
                    width = std::cmp::max(width, requested_width)
                }
            };
        }

        // Compute remaining free sizes
        let free_height = dimensions.height - min_height;

        // Layout children
        let mut child_top = dimensions.top;
        let child_left = dimensions.left;

        for child in self.children.iter_mut() {
            let measure_specs = MeasureSpecs::from(dimensions);
            let requested_dimensions = child.view.measure(&measure_specs);

            let child_height = match requested_dimensions.height {
                RequestedDimension::ExpandMax => {
                    free_height * child.weight / match_height_weight_total
                }
                RequestedDimension::Absolute(requested_height) => requested_height,
            };

            let child_dimensions = Dimensions {
                top: child_top,
                left: child_left,
                width,
                height: child_height,
            };

            child.view.layout(&child_dimensions);

            child_top += child_dimensions.height;
        }

        assert!(child_top <= dimensions.top + dimensions.height);
    }

    fn layout_horizontal(&mut self, dimensions: &Dimensions) {
        // Gather requested sizes
        let mut height = 0;
        let mut min_width = 0;
        let mut match_width_weight_total = 0;

        log::debug!("layout with horizontal orientation");
        for child in self.children.iter_mut() {
            let measure_specs = MeasureSpecs::from(dimensions);
            let requested_dimensions = child.view.measure(&measure_specs);

            match requested_dimensions.height {
                RequestedDimension::ExpandMax => height = dimensions.height,
                RequestedDimension::Absolute(requested_width) => {
                    height = std::cmp::max(height, requested_width)
                }
            };

            match requested_dimensions.width {
                RequestedDimension::ExpandMax => match_width_weight_total += child.weight,
                RequestedDimension::Absolute(requested_width) => min_width += requested_width,
            };
        }

        // Compute remaining free sizes
        let free_width = dimensions.width - min_width;

        // Layout children
        let child_top = dimensions.top;
        let mut child_left = dimensions.left;

        for child in self.children.iter_mut() {
            let measure_specs = MeasureSpecs::from(dimensions);
            let requested_dimensions = child.view.measure(&measure_specs);

            let child_width = match requested_dimensions.width {
                RequestedDimension::ExpandMax => {
                    free_width * child.weight / match_width_weight_total
                }
                RequestedDimension::Absolute(requested_height) => requested_height,
            };

            let child_dimensions = Dimensions {
                top: child_top,
                left: child_left,
                height,
                width: child_width,
            };

            child.view.layout(&child_dimensions);

            child_left += child_dimensions.width;
        }

        assert!(child_left <= dimensions.left + dimensions.width);
    }
}

impl<E, W> View<E, W> for LinearLayout<E, W>
where
    W: Write + AsFd,
{
    fn measure(&self, measure_specs: &MeasureSpecs) -> RequestedDimensions {
        let children_requested_dimensions: Vec<RequestedDimensions> = self
            .iter_children()
            .map(|child| child.measure(measure_specs))
            .collect();

        let max_width = match self.orientation {
            Orientation::Horizontal => children_requested_dimensions
                .iter()
                .map(|child_requested_dimensions| child_requested_dimensions.width)
                .sum(),
            Orientation::Vertical => children_requested_dimensions
                .iter()
                .map(|child_requested_dimensions| child_requested_dimensions.width)
                .max()
                .unwrap_or(RequestedDimension::Absolute(0)),
        };

        let max_height = match self.orientation {
            Orientation::Horizontal => children_requested_dimensions
                .iter()
                .map(|child_requested_dimensions| child_requested_dimensions.height)
                .max()
                .unwrap_or(RequestedDimension::Absolute(0)),
            Orientation::Vertical => children_requested_dimensions
                .iter()
                .map(|child_requested_dimensions| child_requested_dimensions.height)
                .sum(),
        };

        RequestedDimensions {
            width: max_width,   // Let parent reconcile
            height: max_height, // Let parent reconcile
        }
    }

    fn layout(&mut self, dimensions: &Dimensions) {
        log::debug!("layout {} {:?}", std::any::type_name::<Self>(), dimensions);
        if self.dimensions.as_ref() != Some(dimensions) {
            self.dirty.set(true);
            self.dimensions.replace(dimensions.clone());
        }
        match self.orientation {
            Orientation::Horizontal => self.layout_horizontal(dimensions),
            Orientation::Vertical => self.layout_vertical(dimensions),
        }
    }

    fn render(&self, screen: &mut Screen<W>) {
        log::debug!("rendering {}", std::any::type_name::<Self>());
        let was_dirty = self.dirty.replace(false);
        if was_dirty {
            clear_screen(self.dimensions.as_ref().unwrap(), screen);
        }

        for child in self.children.iter() {
            if was_dirty || child.view.is_dirty() {
                child.view.render(screen);
            }
        }
    }

    fn set_dirty(&mut self) {
        self.dirty.set(true);
        self.children
            .iter_mut()
            .for_each(|child| child.view.set_dirty());
    }

    fn is_dirty(&self) -> bool {
        self.dirty.get() || self.children.iter().any(|child| child.view.is_dirty())
    }

    fn event(&mut self, event: &mut E) {
        if let Some(handler) = &self.event_handler {
            let handler = Rc::clone(handler);
            let handler = &mut *handler.borrow_mut();
            handler(self, event);
        } else {
            for child in self.iter_children_mut() {
                child.event(event);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use mockall::predicate::*;
    use std::fs::File;
    use test_log::test;

    use super::*;
    use crate::{MockView, RequestedDimension};

    #[test]
    fn test_vertical_layout_children_evenly() {
        // Given
        let mut layout = LinearLayout::<(), File>::new(Orientation::Vertical);

        let mut first_view = MockView::new();
        first_view
            .expect_measure()
            .return_const(RequestedDimensions {
                width: RequestedDimension::ExpandMax,
                height: RequestedDimension::ExpandMax,
            });
        let mut second_view = MockView::new();
        second_view
            .expect_measure()
            .return_const(RequestedDimensions {
                width: RequestedDimension::ExpandMax,
                height: RequestedDimension::ExpandMax,
            });

        // Then
        first_view
            .expect_layout()
            .with(eq(Dimensions {
                top: 1,
                left: 1,
                width: 100,
                height: 50,
            }))
            .times(1)
            .return_const(());
        second_view
            .expect_layout()
            .with(eq(Dimensions {
                top: 51,
                left: 1,
                width: 100,
                height: 50,
            }))
            .times(1)
            .return_const(());

        // When

        layout.push(first_view, 1);
        layout.push(second_view, 1);
        layout.layout(&Dimensions {
            top: 1,
            left: 1,
            width: 100,
            height: 100,
        });
    }

    #[test]
    fn test_vertical_layout_free_space_respecting_weight() {
        // Given
        let mut layout = LinearLayout::<(), File>::new(Orientation::Vertical);

        let mut first_view = MockView::new();
        first_view
            .expect_measure()
            .return_const(RequestedDimensions {
                width: RequestedDimension::ExpandMax,
                height: RequestedDimension::ExpandMax,
            });
        let mut second_view = MockView::new();
        second_view
            .expect_measure()
            .return_const(RequestedDimensions {
                width: RequestedDimension::ExpandMax,
                height: RequestedDimension::ExpandMax,
            });
        let mut third_view = MockView::new();
        third_view
            .expect_measure()
            .return_const(RequestedDimensions {
                width: RequestedDimension::ExpandMax,
                height: RequestedDimension::Absolute(40),
            });

        // Then
        first_view
            .expect_layout()
            .with(eq(Dimensions {
                top: 1,
                left: 1,
                width: 100,
                height: 20,
            }))
            .times(1)
            .return_const(());
        second_view
            .expect_layout()
            .with(eq(Dimensions {
                top: 21,
                left: 1,
                width: 100,
                height: 40,
            }))
            .times(1)
            .return_const(());
        third_view
            .expect_layout()
            .with(eq(Dimensions {
                top: 61,
                left: 1,
                width: 100,
                height: 40,
            }))
            .times(1)
            .return_const(());

        // When

        layout.push(first_view, 1);
        layout.push(second_view, 2);
        layout.push(third_view, 1);
        layout.layout(&Dimensions {
            top: 1,
            left: 1,
            width: 100,
            height: 100,
        });
    }

    #[test]
    fn test_horizontal_layout_children_evenly() {
        // Given
        let mut layout = LinearLayout::<(), File>::new(Orientation::Horizontal);

        let mut first_view = MockView::new();
        first_view
            .expect_measure()
            .return_const(RequestedDimensions {
                width: RequestedDimension::ExpandMax,
                height: RequestedDimension::ExpandMax,
            });
        let mut second_view = MockView::new();
        second_view
            .expect_measure()
            .return_const(RequestedDimensions {
                width: RequestedDimension::ExpandMax,
                height: RequestedDimension::ExpandMax,
            });

        // Then
        first_view
            .expect_layout()
            .with(eq(Dimensions {
                top: 1,
                left: 1,
                height: 100,
                width: 50,
            }))
            .times(1)
            .return_const(());
        second_view
            .expect_layout()
            .with(eq(Dimensions {
                top: 1,
                left: 51,
                height: 100,
                width: 50,
            }))
            .times(1)
            .return_const(());

        // When

        layout.push(first_view, 1);
        layout.push(second_view, 1);
        layout.layout(&Dimensions {
            top: 1,
            left: 1,
            width: 100,
            height: 100,
        });
    }

    #[test]
    fn test_horizontal_layout_free_space_respecting_weight() {
        // Given
        let mut layout = LinearLayout::<(), File>::new(Orientation::Horizontal);

        let mut first_view = MockView::new();
        first_view
            .expect_measure()
            .return_const(RequestedDimensions {
                width: RequestedDimension::ExpandMax,
                height: RequestedDimension::ExpandMax,
            });
        let mut second_view = MockView::new();
        second_view
            .expect_measure()
            .return_const(RequestedDimensions {
                width: RequestedDimension::ExpandMax,
                height: RequestedDimension::ExpandMax,
            });
        let mut third_view = MockView::new();
        third_view
            .expect_measure()
            .return_const(RequestedDimensions {
                width: RequestedDimension::Absolute(40),
                height: RequestedDimension::ExpandMax,
            });

        // Then
        first_view
            .expect_layout()
            .with(eq(Dimensions {
                top: 1,
                left: 1,
                width: 20,
                height: 200,
            }))
            .times(1)
            .return_const(());
        second_view
            .expect_layout()
            .with(eq(Dimensions {
                top: 1,
                left: 21,
                width: 40,
                height: 200,
            }))
            .times(1)
            .return_const(());
        third_view
            .expect_layout()
            .with(eq(Dimensions {
                top: 1,
                left: 61,
                width: 40,
                height: 200,
            }))
            .times(1)
            .return_const(());

        // When

        layout.push(first_view, 1);
        layout.push(second_view, 2);
        layout.push(third_view, 1);
        layout.layout(&Dimensions {
            top: 1,
            left: 1,
            width: 100,
            height: 200,
        });
    }

    #[test]
    fn test_set_dirty_to_children() {
        // Given
        let mut layout = LinearLayout::<(), File>::new(Orientation::Horizontal);

        // Then
        let mut first_view = MockView::new();
        first_view.expect_set_dirty().times(1).return_const(());
        let mut second_view = MockView::new();
        second_view.expect_set_dirty().times(1).return_const(());

        // When
        layout.push(first_view, 1);
        layout.push(second_view, 2);

        layout.set_dirty();
    }
}
