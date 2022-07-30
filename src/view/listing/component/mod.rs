pub mod scroll;
pub mod cursor;

use gtk::prelude::WidgetExt;

#[derive(Debug)]
pub struct Events {
    wants_draw: bool,
    wants_animate: bool,
    wants_update: bool,
}

impl Events {
    pub fn new() -> Events {
        Events {
            wants_draw: false,
            wants_animate: false,
            wants_update: false,
        }
    }

    pub fn want_draw(&mut self) {
        self.wants_draw = true;
    }

    pub fn want_animate(&mut self) {
        self.wants_animate = true;
    }

    pub fn want_update(&mut self) {
        self.wants_update = true;
    }

    pub fn collect_draw(&mut self, da: &gtk::DrawingArea) {
        // TODO: wants_animate
        if std::mem::replace(&mut self.wants_draw, false) {
            da.queue_draw();
        }
    }

    pub fn collect_update(&mut self) -> bool {
        std::mem::replace(&mut self.wants_update, false)
    }
}
