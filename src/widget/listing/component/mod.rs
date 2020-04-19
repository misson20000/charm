pub mod scroll;

use gtk::WidgetExt;

#[derive(Debug)]
pub struct Events {
    wants_draw: bool,
    wants_animate: bool,
}

impl Events {
    pub fn new() -> Events {
        Events {
            wants_draw: false,
            wants_animate: false,
        }
    }

    pub fn want_draw(&mut self) {
        self.wants_draw = true;
    }

    pub fn want_animate(&mut self) {
        self.wants_animate = true;
    }

    pub fn want(&mut self) {
        self.wants_draw = true;
        self.wants_animate = true;
    }
    
    pub fn collect_draw(&mut self, da: &gtk::DrawingArea) {
        // TODO: wants_animate
        if std::mem::replace(&mut self.wants_draw, false) {
            da.queue_draw();
        }
    }
}
