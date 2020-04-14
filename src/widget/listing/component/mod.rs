use std::cell;

pub mod cursor;
pub mod scroll;

use gtk::WidgetExt;

#[derive(Debug)]
struct Interior {
    wants_draw: bool,
    wants_animate: bool,
}

#[derive(Debug)]
pub struct Events {
    cell: cell::RefCell<Interior>
}

impl Events {
    pub fn new() -> Events {
        Events {
            cell: cell::RefCell::new(Interior {
                wants_draw: false,
                wants_animate: false,
            }),
        }
    }

    pub fn want_draw(&self) {
        self.cell.borrow_mut().wants_draw = true;
    }

    pub fn want_animate(&self) {
        self.cell.borrow_mut().wants_animate = true;
    }

    pub fn want(&self) {
        let mut c = self.cell.borrow_mut();
        c.wants_draw = true;
        c.wants_animate = true;
    }
    
    pub fn collect_draw(&self, da: &gtk::DrawingArea) {
        let mut c = self.cell.borrow_mut();
        
        // TODO: wants_animate
        if std::mem::replace(&mut c.wants_draw, false) {
            da.queue_draw();
        }
    }
}
