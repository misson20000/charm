//pub mod scroll;
pub mod cursor;

use std::task;

pub trait Facet {
    fn wants_draw(&mut self) -> &mut Event;
    fn wants_work(&mut self) -> &mut Event;
    fn work(&mut self, cx: &mut task::Context);
}

#[derive(Debug)]
pub struct Event {
    wanted: bool,
}

impl Event {
    pub fn new() -> Event {
        Event {
            wanted: false,
        }
    }

    pub fn want(&mut self) {
        self.wanted = true;
    }

    pub fn collect(&mut self) -> bool {
        std::mem::replace(&mut self.wanted, false)
    }
}
