use std::sync;
use std::task;

use crate::model::document;
use crate::util;

use gtk::prelude::*;

pub mod scroll;
pub mod cursor;

pub trait Facet {
    fn wants_draw(&mut self) -> &mut Event;
    fn wants_work(&mut self) -> &mut Event;
    fn work(&mut self, _document: &sync::Arc<document::Document>, _cx: &mut task::Context) {
    }

    fn collect_events(&mut self, widget: &super::ListingWidget, work_notifier: &util::Notifier) {
        if self.wants_draw().collect() {
            widget.queue_draw();
        }

        if self.wants_work().collect() {
            work_notifier.notify();
        }
    }
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

    pub fn new_wanted() -> Event {
        Event {
            wanted: true,
        }
    }

    pub fn want(&mut self) {
        self.wanted = true;
    }

    pub fn collect(&mut self) -> bool {
        std::mem::replace(&mut self.wanted, false)
    }
}
