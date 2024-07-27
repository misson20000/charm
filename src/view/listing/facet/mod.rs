use std::sync;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::task;

use crate::model::document;
use crate::util;

use gtk::subclass::prelude::*;
use gtk::prelude::*;

pub mod scroll;
pub mod cursor;

pub trait Facet {
    fn wants_draw(&self) -> &Event {
        &UNUSED_EVENT
    }
    
    fn wants_work(&self) -> &Event  {
        &UNUSED_EVENT
    }
    
    fn wants_repick(&self) -> &Event {
        &UNUSED_EVENT
    }

    /// Returns true if more work is necessary
    fn work(&mut self, _document: &sync::Arc<document::Document>, _cx: &mut task::Context) -> bool {
        false
    }

    fn collect_events(&mut self, widget: &super::ListingWidget, work_notifier: &util::Notifier, work_incomplete: &mut bool) {
        if self.wants_draw().collect() {
            widget.queue_draw();
        }

        if self.wants_work().collect() {
            *work_incomplete = true;
            work_notifier.notify();
        }

        if self.wants_repick().collect() {
            widget.imp().should_repick.store(true, Ordering::Relaxed);
        }
    }
}

#[derive(Debug)]
pub struct Event {
    wanted: AtomicBool,
}

impl Event {
    pub fn new() -> Event {
        Event {
            wanted: AtomicBool::new(false),
        }
    }

    pub fn new_wanted() -> Event {
        Event {
            wanted: AtomicBool::new(true),
        }
    }

    pub fn want(&mut self) {
        self.wanted.store(true, Ordering::Relaxed);
    }

    pub fn collect(&self) -> bool {
        self.wanted.swap(false, Ordering::Relaxed)
    }
}

static UNUSED_EVENT: Event = Event {
    wanted: AtomicBool::new(false),
};
