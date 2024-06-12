use std::cell;
use std::rc;
use std::sync;

use crate::catch_panic;
use crate::model::document;
use crate::model::document::structure;
use crate::model::selection;
use crate::view::error;
use crate::view::listing::pick;
use crate::view::window;

use gtk::prelude::*;
use gtk::gio;
use gtk::glib::clone;

pub struct ResizeNodeAction {
    document_host: sync::Arc<document::DocumentHost>,
    selection_host: sync::Arc<selection::listing::Host>,
    path: cell::RefCell<Option<(sync::Arc<document::Document>, structure::Path)>>,
    pub action: gio::SimpleAction,
}

struct Activation {
    document: sync::Arc<document::Document>,
}

impl ResizeNodeAction {
    pub fn new(window: &rc::Rc<window::CharmWindow>, document_host: sync::Arc<document::DocumentHost>, selection_host: sync::Arc<selection::listing::Host>) -> rc::Rc<Self> {
        let action = gio::SimpleAction::new("resize_node", None);

        let action = rc::Rc::new(Self {
            document_host,
            selection_host,
            path: cell::RefCell::new(None),
            action,
        });

        action.action.connect_activate(clone!(@weak window, @strong action => move |_, _| catch_panic! {
            action.activate(&window);
        }));

        action
    }

    pub fn pick_updated(&self, new_pick: &Option<(&sync::Arc<document::Document>, structure::Path, pick::Part)>) {
        *self.path.borrow_mut() = new_pick.as_ref().map(|(doc, path, _part)| ((*doc).clone(), path.clone()));
        
        self.update_is_enabled();
    }

    fn update_is_enabled(&self) {
        self.action.set_enabled(self.path.borrow().is_some());
    }

    fn change(&self) -> Result<document::change::Change, error::Error> {
        todo!();
    }
    
    fn do_resize(&self) -> Result<(), error::Error> {
        self.document_host.change(self.change()?).map_err(|(error, attempted_version)| error::Error {
            while_attempting: error::Action::ResizeNode,
            trouble: error::Trouble::DocumentUpdateFailure {
                error,
                attempted_version
            },
            level: error::Level::Error,
            is_bug: false,
        }).map(|_| {})
    }

    fn activate(&self, window: &window::CharmWindow) {
        
    }
}
