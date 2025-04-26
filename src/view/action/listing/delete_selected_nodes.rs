use std::cell;
use std::rc;
use std::sync;

use crate::catch_panic;
use crate::model::document;
use crate::model::selection;
use crate::view::error;
use crate::view::helpers;
use crate::view::window;
use crate::view::window::ErrorReporter;

use gtk::prelude::*;
use gtk::gio;

struct DeleteSelectedNodesAction {
    document_host: sync::Arc<document::DocumentHost>,
    selection_host: sync::Arc<selection::listing::Host>,
    selection: cell::RefCell<sync::Arc<selection::ListingSelection>>,
    subscriber: helpers::AsyncSubscriber,
    action: glib::WeakRef<gio::SimpleAction>,
}

pub fn add_action(window_context: &window::WindowContext) {
    let gio_action = gio::SimpleAction::new("delete_selected_nodes", None);
    let action_impl = DeleteSelectedNodesAction::new(window_context, gio_action.clone());
    let window = window_context.window.clone();
    action_impl.update_is_enabled();
    
    gio_action.connect_activate(move |_, _| catch_panic! {
        let Some(window) = window.upgrade() else { return };
        
        if let Err(e) = action_impl.activate() {
            window.report_error(e);
        }
    });
    
    window_context.action_group.add_action(&gio_action);
}

impl DeleteSelectedNodesAction {
    fn new(window_context: &window::WindowContext, action: gio::SimpleAction) -> rc::Rc<Self> {
        let document_host = window_context.project.document_host.clone();
        let selection_host = window_context.listing_selection_host.clone();
        let selection = selection_host.get();

        rc::Rc::new_cyclic(|weak: &rc::Weak<Self>| Self {
            subscriber: helpers::subscribe_to_updates(weak.clone(), selection_host.clone(), selection.clone(), |action, new_sel| {
                action.selection_updated(new_sel);
            }),

            document_host,
            selection_host,
            selection: cell::RefCell::new(selection),
            action: action.downgrade(),
        })
    }

    fn selection_updated(&self, new_sel: &sync::Arc<selection::ListingSelection>) {
        *self.selection.borrow_mut() = new_sel.clone();
        self.update_is_enabled();
    }

    fn update_is_enabled(&self) {
        if let Some(action) = self.action.upgrade() {
            action.set_enabled(match &self.selection.borrow().mode {
                selection::listing::Mode::Structure(selection::listing::StructureMode::Range(range)) => {
                    /* Only enable the action if there are actually any nodes selected */
                    range.begin.1 != range.end.1
                },

                _ => false
            });
        }
    }

    fn change(&self) -> Result<document::change::Change, error::Error> {
        let selection = self.selection.borrow();

        let (range, _extent) = match &selection.mode {
            selection::listing::Mode::Structure(selection::listing::StructureMode::Range(range)) => range.clone().to_sibling_range_and_extent(),
            
            _ => return Err(error::Error {
                while_attempting: error::Action::DeleteNodesInListing,
                trouble: error::Trouble::UnsupportedListingSelectionMode {
                    selection: selection.clone()
                },
                level: error::Level::Error,
                is_bug: false,
            }),
        }.map_err(|_| error::Error {
            while_attempting: error::Action::DeleteNodesInListing,
            trouble: error::Trouble::NoNodesInSelection,
            level: error::Level::Error,
            is_bug: false,
        })?;
        
        Ok(selection.document.delete_range(range))
    }
    
    fn activate(&self) -> Result<(), error::Error> {
        self.document_host.change(self.change()?).map_err(|(error, attempted_version)| error::Error {
            while_attempting: error::Action::DeleteNodesInListing,
            trouble: error::Trouble::DocumentUpdateFailure {
                error,
                attempted_version
            },
            level: error::Level::Error,
            is_bug: false,
        }).map(|_| {})
    }
}
