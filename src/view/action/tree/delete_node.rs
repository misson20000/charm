use std::cell;
use std::rc;
use std::sync;

use gtk::prelude::*;
use gtk::glib;
use gtk::glib::clone;
use gtk::gio;

use crate::model::document;
use crate::model::selection;
use crate::view::error;
use crate::view::helpers;
use crate::view::window;
use crate::view::window::ErrorReporter;

struct DeleteNodeAction {
    document_host: sync::Arc<document::DocumentHost>,
    selection_host: sync::Arc<selection::tree::Host>,
    selection: cell::RefCell<sync::Arc<selection::TreeSelection>>,
    window: rc::Weak<window::CharmWindow>,
    
    subscriber: once_cell::unsync::OnceCell<helpers::AsyncSubscriber>,
}

pub fn add_action(window_context: &window::WindowContext) {
    let selection = window_context.tree_selection_host.get();
    
    let action_impl = rc::Rc::new(DeleteNodeAction {
        document_host: window_context.project.document_host.clone(),
        selection_host: window_context.tree_selection_host.clone(),
        selection: cell::RefCell::new(selection.clone()),
        window: window_context.window.clone(),
        subscriber: Default::default(),
    });
    
    let action = helpers::create_simple_action_strong(action_impl.clone(), "delete_node", |action| action.activate());
    action.set_enabled(action_impl.enabled());
    
    action_impl.subscriber.set(
        helpers::subscribe_to_updates(
            rc::Rc::downgrade(&action_impl),
            action_impl.selection_host.clone(),
            selection,
            clone!(#[weak] action, move |action_impl, selection| {
                *action_impl.selection.borrow_mut() = selection.clone();
                action.set_enabled(action_impl.enabled());
            })
        )
    ).unwrap();
    
    window_context.action_group.add_action(&action);
}

fn update_enabled(action: &gio::SimpleAction, selection: &selection::TreeSelection) {
    action.set_enabled(selection.any_selected() && !selection.root_selected());
}

impl DeleteNodeAction {
    fn enabled(&self) -> bool {
        let sel = self.selection.borrow();
        sel.any_selected() && !sel.root_selected()
    }
    
    fn activate(&self) {
        if let Some(window) = self.window.upgrade() {
            let selection = self.selection.borrow();

            if let Err((error, attempted_version)) = selection.ancestor_ranges_selected(|range| {
                self.document_host.change(selection.document.delete_range(range.clone())).map(|_| ())
            }) {
                /* Inform the user that their action failed. */
                window.report_error(error::Error {
                    while_attempting: error::Action::DeleteNode,
                    trouble: error::Trouble::DocumentUpdateFailure {
                        error,
                        attempted_version
                    },
                    level: error::Level::Error,
                    is_bug: false,
                });
            }
        }
    }
}
