use std::cell;
use std::rc;
use std::sync;

use gtk::prelude::*;
use gtk::glib;
use gtk::glib::clone;

use crate::model::document;
use crate::model::selection;
use crate::view::error;
use crate::view::helpers;
use crate::view::window;
use crate::view::window::ErrorReporter;

struct SelectAction {
    document_host: sync::Arc<document::DocumentHost>,
    selection_host: sync::Arc<selection::tree::Host>,
    selection: cell::RefCell<sync::Arc<selection::TreeSelection>>,
    window: rc::Weak<window::CharmWindow>,
    
    subscriber: once_cell::unsync::OnceCell<helpers::AsyncSubscriber>,
}

pub fn add_actions(window_context: &window::WindowContext) {
    let selection = window_context.tree_selection_host.get();
    
    let action_impl = rc::Rc::new(SelectAction {
        document_host: window_context.project.document_host.clone(),
        selection_host: window_context.tree_selection_host.clone(),
        selection: cell::RefCell::new(selection.clone()),
        window: window_context.window.clone(),
        subscriber: Default::default(),
    });
    
    let select_descendants_action = helpers::create_simple_action_strong(action_impl.clone(), "select_descendants", |action| action.activate_select_descendants());
    select_descendants_action.set_enabled(action_impl.select_descendants_enabled());
    
    action_impl.subscriber.set(
        helpers::subscribe_to_updates(
            rc::Rc::downgrade(&action_impl),
            action_impl.selection_host.clone(),
            selection,
            clone!(#[weak] select_descendants_action, move |action_impl, selection| {
                *action_impl.selection.borrow_mut() = selection.clone();
                select_descendants_action.set_enabled(action_impl.select_descendants_enabled());
            })
        )
    ).unwrap();
    
    window_context.action_group.add_action(&select_descendants_action);

    window_context.action_group.add_action(&helpers::create_simple_action_strong(action_impl.clone(), "select_all", |action| action.activate_select_all()));
    window_context.action_group.add_action(&helpers::create_simple_action_strong(action_impl.clone(), "select_none", |action| action.activate_select_none()));
}

impl SelectAction {
    fn select_descendants_enabled(&self) -> bool {
        let sel = self.selection.borrow();
        sel.any_selected()
    }
    
    fn activate_select_descendants(&self) {
        self.attempt_change(error::Action::SelectDescendants, selection::tree::Change::SelectDescendants);
    }

    fn activate_select_all(&self) {
        self.attempt_change(error::Action::SelectAll, selection::tree::Change::SelectAll);
    }

    fn activate_select_none(&self) {
        self.attempt_change(error::Action::SelectNone, selection::tree::Change::Clear);
    }

    fn attempt_change(&self, action: error::Action, change: selection::tree::Change) {
        if let Some(window) = self.window.upgrade() {
            if let Err((error, attempted_version)) = self.selection_host.change(change) {
                /* Inform the user that their action failed. */
                window.report_error(error::Error {
                    while_attempting: action,
                    trouble: error::Trouble::TreeSelectionUpdateFailure {
                        error,
                        attempted_version
                    },
                    level: error::Level::Error,
                    is_bug: true,
                });
            }
        }
    }
}
