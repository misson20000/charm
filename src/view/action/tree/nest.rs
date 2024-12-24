use std::cell;
use std::rc;
use std::sync;

use gtk::prelude::*;
use gtk::glib;
use gtk::glib::clone;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::selection;
use crate::model::versioned::Versioned;
use crate::view::error;
use crate::view::helpers;
use crate::view::window;
use crate::view::window::ErrorReporter;

struct NestAction {
    document_host: sync::Arc<document::DocumentHost>,
    selection_host: sync::Arc<selection::tree::Host>,
    selection: cell::RefCell<(sync::Arc<selection::TreeSelection>, Option<structure::SiblingRange>)>,
    window: rc::Weak<window::CharmWindow>,
    
    subscriber: once_cell::unsync::OnceCell<helpers::AsyncSubscriber>,
}

pub fn add_action(window_context: &window::WindowContext) {
    let selection = window_context.tree_selection_host.get();
    
    let action_impl = rc::Rc::new(NestAction {
        document_host: window_context.project.document_host.clone(),
        selection_host: window_context.tree_selection_host.clone(),
        selection: cell::RefCell::new((selection.clone(), selection.one_range_selected())),
        window: window_context.window.clone(),
        subscriber: Default::default(),
    });
    
    let action = helpers::create_simple_action_strong(action_impl.clone(), "nest", |action| action.activate());
    action.set_enabled(action_impl.enabled());
    
    action_impl.subscriber.set(
        helpers::subscribe_to_updates(
            rc::Rc::downgrade(&action_impl),
            action_impl.selection_host.clone(),
            selection,
            clone!(#[weak] action, move |action_impl, selection| {
                *action_impl.selection.borrow_mut() = (selection.clone(), selection.one_range_selected());
                action.set_enabled(action_impl.enabled());
            })
        )
    ).unwrap();
    
    window_context.action_group.add_action(&action);
}

impl NestAction {    
    fn enabled(&self) -> bool {
        self.selection.borrow().1.is_some()
    }

    fn activate(&self) {
        if let Some(window) = self.window.upgrade() {
            if let (selection, Some(range)) = &*self.selection.borrow() {
                let parent_node = selection.document.lookup_node(&range.parent).0;
                let extent = addr::Extent::between(parent_node.children[range.first].offset, parent_node.children[range.last].end());
                
                let new_doc = match self.document_host.change(selection.document.nest(range.clone(), extent, parent_node.props.clone_rename("".to_string()))) {
                    Ok(new_doc) => new_doc,
                    Err((error, attempted_version)) => {
                        /* Inform the user that their action failed. */
                        window.report_error(error::Error {
                            while_attempting: error::Action::Nest,
                            trouble: error::Trouble::DocumentUpdateFailure {
                                error,
                                attempted_version
                            },
                            level: error::Level::Error,
                            is_bug: false,
                        });
                        
                        return;
                    }
                };

                /* We need to dig the information about where the nested node wound up out of the change record since it's
                 * possible we had an outdated copy of the document when we submitted the change. */
                let record = &new_doc.previous().expect("just-changed document should have a previous document and change").1;
                let nested_node_path = match record {
                    document::change::ApplyRecord::Nest { range, .. } => {
                        let mut path = range.parent.clone();
                        path.push(range.first);
                        path
                    },
                    _ => panic!("change was transmuted into a different type?")
                };
                
                let new_sel = match self.selection_host.change(selection::tree::Change::SetSingle(new_doc, nested_node_path)) {
                    Ok(new_sel) => new_sel,
                    Err((error, attempted_version)) => {
                        /* This shouldn't happen. */
                        window.report_error(error::Error {
                            while_attempting: error::Action::Nest,
                            trouble: error::Trouble::TreeSelectionUpdateFailure {
                                error,
                                attempted_version
                            },
                            level: error::Level::Informational,
                            is_bug: true,
                        });
                        
                        return;
                    }
                };

                if let Some(w) = self.window.upgrade() {
                    /* Force the properties editor to update to the new selection synchronously */
                    w.props_editor.update_selection(&new_sel);
                    w.props_editor.focus_name();
                }
            }
        }
    }
}
