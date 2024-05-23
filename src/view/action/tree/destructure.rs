use std::cell;
use std::rc;
use std::sync;

use gtk::glib;
use gtk::glib::clone;
use gtk::gio;

use crate::model::document;
use crate::model::document::structure;
use crate::model::selection;
use crate::view::error;
use crate::view::helpers;
use crate::view::window;

struct DestructureAction {
    document_host: sync::Arc<document::DocumentHost>,
    selection_host: sync::Arc<selection::tree::Host>,
    path: cell::RefCell<(sync::Arc<selection::TreeSelection>, Option<structure::Path>)>,
    window: rc::Weak<window::CharmWindow>,
    
    subscriber: once_cell::unsync::OnceCell<helpers::AsyncSubscriber>,
}

pub fn create_action(window_context: &window::WindowContext) -> gio::SimpleAction {
    let selection = window_context.tree_selection_host.get();
    
    let action_impl = rc::Rc::new(DestructureAction {
        document_host: window_context.project.document_host.clone(),
        selection_host: window_context.tree_selection_host.clone(),
        path: cell::RefCell::new((selection.clone(), selection.single_selected())),
        window: window_context.window.clone(),
        subscriber: Default::default(),
    });
    
    let action = helpers::create_simple_action_strong(action_impl.clone(), "destructure", |action| action.activate());
    action.set_enabled(action_impl.enabled());
    
    action_impl.subscriber.set(helpers::subscribe_to_updates(rc::Rc::downgrade(&action_impl), action_impl.selection_host.clone(), selection, clone!(@weak action => move |action_impl, selection| {
        action_impl.update_path(selection.clone());
        action.set_enabled(action_impl.enabled());
    }))).unwrap();
    
    action
}

impl DestructureAction {
    fn enabled(&self) -> bool {
        self.path.borrow().1.as_ref().map_or(false, |path| path.len() > 0)
    }
    
    fn update_path(&self, selection: sync::Arc<selection::TreeSelection>) {
        let single = selection.single_selected();
        *self.path.borrow_mut() = (selection, single);
    }
    
    fn activate(&self) {
        if let Some(window) = self.window.upgrade() {
            if let (selection, Some(path)) = &*self.path.borrow() {
                if let Err((error, attempted_version)) = self.document_host.change(selection.document.destructure(path).unwrap()) {
                    /* Inform the user that their action failed. */
                    window.report_error(error::Error {
                        while_attempting: error::Action::DestructureNode,
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
}
