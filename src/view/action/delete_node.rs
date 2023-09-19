use std::cell;
use std::rc;
use std::sync;

use gtk::glib;
use gtk::glib::clone;
use gtk::gio;

use crate::model::document;
use crate::model::selection;
use crate::view::helpers;
use crate::view::window;

struct DeleteNodeAction {
    document_host: sync::Arc<document::DocumentHost>,
    selection_host: sync::Arc<selection::tree::Host>,
    selection: cell::RefCell<sync::Arc<selection::TreeSelection>>,
    
    subscriber: once_cell::unsync::OnceCell<helpers::AsyncSubscriber>,
}

pub fn create_action(window_context: &window::WindowContext) -> gio::SimpleAction {
    let selection = window_context.tree_selection_host.get();
    
    let action_impl = rc::Rc::new(DeleteNodeAction {
        document_host: window_context.document_host.clone(),
        selection_host: window_context.tree_selection_host.clone(),
        selection: cell::RefCell::new(selection.clone()),
        subscriber: Default::default(),
    });
    
    let action = helpers::create_simple_action_strong(action_impl.clone(), "delete_node", |action| action.activate());

    update_enabled(&action, &selection);
    
    action_impl.subscriber.set(helpers::subscribe_to_updates(rc::Rc::downgrade(&action_impl), action_impl.selection_host.clone(), selection, clone!(@weak action => move |action_impl, selection| {
        *action_impl.selection.borrow_mut() = selection.clone();
        update_enabled(&action, selection);
    }))).unwrap();
    
    action
}

fn update_enabled(action: &gio::SimpleAction, selection: &selection::TreeSelection) {
    action.set_enabled(selection.any_selected() && !selection.root_selected());
}

impl DeleteNodeAction {    
    fn activate(&self) {
        //let selection = self.selection.borrow();

        todo!();
        
        // TODO: failure feedback
        //let _ = self.document_host.change(selection.document.delete_range(parent.to_vec(), first_sibling, last_sibling));
    }
}
