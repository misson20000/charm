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
    selection_host: sync::Arc<selection::hierarchy::Host>,
    selection: cell::RefCell<sync::Arc<selection::HierarchySelection>>,
    
    subscriber: once_cell::unsync::OnceCell<helpers::AsyncSubscriber>,
}

pub fn create_action(window_context: &window::WindowContext) -> gio::SimpleAction {
    let selection = window_context.hierarchy_selection_host.get();
    
    let action_impl = rc::Rc::new(DeleteNodeAction {
        document_host: window_context.document_host.clone(),
        selection_host: window_context.hierarchy_selection_host.clone(),
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

fn update_enabled(action: &gio::SimpleAction, selection: &selection::HierarchySelection) {
    action.set_enabled(match &selection.mode {
        selection::hierarchy::Mode::Empty => false,
        selection::hierarchy::Mode::Single(path) => !path.is_empty(),
        selection::hierarchy::Mode::SiblingRange(_, _, _) => true,
        selection::hierarchy::Mode::All => false,
    });
}

impl DeleteNodeAction {    
    fn activate(&self) {
        let selection = self.selection.borrow();

        let (parent, first_sibling, last_sibling) = match &selection.mode {
            selection::hierarchy::Mode::Empty => return,
            selection::hierarchy::Mode::Single(path) if !path.is_empty() => (&path[0..path.len()-1], *path.last().unwrap(), *path.last().unwrap()),
            selection::hierarchy::Mode::SiblingRange(path, begin, end) => (&path[..], *begin, *end),
            selection::hierarchy::Mode::All | selection::hierarchy::Mode::Single(_) => {
                // TODO: find a way to issue a warning for this
                return;
            }
        };

        // TODO: failure feedback
        let _ = self.document_host.change(selection.document.delete_range(parent.to_vec(), first_sibling, last_sibling));
    }
}
