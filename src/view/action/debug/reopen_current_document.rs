use std::rc;
use std::sync;

use gtk::gio;

use crate::model::document;
use crate::view::helpers;
use crate::view::window;

struct ReopenCurrentDocumentAction {
    document_host: sync::Arc<document::DocumentHost>,
    window: rc::Weak<window::CharmWindow>,
}

pub fn create_action(window_context: &window::WindowContext) -> gio::SimpleAction {
    let action_impl = rc::Rc::new(ReopenCurrentDocumentAction {
        document_host: window_context.document_host.clone(),
        window: window_context.window.clone(),
    });
    
    let action = helpers::create_simple_action_strong(action_impl, "debug.reopen_current_document", |action| action.activate());

    action
}

impl ReopenCurrentDocumentAction {    
    fn activate(&self) {
        if let Some(window) = self.window.upgrade() {
            window.attach_context(Some(window::WindowContext::new(&window, (**self.document_host.borrow()).clone(), None)));
        }
    }
}
