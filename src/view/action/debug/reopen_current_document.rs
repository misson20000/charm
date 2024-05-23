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
        let Some(window) = self.window.upgrade() else { return };

        let context = match window.context().as_ref() {
            Some(c) => c.recreate(&window),
            None => return,
        };
        
        window.set_context(Some(context));
    }
}
