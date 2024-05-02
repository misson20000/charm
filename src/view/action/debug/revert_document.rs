use std::rc;
use std::sync;

use gtk::gio;
use gtk::glib;

use crate::model::document;
use crate::model::versioned::Versioned;
use crate::view::window;

struct RevertDocumentAction {
    document_host: sync::Arc<document::DocumentHost>,
    window: rc::Weak<window::CharmWindow>,
}

pub fn create_action(window_context: &window::WindowContext) -> gio::SimpleAction {
    let action_impl = rc::Rc::new(RevertDocumentAction {
        document_host: window_context.document_host.clone(),
        window: window_context.window.clone(),
    });

    let action = gio::SimpleAction::new("debug.revert_document", Some(glib::VariantTy::UINT32));
    action.connect_activate(move |_, parameter| {
        /* FFI CALLBACK */
        if let Some(amount) = parameter.and_then(|parameter| parameter.get::<u32>()) {
            action_impl.activate(amount);
        }
    });
    action.set_enabled(true);

    action
}

impl RevertDocumentAction {    
    fn activate(&self, amount: u32) {
        if let Some(window) = self.window.upgrade() {
            let current = self.document_host.borrow();
            let mut document = &*current;

            for _ in 0..amount {
                document = match document.previous() {
                    Some((doc, _)) => doc,
                    None => return,
                }
            }
            
            window.attach_context(Some(window::WindowContext::new(&window, (**document).clone(), None)));
        }
    }
}
