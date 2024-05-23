use std::rc;
use std::sync;

use gtk::gio;
use gtk::glib;

use crate::catch_panic;
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
    action.connect_activate(move |_, parameter| catch_panic! {
        if let Some(amount) = parameter.and_then(|parameter| parameter.get::<u32>()) {
            action_impl.activate(amount);
        }
    });
    action.set_enabled(true);

    action
}

impl RevertDocumentAction {    
    fn activate(&self, amount: u32) {
        let Some(window) = self.window.upgrade() else { return };
        let Some(context) = &*window.context() else { return };
        
        let current = context.document_host.borrow();
        let mut document = &*current;

        for _ in 0..amount {
            document = match document.previous() {
                Some((doc, _)) => doc,
                None => return,
            }
        }
        
        window.set_context(Some(window::WindowContext::new(&window, (**document).clone(), context.project_file.borrow().clone())));
    }
}
