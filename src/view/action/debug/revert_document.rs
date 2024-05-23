use std::rc;

use gtk::gio;
use gtk::glib;

use crate::catch_panic;
use crate::view::project;
use crate::view::window;

struct RevertDocumentAction {
    project: rc::Rc<project::Project>,
    window: rc::Weak<window::CharmWindow>,
}

pub fn create_action(window_context: &window::WindowContext) -> gio::SimpleAction {
    let action_impl = rc::Rc::new(RevertDocumentAction {
        project: window_context.project.clone(),
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

        let project = match window.context().as_ref() {
            Some(c) => c.project.revert_from(&*c.project.document_host.borrow(), amount),
            None => return,
        };
        
        window.open_project(project, true);
    }
}
