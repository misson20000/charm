use std::rc;

use crate::view::helpers;
use crate::view::window;

use gtk::prelude::*;

struct ReopenCurrentProjectAction {
    window: rc::Weak<window::CharmWindow>,
}

pub fn add_action(window: &rc::Rc<window::CharmWindow>) {
    let action_impl = rc::Rc::new(ReopenCurrentProjectAction {
        window: rc::Rc::downgrade(window),
    });
    
    window.window.add_action(&helpers::create_simple_action_strong(action_impl, "debug.reopen_current_project", |action| action.activate()));
}

impl ReopenCurrentProjectAction {    
    fn activate(&self) {
        let Some(window) = self.window.upgrade() else { return };

        let project = match window.context().as_ref() {
            Some(c) => c.project.revert_from(&*c.project.document_host.borrow(), 0),
            None => return,
        };
        
        window.open_project(project, true, true);
    }
}
