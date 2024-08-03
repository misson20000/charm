use gtk::prelude::*;

use std::rc;

use crate::view::helpers;
use crate::view::window;

struct OpenSettingsAction {
    window: rc::Weak<window::CharmWindow>,
    dialog: gtk::Window,
}

pub fn add_action(window: &rc::Rc<window::CharmWindow>) {
    let action = rc::Rc::new(OpenSettingsAction {
        window: rc::Rc::downgrade(window),
        dialog: window.application.settings_dialog.clone(),
    });
    
    window.window.add_action(&helpers::create_simple_action_strong(action.clone(), "settings", |action| {
        if let Some(window) = action.window.upgrade() {
            action.dialog.set_transient_for(Some(&window.window));
            action.dialog.present();
        }
    }));
}
