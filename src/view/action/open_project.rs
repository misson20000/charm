use gtk::prelude::*;
use gtk::glib;
use gtk::glib::clone;
use gtk::gio;

use std::rc;

use crate::catch_panic;
use crate::view::helpers;
use crate::view::window;

struct OpenProjectAction {
    window: rc::Weak<window::CharmWindow>,
    dialog: gtk::FileChooserNative,
}

pub fn create_action(window: &rc::Rc<window::CharmWindow>) -> gio::SimpleAction {
    let filter = gtk::FileFilter::new();
    filter.set_name(Some("Charm Projects"));
    filter.add_pattern("*.charm");
    
    let dialog = gtk::FileChooserNative::builder()
        .accept_label("Open")
        .cancel_label("Cancel")
        .title("Charm: Open Project")
        .modal(true)
        .transient_for(&window.window)
        .action(gtk::FileChooserAction::Open)
        .select_multiple(false)
        .filter(&filter)
        .build();
    
    let action = rc::Rc::new(OpenProjectAction {
        window: rc::Rc::downgrade(window),
        dialog: dialog
    });
    
    action.dialog.connect_response(clone!(@weak action => move |_dialog, response_type| catch_panic! {
        action.respond(response_type);
    }));

    helpers::create_simple_action_strong(action.clone(), "open_project", |action| action.activate())
}

impl OpenProjectAction {
    fn activate(&self) {
        self.dialog.show();
    }

    fn respond(&self, response_type: gtk::ResponseType) {
        self.dialog.hide();

        let window = match self.window.upgrade() {
            Some(w) => w,
            None => return,
        };
        
        match response_type {
            gtk::ResponseType::Accept => {
                if let Some(file) = self.dialog.file() {
                    window.open_project(file);
                }
            },
            _ => {} /* we were cancelled, ignore */
        }

        self.dialog.hide();
    }
}
