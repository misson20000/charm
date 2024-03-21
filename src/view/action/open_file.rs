use gtk::prelude::*;
use gtk::glib;
use gtk::glib::clone;
use gtk::gio;

use std::rc;

use crate::view::helpers;
use crate::view::window;

struct OpenFileAction {
    window: rc::Weak<window::CharmWindow>,
    dialog: gtk::FileChooserNative,
}

pub fn create_action(window: &rc::Rc<window::CharmWindow>) -> gio::SimpleAction {
    let dialog = gtk::FileChooserNative::builder()
        .accept_label("Open")
        .cancel_label("Cancel")
        .title("Charm: Open File")
        .modal(true)
        .transient_for(&window.window)
        .action(gtk::FileChooserAction::Open)
        .select_multiple(true)
        .create_folders(true)
        .build();

    let action = rc::Rc::new(OpenFileAction {
        window: rc::Rc::downgrade(window),
        dialog: dialog
    });

    action.dialog.connect_response(clone!(@weak action => move |_dialog, response_type| {
        action.respond(response_type);
    }));
    
    helpers::create_simple_action_strong(action, "open", |action| action.activate())
}

impl OpenFileAction {
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
                for file in &self.dialog.files() {
                    let file = file.expect("list model should not be modified during iteration");
                    
                    if window.has_file_open() {
                        /* open a new window if this window already has something open in it */
                        let window = window.application.new_window();
                        window.open_file(&file.downcast().unwrap());
                        window.present();
                    } else {
                        window.open_file(&file.downcast().unwrap());
                        window.present();
                    }
                }
            },
            _ => {} /* we were cancelled, ignore */
        }

        self.dialog.hide();
    }
}
