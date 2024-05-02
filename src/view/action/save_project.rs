use gtk::prelude::*;
use gtk::glib;
use gtk::glib::clone;
use gtk::gio;

use std::rc;

use crate::view::helpers;
use crate::view::window;

struct SaveProjectAction {
    window: rc::Weak<window::CharmWindow>,
    dialog: gtk::FileChooserNative,
}

pub fn create_actions(window: &rc::Rc<window::CharmWindow>) -> (gio::SimpleAction, gio::SimpleAction) {
    let filter = gtk::FileFilter::new();
    filter.set_name(Some("Charm Projects"));
    filter.add_pattern("*.charm");
    
    let dialog = gtk::FileChooserNative::builder()
        .accept_label("Save")
        .cancel_label("Cancel")
        .title("Charm: Save Project")
        .modal(true)
        .transient_for(&window.window)
        .action(gtk::FileChooserAction::Save)
        .select_multiple(false)
        .create_folders(true)
        .filter(&filter)
        .build();
    
    let action = rc::Rc::new(SaveProjectAction {
        window: rc::Rc::downgrade(window),
        dialog: dialog
    });
    
    action.dialog.connect_response(clone!(@weak action => move |_dialog, response_type| {
        /* FFI CALLBACK */
        action.respond(response_type);
    }));
    
    (helpers::create_simple_action_strong(action.clone(), "save_project", |action| action.activate_save()),
     helpers::create_simple_action_strong(action.clone(), "save_project_as", |action| action.activate_save_as()))
}

impl SaveProjectAction {
    fn activate_save(&self) {
        let window = match self.window.upgrade() {
            Some(w) => w,
            None => return,
        };

        let ctx_guard = window.context();

        let ctx = match &*ctx_guard {
            Some(wc) => wc,
            None => return,
        };

        let file = ctx.project_file.borrow().clone();

        match file {
            Some(file) => ctx.save_project(file),
            None => self.open_save_as_dialog(None),
        };
    }

    fn activate_save_as(&self) {
        self.open_save_as_dialog(None);
    }

    fn open_save_as_dialog(&self, default: Option<gio::File>) {
        if let Some(default) = default {
            /* Ignore error */
            let _ = self.dialog.set_file(&default);
        }

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
                    let ctx_guard = window.context();
                    if let Some(ctx) = &*ctx_guard {
                        ctx.save_project(file);
                    }
                }
            },
            _ => {} /* we were cancelled, ignore */
        }

        self.dialog.hide();
    }
}
