use gtk::prelude::*;
use gtk::glib;
use gtk::glib::clone;
use gtk::gio;

use std::rc;

use crate::catch_panic;
use crate::model::datapath;
use crate::model::space;
use crate::serialization;
use crate::view::error;
use crate::view::helpers;
use crate::view::project;
use crate::view::window;
use crate::view::window::ErrorReporter;

struct OpenProjectAction {
    window: rc::Weak<window::CharmWindow>,
    dialog: gtk::FileChooserNative,
}

pub fn add_action(window: &rc::Rc<window::CharmWindow>) {
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
    
    action.dialog.connect_response(clone!(#[weak] action, move |_dialog, response_type| catch_panic! {
        action.respond(response_type);
    }));

    window.window.add_action(&helpers::create_simple_action_strong(action.clone(), "open_project", |action| action.activate()));
}

pub fn open_project(window: &rc::Rc<window::CharmWindow>, project_file: gio::File) {
    let e = match try_open_project(window, project_file) {
        Ok(()) => return,
        Err(e) => e
    };

    match e {
        OpenProjectError::IoError(e) => window.report_error(error::Error {
            while_attempting: error::Action::OpenProject,
            trouble: error::Trouble::GlibIoError(e),
            level: error::Level::Error,
            is_bug: false,
        }),
        OpenProjectError::DeserializationError(e) => window.report_error(error::Error {
            while_attempting: error::Action::OpenProject,
            trouble: error::Trouble::ProjectDeserializationFailure(e),
            level: error::Level::Error,
            is_bug: false,
        }),
    };
}

fn try_open_project(window: &rc::Rc<window::CharmWindow>, project_file: gio::File) -> Result<(), OpenProjectError> {
    let (bytes, _string) = project_file.load_bytes(gio::Cancellable::NONE)?;
    let document = serialization::deserialize_project(bytes.as_ref())?;

    /* Open any FileAddressSpaces that don't try to get opened during deserialization. */
    for filter in document.datapath.iter_filters() {
        match filter {
            datapath::Filter::LoadSpace(lsf) => match &**lsf.space() {
                space::AddressSpace::File(f) => match f.try_open() {
                    Ok(_) => {},
                    Err(e) => window.report_error(error::Error {
                        while_attempting: error::Action::OpenProject,
                        trouble: error::Trouble::OpenAddressSpaceError {
                            error: e,
                            path: f.path.clone(),
                        },
                        level: error::Level::Error,
                        is_bug: false,
                    }),
                },
            },
            _ => {}
        }
    }
    
    window.open_project(project::Project::new_from_save(document, project_file), false, true);

    Ok(())
}

impl OpenProjectAction {
    fn activate(&self) {
        self.dialog.show();
    }
    
    fn respond(&self, response_type: gtk::ResponseType) {
        self.dialog.hide();

        let Some(window) = self.window.upgrade() else { return };
        
        match response_type {
            gtk::ResponseType::Accept => {
                if let Some(file) = self.dialog.file() {
                    open_project(&window, file);
                }
            },
            _ => {} /* we were cancelled, ignore */
        }

        self.dialog.hide();
    }
}

pub enum OpenProjectError {
    IoError(glib::error::Error),
    DeserializationError(serialization::DeserializationError),
}

impl From<glib::error::Error> for OpenProjectError {
    fn from(e: glib::error::Error) -> OpenProjectError {
        OpenProjectError::IoError(e)
    }
}

impl From<serialization::DeserializationError> for OpenProjectError {
    fn from(e: serialization::DeserializationError) -> OpenProjectError {
        OpenProjectError::DeserializationError(e)
    }
}
