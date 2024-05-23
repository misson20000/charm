use gtk::prelude::*;
use gtk::glib;
use gtk::glib::clone;
use gtk::gio;

use std::rc;
use std::sync;
use std::vec;

use crate::catch_panic;
use crate::model::document;
use crate::model::document::structure;
use crate::model::space;
use crate::view::error;
use crate::view::helpers;
use crate::view::window;

struct NewProjectAction {
    window: rc::Weak<window::CharmWindow>,
    file_chooser: gtk::FileChooserNative,
}

pub fn add_actions(window: &rc::Rc<window::CharmWindow>) {
    let dialog = gtk::FileChooserNative::builder()
        .accept_label("Open")
        .cancel_label("Cancel")
        .title("Charm: Open File")
        .modal(true)
        .transient_for(&window.window)
        .action(gtk::FileChooserAction::Open)
        .create_folders(true)
        .build();

    let action = rc::Rc::new(NewProjectAction {
        window: rc::Rc::downgrade(window),
        file_chooser: dialog
    });

    action.file_chooser.connect_response(clone!(@weak action => move |_dialog, response_type| catch_panic! {
        action.file_chooser_response(response_type);
    }));

    window.window.add_action(&helpers::create_simple_action_strong(action.clone(), "new_project.empty", |action| action.activate_empty()));
    window.window.add_action(&helpers::create_simple_action_strong(action.clone(), "new_project.from_file", |action| action.activate_from_file()));
}

enum NewProjectError {
    GlibError(glib::Error),
    IoError(std::io::Error),
    MissingDisplayName,
    NonNativeFile,
    InvalidSize(std::num::TryFromIntError)
}

impl From<glib::Error> for NewProjectError {
    fn from(gle: glib::Error) -> Self {
        NewProjectError::GlibError(gle)
    }
}

impl From<std::io::Error> for NewProjectError {
    fn from(ioe: std::io::Error) -> Self {
        NewProjectError::IoError(ioe)
    }
}

impl From<std::num::TryFromIntError> for NewProjectError {
    fn from(tfie: std::num::TryFromIntError) -> Self {
        NewProjectError::InvalidSize(tfie)
    }
}

impl NewProjectAction {
    fn activate_empty(&self) {
        let Some(window) = self.window.upgrade() else { return };

        window.open_context(document::Builder::default().build(), None);
    }
    
    fn activate_from_file(&self) {
        self.file_chooser.show();
    }

    fn new_project_from_file(file: &gio::File) -> Result<document::Document, NewProjectError> {
        if !file.is_native() {
            return Err(NewProjectError::NonNativeFile);
        }
        
        let attributes = file.query_info("standard::display-name,standard::size", gio::FileQueryInfoFlags::NONE, gio::Cancellable::NONE)?;
        let dn = attributes.attribute_as_string("standard::display-name").ok_or(NewProjectError::MissingDisplayName)?;
        let size = attributes.size();

        let fas = space::file::FileAddressSpace::new(file.path().unwrap(), &dn);
        fas.try_open()?;
        
        let space = std::sync::Arc::new(fas.into());

        let root = sync::Arc::new(structure::Node {
            props: structure::Properties {
                name: dn.to_string(),
                title_display: structure::TitleDisplay::default(),
                children_display: structure::ChildrenDisplay::default(),
                content_display: structure::ContentDisplay::default(),
                locked: true,
            },
            size: u64::try_from(size)?.into(),
            children: vec::Vec::new(),
        });

        Ok(document::Builder::new(root)
            .load_space(space)
            .build())
    }
    
    fn file_chooser_response(&self, response_type: gtk::ResponseType) {
        self.file_chooser.hide();

        let Some(window) = self.window.upgrade() else { return };
        
        match response_type {
            gtk::ResponseType::Accept => {                
                for file in &self.file_chooser.files() {
                    let file = file.expect("list model should not be modified during iteration");

                    match Self::new_project_from_file(&file.downcast().expect("file chooser files should be gio::File instances")) {
                        Ok(doc) => window.open_context(doc, None),
                        Err(e) => window.report_error(error::Error {
                            while_attempting: error::Action::NewProjectFromFile,
                            trouble: match e {
                                NewProjectError::GlibError(e) => error::Trouble::GlibIoError(e),
                                NewProjectError::IoError(e) => error::Trouble::StdIoError(e),
                                NewProjectError::MissingDisplayName => error::Trouble::Other("Unable to retrieve display name for file".to_string()),
                                NewProjectError::NonNativeFile => error::Trouble::Other("Unable to create project from non-native file".to_string()),
                                NewProjectError::InvalidSize(_) => error::Trouble::Other("Unable to get size of file".to_string()),
                            },
                            level: error::Level::Error,
                            is_bug: false,
                        }),
                    };
                }
            },
            _ => {} /* we were cancelled, ignore */
        }
    }
}
