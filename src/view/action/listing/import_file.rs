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
use crate::view::listing;
use crate::view::window;
use crate::view::window::ErrorReporter;

struct ImportFileAction {
    document_host: sync::Arc<document::DocumentHost>,
    lw: listing::ListingWidget,
    window: rc::Weak<window::CharmWindow>,
    dialog: gtk::FileChooserNative,
}

enum ImportFileError {
    GlibError(glib::Error),
    IoError(std::io::Error),
    MissingDisplayName,
    NonNativeFile,
}

impl From<glib::Error> for ImportFileError {
    fn from(gle: glib::Error) -> Self {
        ImportFileError::GlibError(gle)
    }
}

impl From<std::io::Error> for ImportFileError {
    fn from(ioe: std::io::Error) -> Self {
        ImportFileError::IoError(ioe)
    }
}

pub fn add_action(window_context: &window::WindowContext) {
    let dialog = gtk::FileChooserNative::builder()
        .accept_label("Import")
        .cancel_label("Cancel")
        .title("Charm: Import File into Project")
        .modal(true)
        .transient_for(&window_context.window.upgrade().unwrap().window)
        .action(gtk::FileChooserAction::Open)
        .select_multiple(false)
        .build();
    
    let action = rc::Rc::new(ImportFileAction {
        document_host: window_context.project.document_host.clone(),
        lw: window_context.lw.clone(),
        window: window_context.window.clone(),
        dialog,
    });
    
    action.dialog.connect_response(clone!(#[weak] action, move |_dialog, response_type| catch_panic! {
        action.respond(response_type);
    }));

    window_context.action_group.add_action(&helpers::create_simple_action_strong(action.clone(), "import_file", |action| action.activate()));
}

impl ImportFileAction {
    fn activate(&self) {
        self.dialog.show();
    }

    fn do_import(&self, file: gio::File) -> Result<(), error::Trouble> {
        let mut cursor = self.lw.cursor_mut();
        let addr = cursor.addr();

        if addr.bits() != 0 {
            return Err(error::Trouble::Other("Cannot import file to non-byte-aligned address".to_string()));
        }

        if !file.is_native() {
            return Err(error::Trouble::Other("Unable to import non-native file".to_string()));
        }

        let attributes = file.query_info("standard::display-name,standard::size", gio::FileQueryInfoFlags::NONE, gio::Cancellable::NONE)?;
        
        let dn = attributes.attribute_as_string("standard::display-name").ok_or(error::Trouble::Other("Unable to retrieve display name for file".to_string()))?;
        
        let size = attributes.size() as u64;
        
        let fas = space::file::FileAddressSpace::new(file.path().unwrap(), &dn);
        fas.try_open()?;

        let space = std::sync::Arc::new(fas.into());

        let node = sync::Arc::new(structure::Node {
            props: structure::Properties {
                name: dn.to_string(),
                title_display: structure::TitleDisplay::default(),
                children_display: structure::ChildrenDisplay::default(),
                content_display: structure::ContentDisplay::default(),
                locked: true,
            },
            size: size.into(),
            children: vec::Vec::new(),
        });
        
        let doc = cursor.insert_node(&*self.document_host, node)?;
        self.document_host.change(doc.overwrite_space(addr.bytes(), space, Some(size)))?;

        Ok(())
    }
    
    fn respond(&self, response_type: gtk::ResponseType) {
        self.dialog.hide();

        let Some(window) = self.window.upgrade() else { return };
        
        match response_type {
            gtk::ResponseType::Accept => {
                if let Some(file) = self.dialog.file() {
                    if let Err(trouble) = self.do_import(file) {
                        window.report_error(error::Error {
                            while_attempting: error::Action::ImportFile,
                            trouble,
                            level: error::Level::Error,
                            is_bug: false,
                        });
                    }
                }
            },
            _ => {} /* we were cancelled, ignore */
        }

        self.dialog.hide();
    }
}
