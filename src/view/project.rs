use std::cell;
use std::sync;

use crate::model::document;
use crate::model::versioned::Versioned;
use crate::serialization;

use gtk::prelude::*;
use gtk::gio;

#[derive(Debug)]
pub struct Project {
    pub document_host: sync::Arc<document::DocumentHost>,
    pub save_file: cell::RefCell<Option<gio::File>>,
    pub last_save: cell::RefCell<Option<sync::Arc<document::Document>>>,
}

pub enum SaveProjectError {
    IoError(glib::error::Error),
    SerializationError(serialization::SerializationError),
}

impl From<glib::error::Error> for SaveProjectError {
    fn from(e: glib::error::Error) -> SaveProjectError {
        SaveProjectError::IoError(e)
    }
}

impl From<serialization::SerializationError> for SaveProjectError {
    fn from(e: serialization::SerializationError) -> SaveProjectError {
        SaveProjectError::SerializationError(e)
    }
}

impl Project {
    pub fn new_unsaved(doc: document::Document) -> Self {
        let document_host = sync::Arc::new(document::DocumentHost::new(doc));

        Project {
            document_host,
            save_file: cell::RefCell::new(None),
            last_save: cell::RefCell::new(None),
        }
    }

    pub fn new_from_save(doc: document::Document, file: gio::File) -> Self {
        let document_host = sync::Arc::new(document::DocumentHost::new(doc));
        let last_save = document_host.get();
        
        Project {
            document_host,
            save_file: cell::RefCell::new(Some(file)),
            last_save: cell::RefCell::new(Some(last_save)),
        }
    }

    /// Returns true if the open project has never been saved, or has been modified since last saved.
    pub fn has_unsaved_changes(&self) -> bool {
        let last_save_borrow = self.last_save.borrow();
        let Some(last_save) = last_save_borrow.as_ref() else { return true };
        !sync::Arc::ptr_eq(&*self.document_host.borrow(), last_save)
    }
    
    pub fn try_save_to(&self, file: gio::File) -> Result<(), SaveProjectError> {
        let document = self.document_host.get();
        
        let bytes = serialization::serialize_project(&*document)?;

        file.replace_contents(&bytes[..], None, false, gio::FileCreateFlags::REPLACE_DESTINATION, gio::Cancellable::NONE)?;

        let mut guard = self.save_file.borrow_mut();
        
        if guard.as_ref() != Some(&file) {
            *guard = Some(file);
        }

        *self.last_save.borrow_mut() = Some(document);
        
        Ok(())
    }

    pub fn revert_from(&self, reference: &sync::Arc<document::Document>, revert: u32) -> Self {
        let mut document = reference;

        for _ in 0..revert {
            document = match document.previous() {
                Some((doc, _)) => doc,
                None => document,
            }
        }

        Project {
            document_host: sync::Arc::new(document::DocumentHost::new((**document).clone())),
            save_file: self.save_file.clone(),
            last_save: self.last_save.clone(),
        }
    }

    pub fn title(&self) -> String {
        let doc = self.document_host.borrow();
        let root_node_name = &doc.root.props.name;
        
        match self.save_file.borrow().as_ref() {
            Some(f) => match f.path() {
                Some(p) => format!("{} ({})", p.display(), root_node_name),
                None => format!("{} ({})", f.uri(), root_node_name),
            },
            None => format!("<unsaved> ({})", root_node_name),
        }
    }
}
