use std::sync;

use crate::model::addr;
use crate::model::document;
use crate::model::document::change as doc_change;
use crate::model::document::structure;
use crate::model::versioned;
use crate::model::versioned::Versioned;

#[derive(Debug, Clone, Copy)]
pub enum ModeType {
    Neither,
    Structure,
    Address,
}

#[derive(Debug, Clone)]
pub struct StructureRange {
    pub path: structure::Path,
    pub begin: (addr::Address, usize),
    pub end: (addr::Address, usize), //< exclusive
}

#[derive(Debug, Clone)]
pub enum StructureMode {
    Empty,
    Range(StructureRange),
    All
}

#[derive(Debug, Clone)]
pub struct AddressRange {
    pub begin: addr::Address,
    pub end: addr::Address,
}

#[derive(Debug, Clone)]
pub enum Mode {
    Structure(StructureMode),
    Address(AddressRange),
}

#[derive(Debug, Clone)]
pub struct Selection {
    pub document: sync::Arc<document::Document>,
    pub mode: Mode,

    version: versioned::Version<Selection>,
}

impl Selection {
    pub fn new(document: sync::Arc<document::Document>) -> Self {
        Selection {
            document,
            mode: Mode::Structure(StructureMode::Empty),
            
            version: Default::default(),
        }
    }

    fn update_document(&mut self, new_doc: &sync::Arc<document::Document>) -> ChangeRecord {
        if self.document.is_outdated(new_doc) {
            new_doc.changes_since(&self.document.clone(), &mut |new_doc, change| self.port_doc_change(new_doc, change));
            
            ChangeRecord {
            }
        } else {
            ChangeRecord {
            }
        }
    }

    fn port_doc_change(&mut self, _new_doc: &sync::Arc<document::Document>, _change: &doc_change::Change) {
        todo!();
    }
}

#[derive(Clone)]
pub enum Change {
    DocumentUpdated(sync::Arc<document::Document>),

    Clear,
    SelectAll,
    ConvertToStructure,
    ConvertToAddress,
    AssignFromHierarchy(sync::Arc<super::HierarchySelection>),
    AssignStructure(StructureRange),
    AssignAddress(AddressRange),
    UnionStructure(StructureRange),
    UnionAddress(AddressRange),
}

#[derive(Debug, Clone)]
pub struct ChangeRecord {
}

#[derive(Debug, Clone)]
pub enum ApplyError {
    WrongMode
}

impl versioned::Versioned for Selection {
    type Change = Change;

    fn version(&self) -> &versioned::Version<Selection> {
        &self.version
    }

    fn version_mut(&mut self) -> &mut versioned::Version<Selection> {
        &mut self.version
    }
}

pub type Host = versioned::Host<Selection>;

impl versioned::Change<Selection> for Change {
    type ApplyError = ApplyError;
    type ApplyRecord = ChangeRecord;

    #[allow(unused_mut)] // TODO: remove me once this is implemented
    fn apply(mut self, _selection: &mut Selection) -> Result<(Change, ChangeRecord), ApplyError> {
        todo!();
    }
}
