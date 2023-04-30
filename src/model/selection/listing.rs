use std::sync;

use crate::model::addr;
use crate::model::document;
use crate::model::document::change as doc_change;
use crate::model::document::structure;
use crate::model::listing::token;
use crate::model::versioned;
use crate::model::versioned::Versioned;

#[derive(Debug, Clone, Copy)]
pub enum ModeType {
    Neither,
    Structure,
    Address,
}

#[derive(Debug, Clone, Hash)]
pub struct StructureRange {
    pub path: structure::Path,
    pub begin: (addr::Address, usize),
    pub end: (addr::Address, usize), //< exclusive
}

#[derive(Debug, Clone, Hash)]
pub enum StructureMode {
    Empty,
    Range(StructureRange),
    All
}

#[derive(Debug, Clone, Hash)]
pub enum Mode {
    Structure(StructureMode),
    Address(addr::Extent),
}

#[derive(Debug, Clone)]
pub struct Selection {
    pub document: sync::Arc<document::Document>,
    pub mode: Mode,

    version: versioned::Version<Selection>,
}

pub type Host = versioned::Host<Selection>;

#[derive(Clone)]
pub enum Change {
    DocumentUpdated(sync::Arc<document::Document>),

    Clear,
    SelectAll,
    ConvertToStructure,
    ConvertToAddress,
    AssignFromHierarchy(sync::Arc<super::HierarchySelection>),
    AssignStructure(StructureRange),
    AssignAddress(addr::Extent),
    UnionStructure(StructureRange),
    UnionAddress(addr::Extent),
}

#[derive(Debug, Clone)]
pub struct ChangeRecord {
}

#[derive(Debug, Clone)]
pub enum ApplyError {
    WrongMode
}

#[derive(Clone, Copy, Debug, Hash)]
pub enum TokenIntersection {
    None,
    Partial(addr::Extent),
    Total,
}

impl Selection {
    pub fn new(document: sync::Arc<document::Document>) -> Self {
        Selection {
            document,
            mode: Mode::Structure(StructureMode::Range(StructureRange {
                path: vec![],
                begin: (addr::Address::from(0x10), 0),
                end: (addr::Address::from(0x20), 0),
            })),
            
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

impl versioned::Versioned for Selection {
    type Change = Change;

    fn version(&self) -> &versioned::Version<Selection> {
        &self.version
    }

    fn version_mut(&mut self) -> &mut versioned::Version<Selection> {
        &mut self.version
    }
}

impl versioned::Change<Selection> for Change {
    type ApplyError = ApplyError;
    type ApplyRecord = ChangeRecord;

    #[allow(unused_mut)] // TODO: remove me once this is implemented
    fn apply(mut self, _selection: &mut Selection) -> Result<(Change, ChangeRecord), ApplyError> {
        todo!();
    }
}

impl Mode {
    pub fn token_intersection(&self, token: &token::Token) -> TokenIntersection {
        match self {
            Mode::Structure(StructureMode::Empty) => TokenIntersection::None,
            Mode::Structure(StructureMode::All) => TokenIntersection::Total,
            Mode::Structure(StructureMode::Range(range)) => {
                if token.node_path.len() >= range.path.len() && token.node_path[0..range.path.len()] == range.path[..] {
                    if token.node_path.len() == range.path.len() {
                        TokenIntersection::Partial(addr::Extent::between(range.begin.0, range.end.0))
                    } else if (range.begin.1..range.end.1).contains(&token.node_path[range.path.len()]) {
                        TokenIntersection::Total
                    } else {
                        TokenIntersection::None
                    }
                } else {
                    TokenIntersection::None
                }
            }
            Mode::Address(extent) => {
                let token_extent = addr::Extent::sized(token.node_addr, token.node.size);

                if extent.contains(token_extent) {
                    TokenIntersection::Total
                } else {
                    match extent.intersection(token_extent) {
                        Some(e) => TokenIntersection::Partial(e.debase(token.node_addr)),
                        None => TokenIntersection::None,
                    }
                }
            },
        }
    }
}

impl TokenIntersection {
    pub fn includes(&self, extent: addr::Extent) -> bool {
        match self {
            Self::None => false,
            Self::Partial(e) => e.contains(extent),
            Self::Total => true,
        }
    }
}
