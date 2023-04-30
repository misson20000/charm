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

    fn port_doc_change(&mut self, new_doc: &sync::Arc<document::Document>, change: &doc_change::Change) {
        self.document = new_doc.clone();

        self.mode = match std::mem::replace(&mut self.mode, Mode::Structure(StructureMode::Empty)) {
            Mode::Structure(sm) => Mode::Structure(match sm {
                StructureMode::Empty => StructureMode::Empty,
                StructureMode::Range(sr) => sr.port_doc_change(new_doc, change),
                StructureMode::All => StructureMode::All,
            }),
            Mode::Address(extent) => Mode::Address(extent),
        };
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

    fn apply(mut self, selection: &mut Selection) -> Result<(Change, ChangeRecord), ApplyError> {
        let record = match &mut self {
            Change::DocumentUpdated(new_document) => selection.update_document(new_document),
            _ => todo!(),
        };

        Ok((self, record))
    }
}

impl StructureRange {
    fn extent(&self) -> addr::Extent {
        addr::Extent::between(self.begin.0, self.end.0)
    }
    
    fn port_doc_change(mut self, new_doc: &sync::Arc<document::Document>, change: &doc_change::Change) -> StructureMode {
        let ret = match change.update_path(&mut self.path) {
            doc_change::UpdatePathResult::Moved | doc_change::UpdatePathResult::Unmoved => StructureMode::Range(match &change.ty {
                doc_change::ChangeType::AlterNode(_, _) => self,
                doc_change::ChangeType::InsertNode(affected_path, insertion_index, childhood) if affected_path == &self.path => {
                    if self.begin.1 > *insertion_index || (self.begin.1 == *insertion_index && self.begin.0 > childhood.offset) {
                        self.begin.1+= 1;
                    }
                    
                    if self.end.1 >= *insertion_index {
                        self.end.1+= 1;
                    }
                    
                    if childhood.extent().includes(self.begin.0) {
                        self.begin.0 = childhood.offset;
                    }
                    
                    if childhood.extent().includes(self.end.0) {
                        self.end.0 = childhood.end();
                    }
                    
                    self
                }
                doc_change::ChangeType::InsertNode(_, _, _) => self,
                doc_change::ChangeType::Nest(affected_path, nested_first, nested_last, nested_extent, _props) if affected_path == &self.path => {
                    let nested = *nested_first..=*nested_last;
                    let new_child = &new_doc.lookup_node(affected_path).0.children[*nested_first];
                        
                    if nested.contains(&self.begin.1) && nested.contains(&self.end.1) && nested_extent.contains(self.extent()) {
                        /* The entire range was nested, so just select the same range in the child. */
                        self.path.push(*nested_first);
                        self.begin.0-= new_child.offset.to_size();
                        self.begin.1-= *nested_first;
                        self.end.1-= *nested_first;
                    } else {
                        /* If the beginning was part of the nested range, adjust it to the beginning of the nest. */
                        if nested_extent.includes(self.begin.0) {
                            self.begin.0 = nested_extent.begin;
                        }

                        if nested.contains(&self.begin.1) {
                            self.begin.1 = *nested_first;
                        } else if self.begin.1 > *nested_last {
                            self.begin.1-= nested_last-nested_first;
                        }

                        /* If the end was part of the nested range, adjust it to the end of the nest. */
                        if nested_extent.includes(self.end.0) {
                            self.end.0 = nested_extent.end;
                        }

                        if nested.contains(&self.end.1) {
                            self.end.1 = nested_first + 1;
                        } else if self.end.1 > *nested_last {
                            self.end.1-= nested_last-nested_first;
                        }
                    }

                    self
                },
                doc_change::ChangeType::Nest(_, _, _, _, _) => self,
                doc_change::ChangeType::DeleteRange(affected_path, deleted_first, deleted_last) if affected_path == &self.path => {
                    let deleted = *deleted_first..=*deleted_last;
                    if deleted.contains(&self.begin.1) && deleted.contains(&self.end.1) {
                        return StructureMode::Empty;
                    } else {
                        if deleted.contains(&self.begin.1) {
                            self.begin.1 = *deleted_first;
                        } else if self.begin.1 > *deleted_last {
                            self.begin.1-= (*deleted_last-*deleted_first)+1;
                        }
                        
                        if deleted.contains(&self.end.1) {
                            self.end.1 = *deleted_first-1;
                        } else if self.end.1 > *deleted_last {
                            self.end.1-= (*deleted_last-*deleted_first)+1;
                        }

                        self
                    }
                },
                doc_change::ChangeType::DeleteRange(_, _, _) => self,
            }),
            doc_change::UpdatePathResult::Deleted => StructureMode::Empty,
        };

        ret.check_integrity(new_doc);

        ret
    }

    fn check_integrity(&self, document: &document::Document) {
        let node = document.lookup_node(&self.path).0;
        assert!(self.begin.0 <= node.children[self.begin.1].offset);
        if self.end.1 != node.children.len() {
            assert!(self.end.0 >= node.children[self.end.1].end());
        }
        assert!(self.end.0 <= node.size.to_addr());
    }
}

impl StructureMode {
    fn check_integrity(&self, document: &document::Document) {
        match self {
            StructureMode::Empty => {},
            StructureMode::Range(sr) => sr.check_integrity(document),
            StructureMode::All => {},
        }
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

    pub fn is_total(&self) -> bool {
        match self {
            Self::Total => true,
            _ => false,
        }
    }
}
