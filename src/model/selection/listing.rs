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
    AssignFromTree(sync::Arc<super::TreeSelection>),
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

            /*
            Change::Clear => {
                selection.mode = match selection.mode {
                    Mode::Structure(_) => selection.mode = Mode::Structure(StructureMode::Empty),
                    Mode::Address(_) => selection.mode = Mode::Address(addr::unit::EMPTY),
                };
                ChangeRecord {}
            },
            Change::SelectAll => {
                selection.mode.select_all(),
            
            Change::ConvertToStructure => match selection.mode {
                Mode::Structure(_) =>
                    return Err(ApplyError::WrongMode),
                Mode::Address(extent) =>
                    selection.mode = Mode::Structure(StructureMode::try_from_extent(extent)?),
            },

            Change::ConvertToAddress => match selection.mode {
                Mode::Structure(StructureMode::Empty) => selection.mode =
                    Mode::Address(addr::unit::EMPTY),
                Mode::Structure(StructureMode::Range(sr)) => selection.mode =
                    Mode::Address(sr.get_extent(self.document)),
                Mode::Structure(StructureMode::All) => selection.mode =
                    Mode::Address(addr::unit::UNBOUNDED),
            },

                Change::AssignFromTree(sync::Arc<super::TreeSelection>) => todo!(),
            */
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
                doc_change::ChangeType::AlterNode { .. } => self,
                doc_change::ChangeType::InsertNode { parent: affected_path, index: insertion_index, child: childhood } if affected_path == &self.path => {
                    if self.begin.1 > *insertion_index || (self.begin.1 == *insertion_index && self.begin.0 > childhood.offset) {
                        self.begin.1+= 1;
                    }
                    
                    if self.end.1 >= *insertion_index {
                        self.end.1+= 1;
                    }
                    
                    if childhood.extent().includes(self.begin.0) {
                        self.begin.0 = childhood.offset;
                        self.begin.1 = *insertion_index;
                    }
                    
                    if childhood.extent().includes(self.end.0) {
                        self.end.0 = childhood.end();
                    }
                    
                    self
                }
                doc_change::ChangeType::InsertNode { .. } => self,
                doc_change::ChangeType::Nest { parent: affected_path, first_child: nested_first, last_child: nested_last, extent: nested_extent, props: _ } if affected_path == &self.path => {
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
                doc_change::ChangeType::Nest { .. } => self,
                doc_change::ChangeType::Destructure { parent: affected_parent, child_index, num_grandchildren, offset: _ } if affected_parent == &self.path => {
                    if self.begin.1 > *child_index {
                        self.begin.1+= *num_grandchildren;
                    }

                    if self.end.1 > *child_index {
                        self.end.1+= *num_grandchildren;
                    }

                    self
                },
                doc_change::ChangeType::Destructure { .. } => self,
                doc_change::ChangeType::DeleteRange { parent: affected_path, first_child: deleted_first, last_child: deleted_last } if affected_path == &self.path => {
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
                doc_change::ChangeType::DeleteRange { .. } => self,
            }),
            doc_change::UpdatePathResult::Destructured => match &change.ty {
                /* We had selected a range within to a node that got
                 * destructued, so we need to select that same range in its new
                 * position in the new parent. */
                doc_change::ChangeType::Destructure { parent: _, child_index, num_grandchildren: _, offset } => {
                    self.begin.0+= offset.to_size();
                    self.begin.1+= *child_index;
                    self.end.0+= offset.to_size();
                    self.end.1+= *child_index;
                    StructureMode::Range(self)
                },
                _ => panic!("got UpdatePathResult::Destructured from a non-Destructure type Change"),
            },
            doc_change::UpdatePathResult::Deleted => StructureMode::Empty,
        };

        if !ret.check_integrity(new_doc) {
            println!("selection integrity check failed: {:?}", ret);
            return StructureMode::Empty;
        }

        ret
    }

    fn check_integrity(&self, document: &document::Document) -> bool {
        let node = document.lookup_node(&self.path).0;

        if self.begin.1 > node.children.len() {
            println!("begin index too large");
            return false;
        }

        if self.end.1 > node.children.len() {
            println!("end index too large");
            return false;
        }

        if self.begin.1 != node.children.len() {
            if self.begin.0 > node.children[self.begin.1].offset {
                println!("begins after first indexed child");
                return false;
            }
        }
        
        if self.end.1 != node.children.len() {
            if self.end.0 < node.children[self.end.1].end() {
                println!("ends before last indexed child");
                return false;
            }
        }

        if self.end.0 > node.size.to_addr() {
            println!("ends after end of node");
            return false;
        }

        true
    }
}

impl StructureMode {
    fn check_integrity(&self, document: &document::Document) -> bool{
        match self {
            StructureMode::Empty => true,
            StructureMode::Range(sr) => sr.check_integrity(document),
            StructureMode::All => true,
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
