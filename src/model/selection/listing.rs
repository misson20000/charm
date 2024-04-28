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
    AssignStructure(StructureMode),
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
pub enum NodeIntersection {
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

    pub fn is_empty(&self) -> bool {
        match &self.mode {
            Mode::Structure(StructureMode::Empty) => true,
            Mode::Address(extent) => extent.is_empty(),

            _ => false,
        }
    }

    pub fn is_structure(&self) -> bool {
        match &self.mode {
            Mode::Structure(_) => true,
            _ => false
        }
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

            Change::AssignStructure(mode) => {
                selection.mode = Mode::Structure(mode.clone());
                ChangeRecord {}
            },

            Change::Clear => {
                selection.mode = match selection.mode {
                    Mode::Structure(_) => Mode::Structure(StructureMode::Empty),
                    Mode::Address(_) => Mode::Address(addr::unit::EMPTY),
                };
                ChangeRecord {}
            },

                        /*
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

            _ => todo!(),
        };

        Ok((self, record))
    }
}

impl StructureRange {
    /// Converts the StructureRange to a SiblingRange and extent, if possible. This can fail if the StructureRange doesn't include any children, which cannot be represented by a SiblingRange.
    pub fn to_sibling_range_and_extent(self) -> Result<(structure::SiblingRange, addr::Extent), Self> {
        if self.end.1 == self.begin.1 {
            /* SiblingRange is inclusive, which can't represent this case. */
            return Err(self)
        }
        
        let extent = self.extent();
        Ok((structure::SiblingRange {
            parent: self.path,
            first: self.begin.1,
            last: self.end.1 - 1
        }, extent))
    }
    
    pub fn extent(&self) -> addr::Extent {
        addr::Extent::between(self.begin.0, self.end.0)
    }

    fn port_doc_change(mut self, new_doc: &sync::Arc<document::Document>, change: &doc_change::Change) -> StructureMode {
        let ret = match change.update_path(&mut self.path) {
            doc_change::UpdatePathResult::Moved | doc_change::UpdatePathResult::Unmoved => StructureMode::Range(match &change.ty {
                doc_change::ChangeType::AlterNode { .. } => self,
                doc_change::ChangeType::AlterNodesBulk { .. } => self,
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
                doc_change::ChangeType::Nest { range, extent: nested_extent, props: _ } if range.parent == self.path => {
                    let new_child = &new_doc.lookup_node(&range.parent).0.children[range.first];
                        
                    if range.contains_index(self.begin.1) && range.contains_index(self.end.1) && nested_extent.contains(self.extent()) {
                        /* The entire range was nested, so just select the same range in the child. */
                        self.path.push(range.first);
                        self.begin.0-= new_child.offset.to_size();
                        self.begin.1-= range.first;
                        self.end.1-= range.first;
                    } else {
                        /* If the beginning was part of the nested range, adjust it to the beginning of the nest. */
                        if nested_extent.includes(self.begin.0) {
                            self.begin.0 = nested_extent.begin;
                        }

                        if range.contains_index(self.begin.1) {
                            self.begin.1 = range.first;
                        } else if self.begin.1 > range.last {
                            self.begin.1-= range.count() - 1;
                        }

                        /* If the end was part of the nested range, adjust it to the end of the nest. */
                        if nested_extent.includes(self.end.0) {
                            self.end.0 = nested_extent.end;
                        }

                        if range.contains_index(self.end.1) {
                            self.end.1 = range.first + 1;
                        } else if self.end.1 > range.last {
                            self.end.1-= range.count() - 1;
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
                doc_change::ChangeType::DeleteRange { range } if range.parent == self.path => {
                    if range.contains_index(self.begin.1) && range.contains_index(self.end.1) {
                        return StructureMode::Empty;
                    } else {
                        if range.contains_index(self.begin.1) {
                            self.begin.1 = range.first;
                        } else if self.begin.1 > range.last {
                            self.begin.1-= range.count();
                        }
                        
                        if range.contains_index(self.end.1) {
                            self.end.1 = range.first-1;
                        } else if self.end.1 > range.last {
                            self.end.1-= range.count();
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
    pub fn node_intersection(&self, node: &structure::Node, node_path: &structure::Path, node_addr: addr::Address) -> NodeIntersection {
        match self {
            Mode::Structure(StructureMode::Empty) => NodeIntersection::None,
            Mode::Structure(StructureMode::All) => NodeIntersection::Total,
            Mode::Structure(StructureMode::Range(range)) => {
                if node_path.len() >= range.path.len() && node_path[0..range.path.len()] == range.path[..] {
                    if node_path.len() == range.path.len() {
                        NodeIntersection::Partial(addr::Extent::between(range.begin.0, range.end.0))
                    } else if (range.begin.1..range.end.1).contains(&node_path[range.path.len()]) {
                        NodeIntersection::Total
                    } else {
                        NodeIntersection::None
                    }
                } else {
                    NodeIntersection::None
                }
            }
            Mode::Address(extent) => {
                let node_extent = addr::Extent::sized(node_addr, node.size);

                if extent.contains(node_extent) {
                    NodeIntersection::Total
                } else {
                    match extent.intersection(node_extent) {
                        Some(e) => NodeIntersection::Partial(e.debase(node_addr)),
                        None => NodeIntersection::None,
                    }
                }
            },
        }
    }
}

impl NodeIntersection {
    pub fn includes(&self, addr: addr::Address) -> bool {
        match self {
            Self::None => false,
            Self::Partial(e) => e.includes(addr),
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
