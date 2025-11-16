use std::sync;

use crate::model::addr;
use crate::model::document;
use crate::model::document::change as doc_change;
use crate::model::document::structure;
use crate::model::versioned;
use crate::model::versioned::Versioned;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModeType {
    Neither,
    Structure,
    Address,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct StructureRange {
    pub path: structure::Path,
    pub begin: (addr::Offset, usize),
    pub end: (addr::Offset, usize), //< exclusive
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum StructureMode {
    Empty,
    Range(StructureRange),
    All
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructureEndpoint<'a> {
    pub parent: structure::PathSlice<'a>,
    pub child_index: usize,
    pub offset: addr::Offset
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Mode {
    Structure(StructureMode),
    Address(addr::AbsoluteExtent),
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
    AssignAddress(addr::AbsoluteExtent),
    UnionStructure(StructureRange),
    UnionAddress(addr::AbsoluteExtent),
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
    PartialStructure(addr::Extent, usize, usize),
    PartialAddress(addr::Extent),
    TotalStructure,
    TotalAddress,
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
            new_doc.changes_since(&self.document.clone(), &mut |new_doc, change_record| self.port_doc_change(new_doc, change_record));
            
            ChangeRecord {
            }
        } else {
            ChangeRecord {
            }
        }
    }

    fn port_doc_change(&mut self, new_doc: &sync::Arc<document::Document>, change_record: &doc_change::ApplyRecord) {
        self.document = new_doc.clone();

        self.mode = match std::mem::replace(&mut self.mode, Mode::Structure(StructureMode::Empty)) {
            Mode::Structure(sm) => Mode::Structure(match sm {
                StructureMode::Empty => StructureMode::Empty,
                StructureMode::Range(sr) => sr.port_doc_change(new_doc, change_record),
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

    pub fn is_address(&self) -> bool {
        match &self.mode {
            Mode::Address(_) => true,
            _ => false
        }
    }
    
    pub fn assert_integrity(&self) {
        match &self.mode {
            Mode::Structure(sm) => sm.assert_integrity(&*self.document),
            _ => {},
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
                    Mode::Address(_) => Mode::Address(addr::Extent::default()),
                };
                ChangeRecord {}
            },

            Change::AssignAddress(extent) => {
                selection.mode = Mode::Address(*extent);
                ChangeRecord {}
            },

            Change::SelectAll => {
                selection.mode = match selection.mode {
                    Mode::Structure(_) => Mode::Structure(StructureMode::All),
                    Mode::Address(_) => Mode::Address(selection.document.extent()),
                };
                ChangeRecord {}
            }
            
            Change::ConvertToStructure => {
                selection.mode = match selection.mode {
                    Mode::Structure(_) => return Err(ApplyError::WrongMode),
                    Mode::Address(extent) => Mode::Structure(StructureMode::best_effort_from_extent(&selection.document, extent)),
                };
                ChangeRecord {}
            },

            Change::ConvertToAddress => {
                selection.mode = match &selection.mode {
                    Mode::Address(_) => return Err(ApplyError::WrongMode),
                    Mode::Structure(StructureMode::Empty) => Mode::Address(addr::AbsoluteExtent::sized(0, 0)),
                    Mode::Structure(StructureMode::Range(sr)) => Mode::Address(sr.absolute_extent(&selection.document)),
                    Mode::Structure(StructureMode::All) => Mode::Address(selection.document.extent()),
                };
                ChangeRecord {}
            },

            _ => todo!(),
        };

        selection.assert_integrity();

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

    pub fn absolute_extent(&self, document: &document::Document) -> addr::AbsoluteExtent {
        let (_, addr) = document.lookup_node(&self.path);
        self.extent().absolute_from(addr)
    }

    pub fn single_child(document: &document::Document, parent: structure::PathSlice, child_index: usize) -> Self {
        let (parent_node, _) = document.lookup_node(parent);
        let childhood = &parent_node.children[child_index];
        let extent = childhood.extent();

        StructureRange {
            path: parent.iter().copied().collect(),
            begin: (extent.begin, child_index),
            end: (extent.end, child_index+1),
        }
    }
    
    pub fn between(document: &document::Document, a: StructureEndpoint, b: StructureEndpoint) -> Self {
        /* choose which is the beginning and which is the end */
        // TODO: replace with minmax when stabilized (https://github.com/rust-lang/rust/issues/115939)
        let begin = std::cmp::min(a.clone(), b.clone());
        let end = std::cmp::max(a, b);

        /* This is the common prefix of the path between the two pick results. */
        let path: Vec<usize> = std::iter::zip(begin.parent.iter(), end.parent.iter()).map_while(|(a, b)| if a == b { Some(*a) } else { None }).collect();
        let (node, _node_addr) = document.lookup_node(&path);

        let begin = if path.len() < begin.parent.len() {
            /* Beginning was deeper in the hierarchy than the common prefix. Round down to the start of the child of the common prefix. */
            (node.children[begin.parent[path.len()]].offset, begin.parent[path.len()])
        } else {
            (begin.offset, begin.child_index)
        };

        let end = if path.len() < end.parent.len() {
            /* End was deeper in the hierarchy than the common prefix. Bump out to the end of the child of the common prefix. */
            (node.children[end.parent[path.len()]].end(), end.parent[path.len()]+1)
        } else {
            (end.offset, end.child_index)
        };

        let ret = StructureRange {
            path,
            begin,
            end,
        };

        ret.assert_integrity(document);

        ret
    }
    
    fn port_doc_change(mut self, new_doc: &sync::Arc<document::Document>, change_record: &doc_change::ApplyRecord) -> StructureMode {
        self = match change_record.update_path(&mut self.path) {
            doc_change::UpdatePathResult::Moved | doc_change::UpdatePathResult::Unmoved => match change_record {
                doc_change::ApplyRecord::AlterNode { .. } => self,
                doc_change::ApplyRecord::AlterNodesBulk { .. } => self,
                doc_change::ApplyRecord::StackFilter { .. } => self,
                doc_change::ApplyRecord::InsertNode { parent: affected_path, index: insertion_index, child: childhood } if affected_path == &self.path => {
                    if self.begin.1 > *insertion_index || (self.begin.1 == *insertion_index && self.begin.0 > childhood.offset) {
                        self.begin.1+= 1;
                    }
                    
                    if self.end.1 >= *insertion_index && self.end.0 >= childhood.offset {
                        self.end.1+= 1;
                    }
                    
                    self
                }
                doc_change::ApplyRecord::InsertNode { .. } => self,
                doc_change::ApplyRecord::Nest { range, extent: nested_extent, .. } if range.parent == self.path => {
                    let new_child = &new_doc.lookup_node(&range.parent).0.children[range.first];
                        
                    if range.contains_index(self.begin.1) && range.contains_index(self.end.1) && nested_extent.contains(self.extent()) {
                        /* The entire range was nested, so just select the same range in the child. */
                        self.path.push(range.first);
                        self.begin.0-= new_child.offset;
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
                doc_change::ApplyRecord::Nest { .. } => self,
                doc_change::ApplyRecord::Destructure(dsr) if &dsr.parent == &self.path => {
                    dsr.adjust_sibling_index(&mut self.begin.1);
                    dsr.adjust_sibling_index(&mut self.end.1);

                    self
                },
                doc_change::ApplyRecord::Destructure { .. } => self,
                doc_change::ApplyRecord::DeleteRange { range } if range.parent == self.path => {
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
                doc_change::ApplyRecord::DeleteRange { .. } => self,
                doc_change::ApplyRecord::Resize { path, new_size, parents_resized } => {
                    /* If the node was shrunken, we can't let the selection be bigger than the new size. */
                    self.begin.0 = std::cmp::min(self.begin.0, *new_size);
                    self.end.0 = std::cmp::min(self.end.0, *new_size);
                    
                    if path.len() <= self.path.len() {
                        /* Ancestor or node containing selection was resized. We don't care. */
                        self
                    } else if path.len() - parents_resized > path.len() + 1 {
                        /* Resizing stopped deep enough in the tree that it can't affect either of our endpoints. */
                        self
                    } else if path[0..self.path.len()] != self.path {
                        /* Ancestor or unrelated node was resized. */
                        self
                    } else if self.begin.1 > path[self.path.len()] || self.end.1 <= path[self.path.len()] {
                        /* Resized node was a descendant of the node containing the selection, but wasn't part of the selection. */
                        self
                    } else {
                        /* A descendant of this selection was resized. Unfortunate. */
                        let sel_parent = new_doc.lookup_node(&self.path).0;

                        /* Was there a node before the beginning of our
                         * selection and was it grown? If so, it may have
                         * subsumed the beginning of the selection when it
                         * resized. */
                        if self.begin.1 > 0 && path[self.path.len()] == self.begin.1-1 {
                            if sel_parent.children[self.begin.1-1].end() > self.begin.0 {
                                self.begin = (sel_parent.children[self.begin.1-1].offset, self.begin.1-1);
                            }
                        }

                        /* Similarly, check if a node grew and subsumed the end of the selection. */
                        if self.end.1 > 0 && path[self.path.len()] == self.end.1-1 {
                            if sel_parent.children[self.end.1-1].end() > self.end.0 {
                                self.end = (sel_parent.children[self.begin.1-1].end(), self.end.1);
                            }
                        }

                        self
                    }
                },
                doc_change::ApplyRecord::Paste(par) if par.parent == self.path => {
                    par.adjust_sibling_index(&mut self.begin.1);
                    par.adjust_sibling_index(&mut self.end.1);
                    self
                },
                doc_change::ApplyRecord::Paste(_) => self,
                doc_change::ApplyRecord::Repeat { .. } => self,
            },
            doc_change::UpdatePathResult::Destructured => match change_record {
                /* We had selected a range within a node that got destructured,
                 * so we need to select that same range in its new position in
                 * the new parent. This may also include some interspersed nodes
                 * from the parent. */
                
                doc_change::ApplyRecord::Destructure(dsr) => {
                    self.begin.0+= dsr.offset();
                    self.end.0+= dsr.offset();
                    
                    self.begin.1 = dsr.mapping[self.begin.1];
                    self.end.1 = dsr.mapping[self.end.1];
                    
                    self
                },
                _ => panic!("got UpdatePathResult::Destructured from a non-Destructure type Change"),
            },
            doc_change::UpdatePathResult::Deleted => return StructureMode::Empty,
        };

        /* If the start or end are supposed to be within a child, repair by expanding the selection. */
        let node = new_doc.lookup_node(&self.path).0;
        if self.begin.1 > 0 && node.children[self.begin.1-1].extent().includes(self.begin.0) {
            self.begin.0 = node.children[self.begin.1-1].offset;
        }

        if self.end.1 > 0 && node.children[self.end.1-1].extent().includes(self.end.0) {
            self.end.0 = node.children[self.end.1-1].end();
        }
        
        self.assert_integrity(new_doc);

        StructureMode::Range(self)
    }

    fn assert_integrity(&self, document: &document::Document) {
        let node = document.lookup_node(&self.path).0;

        assert!(self.begin.1 <= self.end.1, "{:?} begin index {} was greater than end index {}", self, self.begin.1, self.end.1);
        assert!(self.begin.0 <= self.end.0, "{:?} begin offset {} was greater than end offset {}", self, self.begin.0, self.end.0);
        
        assert!(self.begin.1 <= node.children.len(), "{:?} begin index {} was too large (node has {} children)", self, self.begin.1, node.children.len());
        assert!(self.end.1 <= node.children.len(), "{:?} end index {} was too large (node has {} children)", self, self.end.1, node.children.len());

        if self.begin.1 != node.children.len() {
            assert!(self.begin.0 <= node.children[self.begin.1].offset, "{:?} begins after first indexed child (begins at {}, child at {})", self, self.begin.0, node.children[self.begin.1].offset);
        }
        
        if self.end.1 > 0 {
            assert!(self.end.0 >= node.children[self.end.1-1].end(), "{:?} ends before last indexed child (ends at {}, child ends at {})", self, self.end.0, node.children[self.end.1-1].end());
        }

        assert!(self.end.0 <= node.size, "{:?} ends after end of node (ends at {}, node size is {})", self, self.end.0, node.size);
    }
}

impl StructureMode {
    fn assert_integrity(&self, document: &document::Document) {
        match self {
            StructureMode::Empty => {},
            StructureMode::Range(sr) => sr.assert_integrity(document),
            StructureMode::All => {},
        }
    }

    pub fn entire_node(document: &document::Document, path: structure::PathSlice) -> Self {
        if path.is_empty() {
            StructureMode::All
        } else {
            StructureMode::Range(StructureRange::single_child(document, &path[0..(path.len()-1)], *path.last().unwrap()))
        }
    }

    pub fn best_effort_from_extent(document: &document::Document, extent: addr::AbsoluteExtent) -> Self {
        let Ok(search) = document.search_addr(extent.begin, document::search::Traversal::PostOrder) else { return Self::Empty };

        for hit in search {
            let extent_in_node = extent.relative_to(hit.node_addr);
            if extent_in_node.end >= hit.node.size {
                continue;
            }

            /* This finds the first node that starts after `extent_in_node.begin`. It's possible the one before that overlaps `extent_in_node`. */
            let mut begin = (extent_in_node.begin, hit.node.child_at_offset(extent_in_node.begin));
            if begin.1 > 0 {
                /* If the structure range would begin in the middle of a child, we can't have that. Need to expand it to include the child. */
                if hit.node.children[begin.1-1].end() > begin.0 {
                    begin.0 = hit.node.children[begin.1-1].offset;
                    begin.1-= 1;
                }
            }

            /* This finds the first node that starts after `extent_in_node.end`. This is actually a little bit more
             * conservative than necessary in the case that children are overlapping. It's possible the one before that
             * overlaps `extent_in_node`. */
            let mut end = (extent_in_node.end, hit.node.child_at_offset(extent_in_node.end));
            if end.1 > 0 {
                /* If the structure range would end in the middle of a child, we can't have that. Need to expand it to include the child. */
                if hit.node.children[end.1-1].end() > end.0 {
                    end.0 = hit.node.children[end.1-1].end();
                }
            }

            return Self::Range(StructureRange {
                path: hit.path,
                begin,
                end,
            });
        }

        return Self::Empty;
    }
}

impl Mode {
    pub fn node_intersection(&self, node: &structure::Node, node_path: &structure::Path, node_addr: addr::AbsoluteAddress) -> NodeIntersection {
        match self {
            Mode::Structure(StructureMode::Empty) => NodeIntersection::None,
            Mode::Structure(StructureMode::All) => NodeIntersection::TotalStructure,
            Mode::Structure(StructureMode::Range(range)) => {
                if node_path.len() >= range.path.len() && node_path[0..range.path.len()] == range.path[..] {
                    if node_path.len() == range.path.len() {
                        NodeIntersection::PartialStructure(addr::Extent::between(range.begin.0, range.end.0), range.begin.1, range.end.1)
                    } else if (range.begin.1..range.end.1).contains(&node_path[range.path.len()]) {
                        NodeIntersection::TotalStructure
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
                    NodeIntersection::TotalAddress
                } else {
                    match extent.intersection(node_extent) {
                        Some(e) => NodeIntersection::PartialAddress(e.relative_to(node_addr)),
                        None => NodeIntersection::None,
                    }
                }
            },
        }
    }
}

impl NodeIntersection {
    pub fn includes(&self, addr: addr::Offset, index: usize) -> bool {
        match self {
            Self::None => false,
            Self::PartialStructure(e, first, last) => e.includes(addr) && (*first..=*last).contains(&index),
            Self::PartialAddress(e) => e.includes(addr),
            Self::TotalStructure => true,
            Self::TotalAddress => true,
        }
    }

    pub fn touches(&self, addr: addr::Offset, index: usize) -> bool {
        match self {
            Self::None => false,
            Self::PartialStructure(e, first, last) => (e.includes(addr) || e.end == addr) && (*first..=*last).contains(&index),
            Self::PartialAddress(e) => e.includes(addr) || e.end == addr,
            Self::TotalStructure => true,
            Self::TotalAddress => true,
        }
    }

    pub fn overlaps(&self, extent: addr::Extent, index: usize) -> bool {
        match self {
            Self::None => false,
            Self::PartialStructure(e, first, last) => e.intersection(extent).is_some() && (*first..=*last).contains(&index),
            Self::PartialAddress(e) => e.intersection(extent).is_some(),
            Self::TotalStructure => true,
            Self::TotalAddress => true,
        }
    }
    
    pub fn includes_child(&self, index: usize) -> bool {
        match self {
            Self::PartialStructure(_extent, first, last) => (*first..*last).contains(&index),
            Self::TotalStructure => true,
            _ => false,
        }
    }

    pub fn mode_type(&self) -> ModeType {
        match self {
            Self::None => ModeType::Neither,
            Self::PartialStructure(..) => ModeType::Structure,
            Self::PartialAddress(..) => ModeType::Address,
            Self::TotalStructure => ModeType::Structure,
            Self::TotalAddress => ModeType::Address,
        }
    }
    
    pub fn is_total(&self) -> bool {
        match self {
            Self::TotalStructure => true,
            Self::TotalAddress => true,
            _ => false,
        }
    }

    pub fn mode_type_if(&self, cond: bool) -> ModeType {
        if cond {
            self.mode_type()
        } else {
            ModeType::Neither
        }
    }
}

impl<'a> StructureEndpoint<'a> {
    pub fn borrow(tuple: &'a (structure::Path, usize, addr::Offset)) -> Self {
        StructureEndpoint {
            parent: &tuple.0,
            child_index: tuple.1,
            offset: tuple.2,
        }
    }

    pub fn to_absolute_addr(&self, document: &document::Document) -> addr::AbsoluteAddress {
        document.lookup_node(self.parent).1 + self.offset
    }
}

impl<'a> Ord for StructureEndpoint<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let mut prefix_length = 0;
        for (a, b) in std::iter::zip(self.parent, other.parent) {
            match a.cmp(&b) {
                std::cmp::Ordering::Equal => prefix_length+= 1,
                x => return x
            }
        }

        /* We share a common prefix. */
        
        if self.parent.len() == prefix_length && other.parent.len() > prefix_length {
            if self.child_index <= other.parent[prefix_length] {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Greater
            }
        } else if self.parent.len() > prefix_length && other.parent.len() == prefix_length {
            if self.parent[prefix_length] >= other.child_index {
                std::cmp::Ordering::Greater
            } else {
                std::cmp::Ordering::Less
            }
        } else if self.parent.len() > prefix_length && other.parent.len() > prefix_length {
            self.parent[prefix_length].cmp(&other.parent[prefix_length])
        } else if self.parent.len() == prefix_length && other.parent.len() == prefix_length {
            if self.child_index == other.child_index {
                self.offset.cmp(&other.offset)
            } else {
                self.child_index.cmp(&other.child_index)
            }
        } else {
            panic!("should be unreachable");
        }
    }
}

impl<'a> PartialOrd for StructureEndpoint<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::versioned::Change;
    
    #[test]
    fn test_structure_update_insert_node_after() {
        let root = structure::Node::builder()
            .name("root")
            .size(0x40)
            .build();

        let mut document = document::Builder::new(root).build();

        let mut sel = Selection {
            document: sync::Arc::new(document.clone()),
            mode: Mode::Structure(StructureMode::Range(StructureRange {
                path: vec![],
                begin: (0x11.into(), 0),
                end: (0x13.into(), 0),
            })),
            version: Default::default(),
        };

        let (_change, record) = document.insert_node(
            vec![], 0, structure::Node::builder()
                .name("child")
                .size(0x10)
                .build_child(0x20))
            .apply(&mut document).unwrap();

        sel.port_doc_change(&sync::Arc::new(document), &record);

        assert_eq!(sel.mode, Mode::Structure(StructureMode::Range(StructureRange {
            path: vec![],
            begin: (0x11.into(), 0),
            end: (0x13.into(), 0),
        })));
    }
}
