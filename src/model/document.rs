pub mod change;
pub mod rebuild;
pub mod search;
pub mod structure;

use std::sync;
use std::vec;

use crate::model::addr;
use crate::model::datapath;
use crate::model::selection;
use crate::model::space;
use crate::model::versioned;
use crate::model::versioned::Versioned;

#[derive(Clone)]
pub struct Document {
    pub root: sync::Arc<structure::Node>,
    pub datapath: datapath::DataPath,

    version: versioned::Version<Document>,
}

pub struct Builder {
    root: sync::Arc<structure::Node>,
    datapath: datapath::DataPath,
}

impl versioned::Versioned for Document {
    type Change = change::Change;

    fn version(&self) -> &versioned::Version<Document> {
        &self.version
    }

    fn version_mut(&mut self) -> &mut versioned::Version<Document> {
        &mut self.version
    }
}

pub type DocumentHost = versioned::Host<Document>;

#[derive(Debug)]
pub enum LoadForTestingError {
    IoError(std::io::Error),
    XmlError(roxmltree::Error),
}

impl From<std::io::Error> for LoadForTestingError {
    fn from(e: std::io::Error) -> LoadForTestingError {
        LoadForTestingError::IoError(e)
    }
}

impl From<roxmltree::Error> for LoadForTestingError {
    fn from(e: roxmltree::Error) -> LoadForTestingError {
        LoadForTestingError::XmlError(e)
    }
}

impl Builder {
    pub fn default() -> Self {
        Self::new(sync::Arc::new(structure::Node {
            size: addr::Offset::MAX,
            props: structure::Properties {
                name: "root".to_string(),
                title_display: structure::TitleDisplay::Major,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::default(),
                locked: true,
            },
            children: vec::Vec::new()
        }))
    }
    
    pub fn new(root: sync::Arc<structure::Node>) -> Self {
        Builder {
            root,
            datapath: datapath::DataPath::new(),
        }
    }

    pub fn datapath(mut self, dp: datapath::DataPath) -> Self {
        self.datapath = dp;
        self
    }

    pub fn load_space(mut self, space: sync::Arc<space::AddressSpace>) -> Self {
        self.datapath.push(datapath::LoadSpaceFilter::new_defaults(space, 0, 0).to_filter());
        self
    }

    pub fn build(self) -> Document {
        Document {
            root: self.root,
            datapath: self.datapath,
            version: Default::default(),
        }
    }

    pub fn arc(self) -> sync::Arc<Document> {
        sync::Arc::new(self.build())
    }

    pub fn host(self) -> DocumentHost {
        DocumentHost::new(self.build())
    }
}

impl Document {
    pub fn load_from_testing_structure<P: AsRef<std::path::Path>>(path: P) -> Result<Document, LoadForTestingError> {
        let xml = std::fs::read_to_string(path)?;
        let xml = roxmltree::Document::parse(&xml)?;
        let tc = crate::model::listing::stream::xml::Testcase::from_xml(&xml);

        Ok(Document {
            root: tc.structure,
            datapath: datapath::DataPath::new(),
            version: Default::default(),
        })
    }

    pub fn lookup_node(&self, path: structure::PathSlice) -> (&sync::Arc<structure::Node>, addr::AbsoluteAddress) {
        let mut current_node = &self.root;
        let mut node_addr = addr::AbsoluteAddress::NULL;

        for i in path {
            let childhood = &current_node.children[*i];
            node_addr+= childhood.offset;
            current_node = &childhood.node;
        }

        (current_node, node_addr)
    }

    pub fn path_successor(&self, path: &mut structure::Path) -> bool {
        self.root.successor(path, 0)
    }
    
    pub fn search_addr<A: Into<addr::AbsoluteAddress>>(&self, addr: A, traversal: search::Traversal) -> Result<search::AddressSearch<'_>, search::SetupError> {
        search::AddressSearch::new(self, addr.into(), traversal)
    }

    pub fn describe_path(&self, path: structure::PathSlice) -> String {
        let mut node = &self.root;
        let mut path_description = node.props.name.clone();
        
        for i in path {
            node = &node.children[*i].node;
            
            if !sync::Arc::ptr_eq(node, &self.root) {
                path_description.push_str(".");
            }
            path_description.push_str(&node.props.name);
        }

        path_description
    }

    #[must_use]
    pub fn alter_node(&self, path: structure::Path, props: structure::Properties) -> change::Change {
        change::Change {
            ty: change::ChangeType::AlterNode { path, props },
            generation: self.generation(),
        }
    }

    #[must_use]
    pub fn alter_nodes_bulk(&self, selection: sync::Arc<selection::TreeSelection>, prop_changes: structure::MaybeProperties) -> change::Change {
        change::Change {
            ty: change::ChangeType::AlterNodesBulk { selection, prop_changes },
            generation: self.generation(),
        }
    }
    
    #[must_use]
    pub fn insert_node(&self, parent: structure::Path, index: usize, child: structure::Childhood) -> change::Change {
        change::Change {
            ty: change::ChangeType::InsertNode { parent, index, child },
            generation: self.generation(),
        }
    }

    #[must_use]
    pub fn nest(&self, range: structure::SiblingRange, extent: addr::Extent, props: structure::Properties) -> change::Change {
        change::Change {
            ty: change::ChangeType::Nest { range, extent, props },
            generation: self.generation(),
        }
    }

    #[must_use]
    pub fn destructure(&self, path: &structure::Path) -> Result<change::Change, DestructureError> {
        if path.is_empty() {
            return Err(DestructureError::AttemptToDestructureRoot);
        }

        Ok(change::Change {
            ty: change::ChangeType::Destructure {
                parent: path[0..path.len()-1].to_vec(),
                child_index: path[path.len()-1],
            },
            generation: self.generation(),
        })
    }
    
    #[must_use]
    pub fn delete_range(&self, range: structure::SiblingRange) -> change::Change {
        change::Change {
            ty: change::ChangeType::DeleteRange { range },
            generation: self.generation(),
        }
    }

    #[must_use]
    pub fn patch_byte(&self, location: u64, patch: u8) -> change::Change {
        change::Change {
            ty: change::ChangeType::StackFilter {
                filter: datapath::OverwriteFilter {
                    offset: location,
                    bytes: vec![patch]
                }.to_filter()
            },
            generation: self.generation(),
        }
    }

    #[must_use]
    pub fn insert_byte(&self, location: u64, patch: u8) -> change::Change {
        change::Change {
            ty: change::ChangeType::StackFilter {
                filter: datapath::InsertFilter {
                    offset: location,
                    bytes: vec![patch]
                }.to_filter()
            },
            generation: self.generation(),
        }
    }

    #[must_use]
    pub fn resize_node(&self, path: structure::Path, new_size: addr::Offset, expand_parents: bool, truncate_parents: bool) -> change::Change {
        change::Change {
            ty: change::ChangeType::Resize { path, new_size, expand_parents, truncate_parents },
            generation: self.generation(),
        }
    }

    #[must_use]
    pub fn paste(&self, src_node: sync::Arc<structure::Node>, src_begin: (addr::Offset, usize), src_end: (addr::Offset, usize), dst: structure::Path, dst_offset: addr::Offset, dst_index: usize) -> change::Change {
        change::Change {
            ty: change::ChangeType::Paste { src_node, src_begin, src_end, dst, dst_offset, dst_index },
            generation: self.generation(),
        }
    }

    #[must_use]
    pub fn repeat(&self, path: structure::Path, pitch: addr::Offset, count: usize, name_prefix: String, name_postfix: String, array_props: structure::Properties) -> change::Change {
        change::Change {
            ty: change::ChangeType::Repeat { path, pitch, count, name_prefix, name_postfix, props: array_props },
            generation: self.generation(),
        }
    }
}
    
#[derive(Debug, Clone, Copy)]
pub enum DestructureError {
    AttemptToDestructureRoot,
}

impl std::fmt::Debug for Document {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Document")
            .field("version", &self.version)
            .finish_non_exhaustive()
    }
}

#[cfg(test)]
mod tests {
    use super::*;    

    #[test]
    fn test_describe_path() {
        let root = structure::Node::builder()
            .name("root")
            .size(0x40)
            .child(0x10, |b| b
                   .name("child0")
                   .size(0x20))
            .child(0x14, |b| b
                   .name("child1")
                   .size(0x1c)
                   .child(0x0, |b| b
                          .name("child1:0")
                          .size(0x4))
                   .child(0x4, |b| b
                          .name("child1:1")
                          .size(0x10)))
            .child(0x20, |b| b
                   .name("child2")
                   .size(0x4))
            .build();

        let d = Builder::new(root).build();

        assert_eq!(d.describe_path(&vec![]), "root");
        assert_eq!(d.describe_path(&vec![0]), "root.child0");
        assert_eq!(d.describe_path(&vec![1]), "root.child1");
        assert_eq!(d.describe_path(&vec![1, 0]), "root.child1.child1:0");
        assert_eq!(d.describe_path(&vec![1, 1]), "root.child1.child1:1");
        assert_eq!(d.describe_path(&vec![2]), "root.child2");
    }


    #[test]
    fn test_successor() {
        let root = structure::Node::builder()
            .name("root")
            .size(0x40)
            .child(0x10, |b| b
                   .name("child0")
                   .size(0x20))
            .child(0x14, |b| b
                   .name("child1")
                   .size(0x1c)
                   .child(0x0, |b| b
                          .name("child1:0")
                          .size(0x4))
                   .child(0x4, |b| b
                          .name("child1:1")
                          .size(0x10)))
            .child(0x20, |b| b
                   .name("child2")
                   .size(0x4))
            .build();

        let d = Builder::new(root).build();
        let mut path = vec![];

        assert!(d.path_successor(&mut path));
        assert_eq!(path, [0]);
        assert!(d.path_successor(&mut path));
        assert_eq!(path, [1]);
        assert!(d.path_successor(&mut path));
        assert_eq!(path, [1, 0]);
        assert!(d.path_successor(&mut path));
        assert_eq!(path, [1, 1]);
        assert!(d.path_successor(&mut path));
        assert_eq!(path, [2]);
        assert!(!d.path_successor(&mut path));
    }
}
