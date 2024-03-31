pub mod change;
pub mod search;
pub mod structure;

use std::sync;
use std::vec;

use crate::model::addr;
use crate::model::datapath;
use crate::model::selection;
use crate::model::space::AddressSpace;
use crate::model::versioned;
use crate::model::versioned::Versioned;

#[derive(Clone)]
pub struct Document {
    pub root: sync::Arc<structure::Node>,
    pub datapath: datapath::DataPath,

    version: versioned::Version<Document>,
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

impl Document {
    pub fn new(space: sync::Arc<AddressSpace>, runtime: tokio::runtime::Handle) -> Document {
        let mut datapath = datapath::DataPath::new();
        datapath.push_back(datapath::LoadSpaceFilter::new(space, 0, 0, runtime).to_filter());
        
        Document {
            root: sync::Arc::new(structure::Node {
                size: addr::unit::MAX,
                props: structure::Properties {
                    name: "root".to_string(),
                    title_display: structure::TitleDisplay::Major,
                    children_display: structure::ChildrenDisplay::Full,
                    content_display: structure::ContentDisplay::default(),
                    locked: true,
                },
                children: vec::Vec::new()
            }),
            datapath,

            version: Default::default(),
        }
    }

    pub fn load_from_testing_structure<P: AsRef<std::path::Path>>(path: P) -> Result<Document, LoadForTestingError> {
        let xml = std::fs::read_to_string(path)?;
        let xml = roxmltree::Document::parse(&xml)?;
        let tc = crate::logic::tokenizer::xml::Testcase::from_xml(&xml);

        Ok(Document {
            root: tc.structure,
            datapath: datapath::DataPath::new(),
            version: Default::default(),
        })
    }

    #[cfg(test)]
    pub fn new_for_structure_test(root: sync::Arc<structure::Node>) -> Document {
        Document {
            root,
            datapath: datapath::DataPath::new(),
            version: Default::default(),
        }
    }
    
    pub fn invalid() -> Document {
        Document {
            root: sync::Arc::new(structure::Node {
                size: addr::unit::MAX,
                props: structure::Properties {
                    name: "root".to_string(),
                    title_display: structure::TitleDisplay::Major,
                    children_display: structure::ChildrenDisplay::Full,
                    content_display: structure::ContentDisplay::default(),
                    locked: true,
                },
                children: vec::Vec::new()
            }),
            datapath: datapath::DataPath::new(),
            version: Default::default(),
        }
    }

    pub fn lookup_node(&self, path: structure::PathSlice) -> (&sync::Arc<structure::Node>, addr::Address) {
        let mut current_node = &self.root;
        let mut node_addr = addr::unit::NULL;

        for i in path {
            let childhood = &current_node.children[*i];
            node_addr+= childhood.offset.to_size();
            current_node = &childhood.node;
        }

        (current_node, node_addr)
    }

    pub fn search_addr<A: Into<addr::Address>>(&self, addr: A, traversal: search::Traversal) -> Result<search::AddressSearch<'_>, search::SetupError> {
        search::AddressSearch::new(self, addr.into(), traversal)
    }

    pub fn describe_path(&self, path: &structure::Path) -> String {
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

        let (parent_node, _) = self.lookup_node(&path[0..path.len()-1]);
        let childhood = &parent_node.children[path[path.len()-1]];
        
        Ok(change::Change {
            ty: change::ChangeType::Destructure {
                parent: path[0..path.len()-1].to_vec(),
                child_index: path[path.len()-1],
                num_grandchildren: childhood.node.children.len(),
                offset: childhood.offset
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
}

    /*
    pub fn patch_byte(&self, location: u64, patch: u8) {
        self.apply_filter(datapath::OverwriteFilter {
            offset: location,
            bytes: vec![patch]
        }.to_filter())
    }

    pub fn insert_byte(&self, location: u64, patch: u8) {
        self.apply_filter(datapath::InsertFilter {
            offset: location,
            bytes: vec![patch]
        }.to_filter())
    }

    pub fn apply_filter(&self, filter: datapath::Filter) {
        let mut interior = self.interior.write();

        let last_undo = interior.undo.last_mut();
        
        /* Try to stack this filter with the most recent one. */
        let stack = match last_undo {
            /* If the last undo operation was a filter, try to stack with it. */
            Some((UndoOp::AddFilter(ref mut undo_filter), ref undo_doc)) => datapath::Filter::stack(undo_filter, &filter).map(|se| (undo_filter, undo_doc, se)).ok_or(filter),
            _ => Err(filter)
        };

        match stack {
            /* If we successfully stacked, replace the undo op's filter with the
             * new stacked version, revert the document, and apply the new
             * stacked filter to the document in place of the old filter. */
            Ok((undo_filter, undo_doc, stacked_filter)) => {
                *undo_filter = stacked_filter.clone();
                interior.current = undo_doc.clone();
                interior.current.datapath.push_back(stacked_filter);
            },
            
            /* Stacking failed. Just add a new undo op. */
            Err(filter) => {
                let old_doc = interior.current.clone();
                interior.undo.push((UndoOp::AddFilter(filter.clone()), old_doc));
                interior.current.datapath.push_back(filter);
            }
        }

        interior.current.datapath_generation = NEXT_DATAPATH_GENERATION.fetch_add(1, sync::atomic::Ordering::SeqCst);
        
        self.notifier.notify();
}
     */

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

        let d = Document::new_for_structure_test(root);

        assert_eq!(d.describe_path(&vec![]), "root");
        assert_eq!(d.describe_path(&vec![0]), "root.child0");
        assert_eq!(d.describe_path(&vec![1]), "root.child1");
        assert_eq!(d.describe_path(&vec![1, 0]), "root.child1.child1:0");
        assert_eq!(d.describe_path(&vec![1, 1]), "root.child1.child1:1");
        assert_eq!(d.describe_path(&vec![2]), "root.child2");
    }
}
