use std::sync;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::versioned;
use crate::model::versioned::Versioned;

#[derive(Debug, Clone)]
pub enum ChangeType {
    /// Modifies the properties of a node, but doesn't affect its children.
    AlterNode {
        path: structure::Path,
        props: structure::Properties
    },

    /// Inserts the node as a child of the node referred to by the given path.
    InsertNode {
        parent: structure::Path,
        index: usize,
        child: structure::Childhood
    },

    /// Wraps a node's children (range inclusive) in a new node.
    Nest {
        parent: structure::Path,
        first_child: usize,
        last_child: usize,
        extent: addr::Extent,
        props: structure::Properties
    },

    /// Deletes a node, letting its children be inherited by their grandparent.
    Destructure {
        parent: structure::Path,
        child_index: usize,

        /* This information is here so that update_path and such can have the
         * information they need without having to look up nodes in the old
         * document. */
        num_grandchildren: usize,
        offset: addr::Address
    },

    /// Deletes some of a node's (range inclusive) children.
    DeleteRange {
        parent: structure::Path,
        first_child: usize,
        last_child: usize
    },
}

#[derive(Debug, Clone)]
pub struct Change {
    pub ty: ChangeType,
    pub generation: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UpdatePathResult {
    /// The node indicated by the provided path wasn't moved by this change.
    Unmoved,

    /// The node indicated by the provided path was moved somewhere else.
    Moved,

    /// The node indicated by the provided path was deleted. You're getting the path of the parent node instead.
    Deleted,

    /// The node indicated by the provided path was destructured. You're getting the path of the parent node instead.
    Destructured,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UpdateError {
    NoCommonAncestor,
    NotUpdatable,
    NodeDeleted,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ApplyError {
    UpdateFailed(UpdateError),
    InvalidParameters(&'static str),
}

impl Change {
    #[must_use]
    pub fn update_path(&self, path: &mut structure::Path) -> UpdatePathResult {
        match &self.ty {
            ChangeType::AlterNode { .. } => UpdatePathResult::Unmoved,
            ChangeType::InsertNode { parent, index: affected_index, child: _ } => {
                if path.len() > parent.len() && path[0..parent.len()] == parent[..] {
                    let path_index = &mut path[parent.len()];
                    
                    if *path_index >= *affected_index {
                        *path_index+= 1;
                        UpdatePathResult::Moved
                    } else {
                        UpdatePathResult::Unmoved
                    }
                } else {
                    UpdatePathResult::Unmoved
                }
            },
            ChangeType::Nest { parent, first_child, last_child, extent: _, props: _ } => {
                if path.len() > parent.len() && &path[0..parent.len()] == &parent[..] {
                    let child_index = &mut path[parent.len()];
                    
                    if *child_index >= *first_child && *child_index <= *last_child {
                        *child_index-= first_child;
                        path.insert(parent.len(), *first_child);
                        UpdatePathResult::Moved
                    } else if *child_index > *last_child {
                        *child_index-= last_child-first_child;
                        UpdatePathResult::Moved
                    } else {
                        UpdatePathResult::Unmoved
                    }
                } else {
                    UpdatePathResult::Unmoved
                }
            },
            ChangeType::Destructure { parent, child_index, num_grandchildren, offset: _ } => {
                if path.len() > parent.len() && &path[0..parent.len()] == &parent[..] {
                    if path[parent.len()] < *child_index {
                        /* Path was pointing to a sibling of the node being destructured that comes before it */
                        UpdatePathResult::Unmoved
                    } else if path[parent.len()] == *child_index {
                        if path.len() == parent.len() + 1 {
                            /* Path was pointing to the node that got deleted. */
                            path.pop();
                            UpdatePathResult::Destructured
                        } else {
                            /* Path was pointing to a descendant. */

                            /* Check that num_grandchildren wasn't obviously wrong. */
                            assert!(path[parent.len()+1] < *num_grandchildren);

                            /* Delete the node that got destructured from the path. */
                            path.remove(parent.len());
                          
                            /* Shift the index of the grandchild. */
                            path[parent.len()]+= child_index;
                            
                            UpdatePathResult::Moved
                        }
                    } else {
                        /* Path was pointing to a sibling of the node being destructured that comes after it */
                        path[parent.len()]+= num_grandchildren - 1;
                        UpdatePathResult::Moved
                    }
                } else {
                    /* Path is unrelated */
                    UpdatePathResult::Unmoved
                }
            },
            ChangeType::DeleteRange { parent, first_child, last_child } => {
                if path.len() > parent.len() && &path[0..parent.len()] == &parent[..] {
                    let child_index = &mut path[parent.len()];
                    
                    if *child_index >= *first_child && *child_index <= *last_child {
                        path.truncate(parent.len());

                        UpdatePathResult::Deleted
                    } else if *child_index > *last_child {
                        *child_index-= last_child-first_child+1;

                        UpdatePathResult::Moved
                    } else {
                        UpdatePathResult::Unmoved
                    }
                } else {
                    UpdatePathResult::Unmoved
                }
            },
        }
    }

    pub fn rebase(mut self, to: &document::Document) -> Result<Self, UpdateError> {
        if self.generation == to.generation() {
            return Ok(self)
        }

        assert!(self.generation < to.generation());
        
        match &to.previous() {
            Some((prev_document, doc_change)) => {
                self = self.rebase(prev_document)?;

                Ok(Change {
                    ty: match self.ty {
                        ChangeType::AlterNode { mut path, props } => ChangeType::AlterNode { path: match doc_change.update_path(&mut path) {
                            UpdatePathResult::Unmoved | UpdatePathResult::Moved => path,
                            UpdatePathResult::Deleted | UpdatePathResult::Destructured => return Err(UpdateError::NodeDeleted),
                        }, props },
                        ChangeType::InsertNode { .. } => return Err(UpdateError::NotUpdatable),
                        ChangeType::Nest { .. } => return Err(UpdateError::NotUpdatable),
                        ChangeType::Destructure { .. } => return Err(UpdateError::NotUpdatable),
                        ChangeType::DeleteRange { .. } => return Err(UpdateError::NotUpdatable),
                    },
                    generation: to.generation()
                })
            }, None => Err(UpdateError::NoCommonAncestor)
        }
    }
}

fn rebuild_node_tree<F, Iter: std::iter::Iterator<Item = usize>>(target: &structure::Node, mut path_segment: Iter, target_modifier: F) -> Result<structure::Node, ApplyError> where
    F: FnOnce(&mut structure::Node) -> Result<(), ApplyError> {
    match path_segment.next() {
        Some(index) => {
            /* Recurse to rebuild the child, then rebuild the target with the new child. */
            let child = &*target.children[index].node;
            let new_child = rebuild_node_tree(child, path_segment, target_modifier)?;
            let mut new_target = (*target).clone();
            new_target.children[index].node = sync::Arc::new(new_child);
            Ok(new_target)
        },
        None => {
            /* Reached the end of the path. Just modify this node directly. */
            let mut new_target = (*target).clone();
            target_modifier(&mut new_target)?;
            Ok(new_target)
        }
    }
}

impl versioned::Change<document::Document> for Change {
    type ApplyError = ApplyError;
    type ApplyRecord = Self;
    
    fn apply(mut self, document: &mut document::Document) -> Result<(Change, Change), ApplyError> {
        self = self.rebase(document).map_err(ApplyError::UpdateFailed)?;

        assert_eq!(self.generation, document.generation());

        match &self.ty {
            ChangeType::AlterNode { path, props } => document.root = sync::Arc::new(rebuild_node_tree(&document.root, path.iter().cloned(), |target| {
                target.props = props.clone();
                Ok(())
            })?),
            ChangeType::InsertNode { parent: path, index: at_child, child: childhood } => document.root = sync::Arc::new(rebuild_node_tree(&document.root, path.iter().cloned(), |target| {
                /* Check at_child to make sure it's not farther than one-past the end. */
                if *at_child > target.children.len() {
                    return Err(ApplyError::InvalidParameters("attempted to insert node at out-of-bounds index"));
                }

                /* Check offset to make sure it's within bounds. */
                // TODO: automatically grow parents?
                if childhood.offset > target.size.to_addr() {
                    return Err(ApplyError::InvalidParameters("attempted to insert node beginning beyond parent's size"));
                }

                /* Check child size to make sure it's within bounds. */
                // TODO: automatically grow parents?
                if childhood.offset + childhood.node.size > target.size.to_addr() {
                    return Err(ApplyError::InvalidParameters("attempted to insert node extending beyond parent's size"));
                }
                
                /* Keep child offsets monotonic. */
                if (*at_child > 0 && target.children[at_child-1].offset > childhood.offset) || (*at_child < target.children.len() && target.children[*at_child].offset < childhood.offset) {
                    println!("rejecting insert at position {}, offset {} into children {:?}", at_child, childhood.offset, target.children);
                    return Err(ApplyError::InvalidParameters("attempted to insert node at an index that would break offset monotonicity"));
                }

                /* Preconditions passed; do the deed. */
                target.children.insert(*at_child, childhood.clone());

                Ok(())
            })?),
            ChangeType::Nest { parent, first_child, last_child, extent, props } => document.root = sync::Arc::new(rebuild_node_tree(&document.root, parent.iter().cloned(), |parent_node| {
                /* Check indices. */
                if *first_child >= parent_node.children.len() || *last_child >= parent_node.children.len() {
                    return Err(ApplyError::InvalidParameters("attempted to nest children that don't exist"));
                }

                if last_child < first_child {
                    return Err(ApplyError::InvalidParameters("last child was before first child"));
                }

                /* Check that children are all contained within the extent */
                let children_extent = addr::Extent::between(parent_node.children[*first_child].offset, parent_node.children[*last_child].end());
                if !extent.contains(children_extent) {
                    return Err(ApplyError::InvalidParameters("data extent does not contain all nested children"));
                }

                /* Preconditions passed; do the deed. */
                let mut children: Vec<structure::Childhood> = parent_node.children.splice(first_child..=last_child, [structure::Childhood::default()]).collect();

                for child in &mut children {
                    child.offset-= extent.begin.to_size();
                }

                let new_node = &mut parent_node.children[*first_child];
                new_node.offset = extent.begin;
                new_node.node = sync::Arc::new(structure::Node {
                    size: extent.length(),
                    children: children,
                    props: props.clone(),
                });

                Ok(())
            })?),
            ChangeType::Destructure { parent, child_index, num_grandchildren, offset } => document.root = sync::Arc::new(rebuild_node_tree(&document.root, parent.iter().cloned(), |parent_node| {
                /* Check that we're trying to destructure a child that actually exists. */
                if *child_index >= parent_node.children.len() {
                    return Err(ApplyError::InvalidParameters("attemped to destructure child that doesn't exist"));
                }

                /* Check that num_grandchildren is correct. */
                if *num_grandchildren != parent_node.children[*child_index].node.children.len() {
                    return Err(ApplyError::InvalidParameters("num_grandchildren was wrong when attempting to destructure node"));
                }

                /* Check that offset is correct. */
                if *offset != parent_node.children[*child_index].offset {
                    return Err(ApplyError::InvalidParameters("offset was wrong when attempting to destructure node"));
                }

                /* Preconditions passed; do the deed. */
                let destructured_child = parent_node.children.remove(*child_index);
                
                parent_node.children.splice(*child_index..*child_index, destructured_child.node.children.iter().map(|childhood| {
                    /* Don't forget to shift offsets. */
                    let mut childhood = childhood.clone();
                    childhood.offset+= offset.to_size();
                    childhood
                }));

                Ok(())
            })?),
            ChangeType::DeleteRange { parent, first_child, last_child } => document.root = sync::Arc::new(rebuild_node_tree(&document.root, parent.iter().cloned(), |parent_node| {
                /* Check indices. */
                if *first_child >= parent_node.children.len() || *last_child >= parent_node.children.len() {
                    return Err(ApplyError::InvalidParameters("attempted to delete children that don't exist"));
                }

                if last_child < first_child {
                    return Err(ApplyError::InvalidParameters("last child was before first child"));
                }

                /* Preconditions passed; do the deed. */
                parent_node.children.splice(first_child..=last_child, []);

                Ok(())
            })?),
        }

        Ok((self.clone(), self))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use assert_matches::assert_matches;

    use crate::model::versioned::Change as VersionedChange;
    
    #[test]
    fn test_update_path_through_alter_node() {
        let mut path = vec![1, 0, 2];

        assert_eq!(Change {
            ty: ChangeType::AlterNode { path: vec![1, 0], props: structure::Properties::default() },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Unmoved);
        
        assert_eq!(path, vec![1, 0, 2]);
    }

    #[test]
    fn test_update_path_through_insert_node() {
        let mut path = vec![1, 0, 2];

        assert_eq!(Change {
            ty: ChangeType::InsertNode { parent: vec![1, 0], index: 1, child: structure::Node::builder().build_child(addr::unit::NULL) },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Moved);
        
        assert_eq!(path, vec![1, 0, 3]);

        assert_eq!(Change {
            ty: ChangeType::InsertNode { parent: vec![1, 0], index: 4, child: structure::Node::builder().build_child(addr::unit::NULL) },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Unmoved);

        assert_eq!(path, vec![1, 0, 3]);

        assert_eq!(Change {
            ty: ChangeType::InsertNode { parent: vec![1, 0], index: 3, child: structure::Node::builder().build_child(addr::unit::NULL) },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Moved);

        assert_eq!(path, vec![1, 0, 4]);

        assert_eq!(Change {
            ty: ChangeType::InsertNode { parent: vec![1, 1], index: 0, child: structure::Node::builder().build_child(addr::unit::NULL) },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Unmoved);

        assert_eq!(path, vec![1, 0, 4]);
    }

    #[test]
    fn test_update_path_through_nest() {
        let extent = addr::Extent::between(addr::unit::NULL, addr::unit::NULL);
        
        /* siblings before but not including the path */
        let mut path = vec![1, 0, 2];
        assert_eq!(Change {
            ty: ChangeType::Nest { parent: vec![1, 0], first_child: 0, last_child: 1, extent, props: structure::Properties::default() },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 0, 1]);

        /* siblings after but not including the path */
        let mut path = vec![1, 0, 2];
        assert_eq!(Change {
            ty: ChangeType::Nest { parent: vec![1, 0], first_child: 3, last_child: 10, extent, props: structure::Properties::default() },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Unmoved);
        assert_eq!(path, vec![1, 0, 2]);

        /* including the path */
        let mut path = vec![1, 0, 5];
        assert_eq!(Change {
            ty: ChangeType::Nest { parent: vec![1, 0], first_child: 3, last_child: 10, extent, props: structure::Properties::default() },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 0, 3, 2]);

        /* ancestor of the path */
        let mut path = vec![1, 0, 5, 4];
        assert_eq!(Change {
            ty: ChangeType::Nest { parent: vec![1, 0], first_child: 3, last_child: 10, extent, props: structure::Properties::default() },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 0, 3, 2, 4]);        

        /* unrelated path */
        let mut path = vec![1, 0, 5, 4];
        assert_eq!(Change {
            ty: ChangeType::Nest { parent: vec![1, 1], first_child: 3, last_child: 10, extent, props: structure::Properties::default() },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Unmoved);
        assert_eq!(path, vec![1, 0, 5, 4]);
    }

    #[test]
    fn test_update_path_through_destructure() {
        /* sibling of destructured node (before) */
        let mut path = vec![1, 0];
        assert_eq!(Change {
            ty: ChangeType::Destructure { parent: vec![1], child_index: 1, num_grandchildren: 3, offset: addr::unit::NULL },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Unmoved);
        assert_eq!(path, vec![1, 0]);

        /* descendant of sibling of destructured node (before) */
        let mut path = vec![1, 0, 6, 7];
        assert_eq!(Change {
            ty: ChangeType::Destructure { parent: vec![1], child_index: 1, num_grandchildren: 3, offset: addr::unit::NULL },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Unmoved);
        assert_eq!(path, vec![1, 0, 6, 7]);
        
        /* sibling of destructured node (after) */
        let mut path = vec![1, 2];
        assert_eq!(Change {
            ty: ChangeType::Destructure { parent: vec![1], child_index: 1, num_grandchildren: 3, offset: addr::unit::NULL },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Moved);
        /* 1 before: [0, [0, 1, 2], 2, 3, ...] */
        /* 1  after: [0, old 0, old 1, old 2, 2, 3, ...] */
        assert_eq!(path, vec![1, 4]);

        /* descendant of sibling of destructured node (after) */
        let mut path = vec![1, 2, 6, 7];
        assert_eq!(Change {
            ty: ChangeType::Destructure { parent: vec![1], child_index: 1, num_grandchildren: 3, offset: addr::unit::NULL },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 4, 6, 7]);
        
        /* destructured node */
        let mut path = vec![1, 1];
        assert_eq!(Change {
            ty: ChangeType::Destructure { parent: vec![1], child_index: 1, num_grandchildren: 3, offset: addr::unit::NULL },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Destructured);
        assert_eq!(path, vec![1]);

        /* descendant of destructured node */
        let mut path = vec![1, 1, 2, 4];
        assert_eq!(Change {
            ty: ChangeType::Destructure { parent: vec![1], child_index: 1, num_grandchildren: 3, offset: addr::unit::NULL },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 3, 4]);

        /* unrelated path */
        let mut path = vec![2, 2, 6, 7];
        assert_eq!(Change {
            ty: ChangeType::Destructure { parent: vec![1], child_index: 1, num_grandchildren: 3, offset: addr::unit::NULL },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Unmoved);
        assert_eq!(path, vec![2, 2, 6, 7]);
    }

    #[test]
    fn test_update_path_through_delete_range() {
        /* siblings before but not including the path */
        let mut path = vec![1, 0, 2];
        assert_eq!(Change {
            ty: ChangeType::DeleteRange { parent: vec![1, 0], first_child: 0, last_child: 1 },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 0, 0]);

        /* siblings after but not including the path */
        let mut path = vec![1, 0, 2];
        assert_eq!(Change {
            ty: ChangeType::DeleteRange { parent: vec![1, 0], first_child: 3, last_child: 10 },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Unmoved);
        assert_eq!(path, vec![1, 0, 2]);

        /* including the path */
        let mut path = vec![1, 0, 5];
        assert_eq!(Change {
            ty: ChangeType::DeleteRange { parent: vec![1, 0], first_child: 3, last_child: 10 },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Deleted);
        /* don't care what the path is */

        /* ancestor of the path */
        let mut path = vec![1, 0, 5, 4];
        assert_eq!(Change {
            ty: ChangeType::DeleteRange { parent: vec![1, 0], first_child: 3, last_child: 10 },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Deleted);
        /* don't care what the path is */

        /* unrelated path */
        let mut path = vec![1, 0, 5, 4];
        assert_eq!(Change {
            ty: ChangeType::DeleteRange { parent: vec![1, 1], first_child: 3, last_child: 10 },
            generation: 0,
        }.update_path(&mut path), UpdatePathResult::Unmoved);
        assert_eq!(path, vec![1, 0, 5, 4]);
    }

    /* This exists to produce errors if another ChangeType gets added without corresponding tests. */
    fn update_path_exhaustiveness(ty: ChangeType) {
        match ty {
            ChangeType::AlterNode { .. } => test_update_path_through_alter_node(),
            ChangeType::InsertNode { .. } => test_update_path_through_insert_node(),
            ChangeType::Nest { .. } => test_update_path_through_nest(),
            ChangeType::Destructure { .. } => test_update_path_through_destructure(),
            ChangeType::DeleteRange { .. } => test_update_path_through_delete_range(),
            /* Make tests for your new ChangeType! */
        }
    }
    
    fn create_test_document_1() -> document::Document {
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
                          .name("child1.0")
                          .size(0x4))
                   .child(0x4, |b| b
                          .name("child1.1")
                          .size(0x10)))
            .child(0x20, |b| b
                   .name("child2")
                   .size(0x4))
            .build();

        document::Document::new_for_structure_test(root)
    }

    fn create_test_document_2() -> document::Document {
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
                          .name("child1.0")
                          .size(0x4))
                   .child(0x4, |b| b
                          .name("child1.1")
                          .size(0x10))
                   .child(0x8, |b| b
                          .name("child1.2")
                          .size(0x10))
                   .child(0xc, |b| b
                          .name("child1.3")
                          .size(0x10)))
            .child(0x20, |b| b
                   .name("child2")
                   .size(0x4))
            .build();

        document::Document::new_for_structure_test(root)
    }

    #[test]
    fn test_structural_change_alter_node() {
        let mut doc = create_test_document_1();
        
        let mut props = doc.lookup_node(&vec![1, 0]).0.props.clone();
        assert_eq!(props.name, "child1.0");

        props.name = "modified".to_string();
        
        Change {
            ty: ChangeType::AlterNode { path: vec![1, 0], props },
            generation: doc.generation(),
        }.apply(&mut doc).unwrap();

        assert_eq!(doc.lookup_node(&vec![1, 0]).0.props.name, "modified");
    }

    #[test]
    fn test_structural_change_insert_node_preconditions() {
        let doc = create_test_document_1();

        let builder = structure::Node::builder().size(0x10);

        {
            let mut doc = doc.clone();
            assert_matches!(Change {
                ty: ChangeType::InsertNode { parent: vec![], index: 4, child: builder.build_child(addr::Address::from(0x100)) },
                generation: doc.generation(),
            }.apply(&mut doc), Err(ApplyError::InvalidParameters("attempted to insert node at out-of-bounds index")));
            assert!(doc.lookup_node(&vec![0]).0.size < 0x21.into());
        }

        {
            let mut doc = doc.clone();
            assert_matches!(Change {
                ty: ChangeType::InsertNode { parent: vec![0], index: 0, child: builder.build_child(addr::Address::from(0x21)) },
                generation: doc.generation(),
            }.apply(&mut doc), Err(ApplyError::InvalidParameters("attempted to insert node beginning beyond parent's size")));
            assert_eq!(doc.lookup_node(&vec![0]).0.size, 0x20.into());
        }

        {
            let mut doc = doc.clone();
            assert_matches!(Change {
                ty: ChangeType::InsertNode { parent: vec![0], index: 0, child: builder.build_child(addr::Address::from(0x11)) },
                generation: doc.generation(),
            }.apply(&mut doc), Err(ApplyError::InvalidParameters("attempted to insert node extending beyond parent's size")));
            assert_eq!(doc.lookup_node(&vec![1, 1]).1, addr::Address::from(0x18));
        }

        {
            let mut doc = doc.clone();
            assert_matches!(Change {
                ty: ChangeType::InsertNode { parent: vec![1], index: 0, child: builder.build_child(addr::Address::from(0x2)) },
                generation: doc.generation(),
            }.apply(&mut doc), Err(ApplyError::InvalidParameters("attempted to insert node at an index that would break offset monotonicity")));
        }

        {
            let mut doc = doc.clone();
            assert_matches!(Change {
                ty: ChangeType::InsertNode { parent: vec![1], index: 2, child: builder.build_child(addr::Address::from(0x2)) },
                generation: doc.generation(),
            }.apply(&mut doc), Err(ApplyError::InvalidParameters("attempted to insert node at an index that would break offset monotonicity")));
        }
    }
    
    #[test]
    fn test_structural_change_insert_node() {
        let mut doc = create_test_document_1();

        let new_child = structure::Node::builder().size(0x10).build_child(addr::Address::from(0x2));

        let original_child = doc.lookup_node(&vec![1, 1]).0.clone();
        
        Change {
            ty: ChangeType::InsertNode { parent: vec![1], index: 1, child: new_child.clone() },
            generation: doc.generation(),
        }.apply(&mut doc).unwrap();

        assert!(sync::Arc::ptr_eq(&original_child, doc.lookup_node(&vec![1, 2]).0));
        assert!(sync::Arc::ptr_eq(&new_child.node, doc.lookup_node(&vec![1, 1]).0));
    }

    #[test]
    fn test_structural_change_nest_preconditions() {
        let doc = create_test_document_1();

        assert_matches!(Change {
            ty: ChangeType::Nest { parent: vec![1], first_child: 0, last_child: 2, extent: addr::Extent::between(0x0, 0x20), props: structure::Properties::default() },
            generation: doc.generation(),
        }.apply(&mut doc.clone()), Err(ApplyError::InvalidParameters("attempted to nest children that don't exist")));

        assert_matches!(Change {
            ty: ChangeType::Nest { parent: vec![1], first_child: 1, last_child: 0, extent: addr::Extent::between(0x0, 0x20), props: structure::Properties::default() },
            generation: doc.generation(),
        }.apply(&mut doc.clone()), Err(ApplyError::InvalidParameters("last child was before first child")));

        assert_matches!(Change {
            ty: ChangeType::Nest { parent: vec![1], first_child: 1, last_child: 1, extent: addr::Extent::between(0x5, 0x20), props: structure::Properties::default() },
            generation: doc.generation(),
        }.apply(&mut doc.clone()), Err(ApplyError::InvalidParameters("data extent does not contain all nested children")));

        assert_matches!(Change {
            ty: ChangeType::Nest { parent: vec![1], first_child: 0, last_child: 1, extent: addr::Extent::between(0x0, 0x13), props: structure::Properties::default() },
            generation: doc.generation(),
        }.apply(&mut doc.clone()), Err(ApplyError::InvalidParameters("data extent does not contain all nested children")));
    }

    #[test]
    fn test_structural_change_nest() {
        let doc = create_test_document_2();

        let orig_child1 = doc.root.children[1].node.clone();
        let mut props = structure::Properties::default();
        props.name = "nest".to_string();

        let mut new_doc = doc.clone();
        Change {
            ty: ChangeType::Nest { parent: vec![1], first_child: 1, last_child: 2, extent: addr::Extent::between(0x2, 0x18), props: structure::Properties::default() },
            generation: doc.generation(),
        }.apply(&mut new_doc).unwrap();

        let new_child1 = new_doc.root.children[1].node.clone();

        /* Root's other children should be unchanged. */
        {
            assert!(sync::Arc::ptr_eq(&doc.root.children[0].node, &new_doc.root.children[0].node));
            assert!(sync::Arc::ptr_eq(&doc.root.children[2].node, &new_doc.root.children[2].node));
        }

        /* Child1's non-nested children should be unchanged, aside from changing index. */
        {
            assert!(sync::Arc::ptr_eq(&orig_child1.children[0].node, &new_child1.children[0].node));
            assert_eq!(orig_child1.children[0].offset, new_child1.children[0].offset);

            assert!(sync::Arc::ptr_eq(&orig_child1.children[3].node, &new_child1.children[2].node));
            assert_eq!(orig_child1.children[3].offset, new_child1.children[2].offset);
        }

        let nest_child = &new_child1.children[1];
        assert_eq!(nest_child.offset, 0x2.into()); /* this should come straight from the extent */
        assert_eq!(nest_child.node.size, 0x16.into()); /* this should come straight from the extent */
        assert_eq!(nest_child.node.children.len(), 2);
        assert_eq!(nest_child.node.children[0].offset, 0x2.into());
        assert!(sync::Arc::ptr_eq(&nest_child.node.children[0].node, &orig_child1.children[1].node));
        assert_eq!(nest_child.node.children[1].offset, 0x6.into());
        assert!(sync::Arc::ptr_eq(&nest_child.node.children[1].node, &orig_child1.children[2].node));
    }

    #[test]
    fn test_structural_change_destructure_preconditions() {
        let doc = create_test_document_2();

        assert_matches!(Change {
            ty: ChangeType::Destructure { parent: vec![], child_index: 1, num_grandchildren: 4, offset: 0x13.into() },
            generation: doc.generation(),
        }.apply(&mut doc.clone()), Err(ApplyError::InvalidParameters("offset was wrong when attempting to destructure node")));

        assert_matches!(Change {
            ty: ChangeType::Destructure { parent: vec![], child_index: 1, num_grandchildren: 5, offset: 0x14.into() },
            generation: doc.generation(),
        }.apply(&mut doc.clone()), Err(ApplyError::InvalidParameters("num_grandchildren was wrong when attempting to destructure node")));

        assert_matches!(Change {
            ty: ChangeType::Destructure { parent: vec![], child_index: 30, num_grandchildren: 5, offset: 0x14.into() },
            generation: doc.generation(),
        }.apply(&mut doc.clone()), Err(ApplyError::InvalidParameters("attemped to destructure child that doesn't exist")));
    }
    
    #[test]
    fn test_structural_change_destructure() {
        let doc = create_test_document_2();
        let orig_child1 = doc.root.children[1].node.clone();
        
        let mut new_doc = doc.clone();
        Change {
            ty: ChangeType::Destructure { parent: vec![], child_index: 1, num_grandchildren: 4, offset: 0x14.into() },
            generation: doc.generation(),
        }.apply(&mut new_doc).unwrap();

        /* Root's other children should be unchanged. */
        {
            assert!(sync::Arc::ptr_eq(&doc.root.children[0].node, &new_doc.root.children[0].node));
            assert!(sync::Arc::ptr_eq(&doc.root.children[2].node, &new_doc.root.children[5].node));
        }

        /* Destructured node's children should be moved to root and have offsets adjusted, but still point to the same objects. */
        for i in 0..4 {
            assert_eq!(orig_child1.children[i].offset + 0x14, new_doc.root.children[i+1].offset);
            assert!(sync::Arc::ptr_eq(&orig_child1.children[i].node, &new_doc.root.children[i+1].node));
        }
        /* Sanity check */
        assert_eq!(new_doc.root.children[3].offset, 0x1c.into());
    }
    
    #[test]
    fn test_structural_change_delete_range() {
        let doc = create_test_document_2();

        /* try deleting just child1 */
        let mut new_doc = doc.clone();
        Change {
            ty: ChangeType::DeleteRange { parent: vec![], first_child: 1, last_child: 1 },
            generation: doc.generation(),
        }.apply(&mut new_doc).unwrap();

        assert_eq!(new_doc.root.children.len(), 2);
        assert!(sync::Arc::ptr_eq(&doc.root.children[0].node, &new_doc.root.children[0].node));
        assert!(sync::Arc::ptr_eq(&doc.root.children[2].node, &new_doc.root.children[1].node));

        /* try deleting child1.1 through child1.2 */
        let mut new_doc = doc.clone();
        Change {
            ty: ChangeType::DeleteRange { parent: vec![1], first_child: 1, last_child: 2 },
            generation: doc.generation(),
        }.apply(&mut new_doc).unwrap();

        assert_eq!(new_doc.root.children.len(), 3);
        assert!(sync::Arc::ptr_eq(&doc.root.children[0].node, &new_doc.root.children[0].node));
        assert!(sync::Arc::ptr_eq(&doc.root.children[2].node, &new_doc.root.children[2].node));

        let orig_child1 = doc.root.children[1].node.clone();
        let new_child1 = new_doc.root.children[1].node.clone();
        assert_eq!(new_child1.children.len(), 2);
        assert!(sync::Arc::ptr_eq(&orig_child1.children[0].node, &new_child1.children[0].node));
        assert!(sync::Arc::ptr_eq(&orig_child1.children[3].node, &new_child1.children[1].node));
    }

    /* This exists to produce errors if another ChangeType gets added without corresponding tests. */
    fn structural_change_exhaustiveness(ty: ChangeType) {
        match ty {
            ChangeType::AlterNode { .. } => test_structural_change_alter_node(),
            ChangeType::InsertNode { .. } => test_structural_change_insert_node(),
            ChangeType::Nest { .. } => test_structural_change_nest(),
            ChangeType::Destructure { .. } => test_structural_change_destructure(),
            ChangeType::DeleteRange { .. } => test_structural_change_delete_range(),
            /* Make tests for your new ChangeType! */
        }
    }
}
