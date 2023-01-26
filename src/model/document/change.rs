use std::sync;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;

#[derive(Debug, Clone)]
pub enum ChangeType {
    /// Modifies the properties of a node, but doesn't affect its children.
    AlterNode(structure::Path, structure::Properties),

    /// Inserts the node as a child of the node referred to by the given path.
    InsertNode(structure::Path, usize, addr::Address, sync::Arc<structure::Node>),

    /// Wraps a node's children (range inclusive) in a new node.
    Nest(structure::Path, usize, usize, structure::Properties),
}

#[derive(Debug, Clone)]
pub struct Change {
    pub ty: ChangeType,
    pub generation: u64,
}

#[derive(Debug, Clone)]
pub enum UpdateError {
    NoCommonAncestor,
    NotUpdatable,
}

#[derive(Debug, Clone)]
pub enum ApplyError {
    UpdateFailed(UpdateError),
    InvalidParameters,
}

pub fn update_path(path: &mut structure::Path, through: &Change) -> Result<(), UpdateError> {
    match &through.ty {
        ChangeType::AlterNode(_, _) => Ok(()),
        ChangeType::InsertNode(affected_path, affected_index, _new_node_offset, _new_node) => {
            if path.len() > affected_path.len() && path[0..affected_path.len()] == affected_path[..] {
                let path_index = &mut path[affected_path.len()];
                    
                if *path_index >= *affected_index {
                    *path_index+= 1;
                }
            }

            Ok(())
        },
        ChangeType::Nest(parent, first_child, last_child, _props) => {
            if path.len() > parent.len() && &path[0..parent.len()] == &parent[..] {
                let child_index = &mut path[parent.len()];
                
                if *child_index >= *first_child && *child_index <= *last_child {
                    *child_index-= first_child;
                    drop(child_index);
                    path.insert(parent.len(), *first_child);
                } else if *child_index > *last_child {
                    *child_index-= last_child-first_child;
                }
            }

            Ok(())
        },
    }
}

pub fn update_change(change: Change, to: &document::Document) -> Result<Change, UpdateError> {
    if change.generation == to.generation {
        return Ok(change)
    }

    assert!(change.generation < to.generation);
    
    match &to.previous {
        Some((prev_document, doc_change)) => {
            let change = update_change(change, prev_document)?;

            Ok(Change {
                ty: match change.ty {
                    ChangeType::AlterNode(mut path, props) => ChangeType::AlterNode({ update_path(&mut path, doc_change)?; path }, props),
                    ChangeType::InsertNode(_, _, _, _) => return Err(UpdateError::NotUpdatable),
                    ChangeType::Nest(_, _, _, _) => return Err(UpdateError::NotUpdatable),
                },
                generation: to.generation
            })
        }, None => Err(UpdateError::NoCommonAncestor)
    }
}

fn rebuild_node_tree<F>(target: &structure::Node, mut path_segment: structure::PathIter, target_modifier: F) -> Result<structure::Node, ApplyError> where
    F: FnOnce(&mut structure::Node) -> Result<(), ApplyError> {
    let mut new_target = (*target).clone();
    match path_segment.next() {
        Some(index) => {
            /* Recurse to rebuild the child, then rebuild the target with the new child. */
            let child = &*target.children[index].node;
            let new_child = rebuild_node_tree(child, path_segment, target_modifier)?;
            new_target.children[index].node = sync::Arc::new(new_child);
        },
        None => {
            /* Reached the end of the path. Just modify this node directly. */
            target_modifier(&mut new_target)?
        }
    }
    Ok(new_target)
}

pub fn apply_structural_change(document: &sync::Arc<document::Document>, change: Change) -> Result<document::Document, ApplyError> {
    let change = update_change(change, document).map_err(ApplyError::UpdateFailed)?;

    assert_eq!(change.generation, document.generation);

    let mut new_document = document::Document {
        previous: Some((document.clone(), change.clone())),
        root: document.root.clone(),
        datapath: document.datapath.clone(),
        uid: document.uid,
        generation: document::NEXT_GENERATION.fetch_add(1, sync::atomic::Ordering::Relaxed),
    };
    
    match change.ty {
        ChangeType::AlterNode(path, props) => new_document.root = sync::Arc::new(rebuild_node_tree(&document.root, path.into_iter(), |mut target| {
            target.props = props;
            Ok(())
        })?),
        ChangeType::InsertNode(path, after_child, offset, node) => new_document.root = sync::Arc::new(rebuild_node_tree(&document.root, path.into_iter(), |target| {
            /* Check after_child to make sure it's not farther than one-past the end. */
            if after_child > target.children.len() {
                return Err(ApplyError::InvalidParameters);
            }

            /* Check offset to make sure it's within bounds. */
            // TODO: automatically grow parents?
            if offset > target.size.to_addr() {
                return Err(ApplyError::InvalidParameters);
            }

            /* Check child size to make sure it's within bounds. */
            // TODO: automatically grow parents?
            if offset + node.size > target.size.to_addr() {
                return Err(ApplyError::InvalidParameters);
            }
            
            /* Keep child offsets monotonic. */
            if after_child > 0 && target.children[after_child-1].offset > offset {
                return Err(ApplyError::InvalidParameters);
            }

            /* Preconditions passed; do the deed. */
            target.children.insert(after_child, structure::Childhood {
                node,
                offset,
            });

            Ok(())
        })?),
        ChangeType::Nest(parent, first_child, last_child, props) => new_document.root = sync::Arc::new(rebuild_node_tree(&document.root, parent.into_iter(), |parent_node| {
            /* Check indices. */
            if first_child >= parent_node.children.len() || last_child >= parent_node.children.len() || last_child < first_child {
                return Err(ApplyError::InvalidParameters);
            }

            /* Preconditions passed; do the deed. */
            let mut children: Vec<structure::Childhood> = parent_node.children.splice(first_child..=last_child, [structure::Childhood::default()]).collect();

            let offset = children[0].offset;
            let mut size = addr::unit::ZERO;
            for child in &mut children {
                child.offset-= offset.to_size();
                size = std::cmp::max(size, child.offset.to_size() + child.node.size);
            }

            let new_node = &mut parent_node.children[first_child];
            new_node.offset = offset;
            new_node.node = sync::Arc::new(structure::Node {
                size: size,
                children: children,
                props
            });

            Ok(())
        })?),
    }

    Ok(new_document)
}
