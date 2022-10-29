use std::sync;
use std::vec;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;

#[derive(Debug, Clone)]
pub enum ChangeType {
    /// Modifies the properties of a node, but doesn't affect its children.
    AlterNode(structure::Path, structure::Properties),

    /// Inserts an empty node with the given properties into the node referred to by the given path.
    InsertNode(structure::Path, usize, addr::Address, structure::Properties),
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

pub fn update_path(path: structure::Path, through: &Change) -> Result<structure::Path, UpdateError> {
    match through.ty {
        ChangeType::AlterNode(_, _) => Ok(path),
        _ => todo!(),
    }
}

pub fn update_change(change: Change, to: &document::Document) -> Result<Change, UpdateError> {
    if change.generation == to.generation {
        return Ok(change)
    }

    assert!(change.generation < to.generation);
    
    match &to.previous {
        Some((prev_document, doc_change)) => {
            let change = update_change(change, &prev_document)?;

            Ok(Change {
                ty: match change.ty {
                    ChangeType::AlterNode(path, props) => ChangeType::AlterNode(update_path(path, doc_change)?, props),
                    ChangeType::InsertNode(_, _, _, _) => return Err(UpdateError::NotUpdatable),
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
    let change = update_change(change, document).map_err(|e| ApplyError::UpdateFailed(e))?;

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
        ChangeType::InsertNode(path, after_child, offset, props) => new_document.root = sync::Arc::new(rebuild_node_tree(&document.root, path.into_iter(), |target| {
            /* Check after_child to make sure it's not farther than one-past the end. */
            if after_child > target.children.len() {
                return Err(ApplyError::InvalidParameters);
            }

            /* Check offset to make sure it's within bounds. */
            // TODO: automatically grow parents?
            if offset > target.size.to_addr() {
                return Err(ApplyError::InvalidParameters);
            }
            
            /* Keep child offsets monotonic. */
            if after_child > 0 {
                if target.children[after_child-1].offset > offset {
                    return Err(ApplyError::InvalidParameters);
                }
            }

            /* Preconditions passed; do the deed. */
            target.children.insert(after_child, structure::Childhood {
                node: sync::Arc::new(structure::Node {
                    props,
                    size: addr::unit::ZERO,
                    children: vec::Vec::new(),
                }),
                offset,
            });

            Ok(())
        })?),
    }

    Ok(new_document)
}
