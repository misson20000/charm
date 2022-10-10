use std::sync;

use crate::model::document;
use crate::model::document::structure;

#[derive(Debug, Clone)]
pub enum ChangeType {
    /// Modifies the properties of a node, but doesn't affect its children.
    AlterNode(structure::Path, structure::Properties)
}

#[derive(Debug, Clone)]
pub struct Change {
    pub ty: ChangeType,
    pub generation: u64,
}

#[derive(Debug, Clone)]
pub enum UpdateError {
    NoCommonAncestor,
}

pub fn update_path(path: structure::Path, through: &Change) -> Result<structure::Path, UpdateError> {
    match through.ty {
        ChangeType::AlterNode(_, _) => Ok(path),
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
                },
                generation: to.generation
            })
        }, None => Err(UpdateError::NoCommonAncestor)
    }
}

fn rebuild_node_tree<F>(target: &structure::Node, mut path_segment: structure::PathIter, target_modifier: F) -> structure::Node where
    F: FnOnce(&mut structure::Node) {
    let mut new_target = (*target).clone();
    match path_segment.next() {
        Some(index) => {
            /* Recurse to rebuild the child, then rebuild the target with the new child. */
            let child = &*target.children[index].node;
            let new_child = rebuild_node_tree(child, path_segment, target_modifier);
            new_target.children[index].node = sync::Arc::new(new_child);
        },
        None => {
            /* Reached the end of the path. Just modify this node directly. */
            target_modifier(&mut new_target)
        }
    }
    new_target
}

pub fn apply_structural_change(document: &sync::Arc<document::Document>, change: Change) -> Result<document::Document, UpdateError> {
    let change = update_change(change, document)?;

    assert_eq!(change.generation, document.generation);

    let mut new_document = document::Document {
        previous: Some((document.clone(), change.clone())),
        root: document.root.clone(),
        datapath: document.datapath.clone(),
        uid: document.uid,
        generation: document::NEXT_GENERATION.fetch_add(1, sync::atomic::Ordering::Relaxed),
    };
    
    match change.ty {
        ChangeType::AlterNode(path, props) => new_document.root = sync::Arc::new(rebuild_node_tree(&document.root, path.into_iter(), |mut target| target.props = props)),
    }

    Ok(new_document)
}
