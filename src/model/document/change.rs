use std::sync;

use crate::model::addr;
use crate::model::datapath;
use crate::model::document;
use crate::model::document::rebuild;
use crate::model::document::structure;
use crate::model::selection;
use crate::model::versioned;
use crate::model::versioned::Versioned;

#[derive(Debug, Clone)]
pub enum ChangeType {
    /// Modifies the properties of a node, but doesn't affect its children.
    AlterNode {
        path: structure::Path,
        props: structure::Properties
    },

    /// Modifies the properties of all nodes specified by the selection.
    AlterNodesBulk {
        selection: sync::Arc<selection::TreeSelection>,
        prop_changes: structure::MaybeProperties,
    },
    
    /// Inserts the node as a child of the node referred to by the given path.
    InsertNode {
        parent: structure::Path,
        index: usize,
        child: structure::Childhood,
    },

    /// Wraps a node's children (range inclusive) in a new node.
    Nest {
        range: structure::SiblingRange,
        extent: addr::Extent,
        props: structure::Properties,
    },

    /// Deletes a node, letting its children be inherited by their grandparent.
    Destructure {
        parent: structure::Path,
        child_index: usize,
    },

    /// Deletes some of a node's (range inclusive) children.
    DeleteRange {
        range: structure::SiblingRange,
    },

    /// Adds a filter to the datapath (or combines it with the topmost one, if compatible).
    StackFilter {
        filter: datapath::Filter,
    },

    /// Resizes a node (and its parents, if necessary)
    Resize {
        path: structure::Path,
        new_size: addr::Offset,

        /// If set, will also resize any parents necessary to avoid children extending beyond their parents' sizes, unless they are locked in which case an error will be generated.
        expand_parents: bool,

        /// If set, will truncate any parents that ended at the end of this child, unless they are locked in which case they will not be truncated.
        truncate_parents: bool,
    },

    /// Pastes child nodes from a range within a given structure node (not necessarily from this document) at the given location
    Paste {
        src_node: sync::Arc<structure::Node>,
        src_begin: (addr::Offset, usize),
        src_end: (addr::Offset, usize), //< exclusive

        dst: structure::Path,
        dst_offset: addr::Offset,
        dst_index: usize,
    },

    /// Creates repetitions of the targeted node, naming them as "{name_prefix}{i}{name_postfix}", and wrapping them all in a new "array" node.
    Repeat {
        path: structure::Path,
        pitch: addr::Offset,
        count: usize,
        name_prefix: String,
        name_postfix: String,
        props: structure::Properties,
    },
}

#[derive(Debug, Clone)]
pub enum ApplyRecord {
    AlterNode {
        path: structure::Path,
        node: sync::Arc<structure::Node>,
    },

    AlterNodesBulk {
        selection: sync::Arc<selection::TreeSelection>,
        prop_changes: structure::MaybeProperties,
    },

    InsertNode {
        parent: structure::Path,
        index: usize,
        child: structure::Childhood,
    },

    Nest {
        range: structure::SiblingRange,
        extent: addr::Extent,
        child: structure::Childhood,
    },

    Destructure(DestructureApplyRecord),

    DeleteRange {
        range: structure::SiblingRange,
    },

    StackFilter {
        filter: datapath::Filter,
    },

    Resize {
        path: structure::Path,
        new_size: addr::Offset,

        /// How many parents of the targeted node were also resized as part of this operation.
        parents_resized: usize,
    },

    Paste(PasteApplyRecord),

    Repeat {
        path: structure::Path,
        pitch: addr::Offset,
        count: usize,
    },
}

#[derive(Debug, Clone)]
pub struct DestructureApplyRecord {
    pub parent: structure::Path,
    pub child_index: usize,

    pub childhood: structure::Childhood,

    /// Each item in this vector indicates the final index within
    /// the parent that a grandchild wound up at. This should be
    /// monotonically increasing.
    pub mapping: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct PasteApplyRecord {
    pub parent: structure::Path,

    pub dst_begin: (addr::Offset, usize),
    pub dst_end: (addr::Offset, usize),
    
    /// Each item in this vector indicates the final index within
    /// the parent that a new child wound up at. This should be
    /// monotonically increasing.
    pub mapping: Vec<usize>,
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
pub enum UpdateRangeResult {
    /// The range wasn't affected by this change at all.
    Unmoved(structure::SiblingRange),

    /// The range was moved or shifted somewhere else.
    Moved(structure::SiblingRange),

    /// An ancestor of the range was deleted.
    ParentDeleted,

    /// All the children referred to by the range were deleted.
    AllDeleted,
    
    /// Some children referred to by the range were deleted or moved elsewhere, but not all of them.
    Split,

    /// Another child was inserted in the middle of the range.
    Inserted { range: structure::SiblingRange, absolute_index: usize },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UpdateError {
    NoCommonAncestor,
    NotUpdatable,
    NotYetImplemented,
    NodeDeleted,
    RangeSplit,
}

#[derive(Debug, Clone)]
pub enum ApplyErrorType {
    UpdateFailed {
        error: UpdateError,
        incompatible_change: Option<ApplyRecord>,
    },
    InvalidRange(structure::RangeInvalidity),
    InvalidParameters(&'static str),
    ResizedSmallerThanChildren,
    ResizedLargerThanParent,
}

#[derive(Debug, Clone)]
pub struct ApplyError {
    pub ty: ApplyErrorType,
    pub change: Change,
}

impl Change {
    /// Ok(updated change)
    /// Err(why, non-updated self, other incompatible change that caused failure)
    pub fn rebase(mut self, to: &document::Document) -> Result<Self, (UpdateError, Self, Option<ApplyRecord>)> {
        if self.generation == to.generation() {
            return Ok(self)
        }

        assert!(self.generation < to.generation());
        
        match &to.previous() {
            Some((prev_document, doc_change)) => {
                self = self.rebase(prev_document)?;

                let backup = self.clone();
                
                Ok(Change {
                    ty: match self.ty {
                        ChangeType::AlterNode { ref mut path, .. } => match doc_change.update_path(path) {
                            UpdatePathResult::Unmoved | UpdatePathResult::Moved => Ok(self.ty),
                            UpdatePathResult::Deleted | UpdatePathResult::Destructured => Err(UpdateError::NodeDeleted),
                        },
                        ChangeType::AlterNodesBulk { .. } => Err(UpdateError::NotYetImplemented),
                        ChangeType::InsertNode { .. } => Err(UpdateError::NotYetImplemented),
                        ChangeType::Nest { range, extent, props } => match doc_change.update_range(range) {
                            UpdateRangeResult::Unmoved(range)
                                | UpdateRangeResult::Moved(range)
                                | UpdateRangeResult::Inserted { range, .. }
                            => Ok(ChangeType::Nest { range, extent, props }),
                            
                            UpdateRangeResult::ParentDeleted
                                | UpdateRangeResult::AllDeleted
                                => Err(UpdateError::NodeDeleted),
                            
                            UpdateRangeResult::Split
                                => Err(UpdateError::RangeSplit),
                        },
                        ChangeType::Destructure { .. } => Err(UpdateError::NotYetImplemented),
                        ChangeType::DeleteRange { range } => match doc_change.update_range(range) {
                            UpdateRangeResult::Unmoved(range)
                                | UpdateRangeResult::Moved(range)
                                => Ok(ChangeType::DeleteRange { range }),
                            
                            UpdateRangeResult::ParentDeleted
                                | UpdateRangeResult::AllDeleted
                                => Err(UpdateError::NodeDeleted),
                            
                            UpdateRangeResult::Split
                                | UpdateRangeResult::Inserted { .. }
                            => Err(UpdateError::RangeSplit),
                        },
                        ChangeType::StackFilter { .. } => Err(UpdateError::NotYetImplemented),
                        ChangeType::Resize { ref mut path, .. } => match doc_change.update_path(path) {
                            UpdatePathResult::Unmoved | UpdatePathResult::Moved => Ok(self.ty),
                            UpdatePathResult::Deleted | UpdatePathResult::Destructured => Err(UpdateError::NodeDeleted),
                        },
                        ChangeType::Paste { .. } => Err(UpdateError::NotYetImplemented),
                        ChangeType::Repeat { .. } => Err(UpdateError::NotYetImplemented),
                    }.map_err(|e| (e, backup, Some(doc_change.clone())))?,
                    generation: to.generation()
                })
            }, None => Err((UpdateError::NoCommonAncestor, self, None))
        }
    }

    fn apply_impl(&self, document: &mut document::Document) -> Result<ApplyRecord, ApplyErrorType> {
        match &self.ty {
            ChangeType::AlterNode { path, props } => {
                let new_tree = rebuild::rebuild_node_tree_visiting_path_simple(&document.root, path, |target| {
                    target.props = props.clone();
                    Ok(())
                })?;

                document.root = new_tree.root;

                Ok(ApplyRecord::AlterNode { path: path.clone(), node: new_tree.target })
            },
            ChangeType::AlterNodesBulk { selection, prop_changes } => {
                if !sync::Arc::ptr_eq(&selection.document.root, &document.root) {
                    return Err(ApplyErrorType::InvalidParameters("selection was for a different document (or different version of the same document)"));
                }
                
                document.root = rebuild::rebuild_node_tree_visiting_tree(selection, |target| {
                    target.props.apply_changes(prop_changes.clone());
                    Ok::<(), ApplyErrorType>(())
                })?;

                Ok(ApplyRecord::AlterNodesBulk { selection: selection.clone(), prop_changes: prop_changes.clone() })
            },
            ChangeType::InsertNode { parent: path, index: at_child, child: childhood } => {
                document.root = rebuild::rebuild_node_tree_visiting_path_simple(&document.root, &path, |target| {
                    /* Check at_child to make sure it's not farther than one-past the end. */
                    if *at_child > target.children.len() {
                        return Err(ApplyErrorType::InvalidParameters("attempted to insert node at out-of-bounds index"));
                    }

                    /* Check offset to make sure it's within bounds. */
                    // TODO: automatically grow parents?
                    if childhood.offset > target.size {
                        return Err(ApplyErrorType::InvalidParameters("attempted to insert node beginning beyond parent's size"));
                    }

                    /* Check child size to make sure it's within bounds. */
                    // TODO: automatically grow parents?
                    let end = match childhood.offset.checked_add(childhood.node.size) {
                        Some(end) => end,
                        None => return Err(ApplyErrorType::InvalidParameters("attempted to insert node at a place where its end would overflow"))
                    };
                    
                    if end > target.size {
                        return Err(ApplyErrorType::InvalidParameters("attempted to insert node extending beyond parent's size"));
                    }
                    
                    /* Keep child offsets monotonic. */
                    if (*at_child > 0 && target.children[at_child-1].offset > childhood.offset) || (*at_child < target.children.len() && target.children[*at_child].offset < childhood.offset) {
                        println!("rejecting insert at position {}, offset {} into children {:?}", at_child, childhood.offset, target.children);
                        return Err(ApplyErrorType::InvalidParameters("attempted to insert node at an index that would break offset monotonicity"));
                    }

                    /* Preconditions passed; do the deed. */
                    target.children.insert(*at_child, childhood.clone());

                    Ok(())
                })?.root;

                Ok(ApplyRecord::InsertNode { parent: path.clone(), index: *at_child, child: childhood.clone() })
            },
            ChangeType::Nest { range, extent, props } => {
                let result = rebuild::rebuild_node_tree_visiting_path_simple(&document.root, &range.parent, |parent_node| {
                    /* Check range validity. */
                    range.check_validity(parent_node)?;

                    /* Check that children are all contained within the extent */
                    let children_extent = addr::Extent::between(parent_node.children[range.first].offset, parent_node.children[range.last].end());
                    if !extent.contains(children_extent) {
                        return Err(ApplyErrorType::InvalidParameters("data extent does not contain all nested children"));
                    }

                    /* Preconditions passed; do the deed. */
                    let mut children: Vec<structure::Childhood> = parent_node.children.splice(range.indices(), [structure::Childhood::default()]).collect();

                    for child in &mut children {
                        child.offset-= extent.begin;
                    }

                    let new_node = &mut parent_node.children[range.first];
                    new_node.offset = extent.begin;
                    new_node.node = sync::Arc::new(structure::Node {
                        size: extent.len(),
                        children: children,
                        props: props.clone(),
                    });

                    Ok(())
                })?;

                document.root = result.root;

                Ok(ApplyRecord::Nest { range: range.clone(), extent: *extent, child: result.target.children[range.first].clone()})
            }
            ChangeType::Destructure { parent, child_index } => {
                let result = rebuild::rebuild_node_tree_visiting_path_simple(&document.root, &parent, |parent_node| {
                    /* Check that we're trying to destructure a child that actually exists. */
                    if *child_index >= parent_node.children.len() {
                        return Err(ApplyErrorType::InvalidParameters("attemped to destructure child that doesn't exist"));
                    }

                    /* Preconditions passed; do the deed. */
                    let destructured_child = parent_node.children.remove(*child_index);
                    let offset = destructured_child.offset;

                    let mut siblings = parent_node.children.split_off(*child_index).into_iter().peekable();
                    let mut grandchildren = destructured_child.node.children.iter().peekable();
                    let mut mapping = Vec::new();
                    mapping.reserve(grandchildren.len());
                    parent_node.children.reserve(grandchildren.len() + siblings.len());

                    /* Intersperse siblings with grandchildren, respecting offset monotonicity. */
                    loop {
                        parent_node.children.push(match (siblings.peek(), grandchildren.peek()) {
                            /* Prefer to sort grandchildren from destructured node before siblings that came after the destructured node. */
                            (Some(s), Some(g)) if (g.offset + offset) <= s.offset => {
                                mapping.push(parent_node.children.len());
                                let mut child = grandchildren.next().unwrap().clone();
                                child.offset+= offset;
                                child
                            },
                            (Some(_s), Some(_g)) => siblings.next().unwrap(),
                            (Some(_s), None) => siblings.next().unwrap(),
                            (None, Some(_g)) => {
                                mapping.push(parent_node.children.len());
                                let mut child = grandchildren.next().unwrap().clone();
                                child.offset+= offset;
                                child
                            },
                            (None, None) => break,
                        });
                    }

                    Ok(ApplyRecord::Destructure(DestructureApplyRecord {
                        parent: parent.clone(),
                        child_index: *child_index,
                        childhood: destructured_child,
                        mapping,
                    }))
                })?;

                document.root = result.root;

                Ok(result.output) 
            },
            ChangeType::DeleteRange { range } => {
                let result = rebuild::rebuild_node_tree_visiting_path_simple(&document.root, &range.parent, |parent_node| {
                    /* Check range validity. */
                    range.check_validity(parent_node)?;

                    /* Preconditions passed; do the deed. */
                    parent_node.children.splice(range.indices(), []);

                    Ok(())
                })?;
                
                document.root = result.root;

                Ok(ApplyRecord::DeleteRange { range: range.clone() })
            },
            ChangeType::StackFilter { filter } => {
                document.datapath.stack(filter);

                Ok(ApplyRecord::StackFilter { filter: filter.clone() })
            },
            ChangeType::Resize { path, new_size, expand_parents, truncate_parents } => {
                let visitor = RebuildNodeTreeVisitorForResize {
                    path,
                    new_size: *new_size,
                    expand_parents: *expand_parents,
                    truncate_parents: *truncate_parents,
                    highest_ancestor_modified: 0,
                };

                let result = rebuild::rebuild_node_tree_visiting_path(&document.root, path, visitor)?;

                document.root = result.root;

                Ok(result.output)
            },
            ChangeType::Paste { src_node, src_begin, src_end, dst, dst_offset, dst_index } => {
                let result = rebuild::rebuild_node_tree_visiting_path_simple(&document.root, &dst, |dst_node| {
                    if src_begin.0 > src_node.size || src_end.0 > src_node.size {
                        return Err(ApplyErrorType::InvalidParameters("attempted to paste from range outside of source node"));
                    }

                    if *dst_offset + (src_end.0 - src_begin.0) > dst_node.size {
                        return Err(ApplyErrorType::InvalidParameters("attempted to paste into node that is too small"));
                    }

                    let mut dst_siblings = dst_node.children.split_off(*dst_index).into_iter().peekable();
                    let mut src_siblings = src_node.children[src_begin.1..src_end.1].iter().cloned().peekable();
                    let mut mapping = Vec::new();
                    mapping.reserve(src_end.1 - src_begin.1);
                    dst_node.children.reserve(dst_siblings.len() + src_siblings.len());

                    /* Intersperse nodes, respecting offset monotonicity. */
                    loop {
                        dst_node.children.push(match (src_siblings.peek(), dst_siblings.peek()) {
                            /* if there is both a child to paste still available and a child in the destination available, but the pastee is at a lower offset */
                            (Some(s), Some(d)) if (s.offset - src_begin.0 + *dst_offset) <= d.offset => {
                                mapping.push(dst_node.children.len());
                                let mut child = src_siblings.next().unwrap();
                                child.offset-= src_begin.0;
                                child.offset+= *dst_offset;
                                child
                            },
                            /* if there is both a child to paste still available and a child in the destination available, but we didn't match earlier, so the existing child comes first */
                            (Some(_s), Some(_d)) => dst_siblings.next().unwrap(),
                            /* there are no more children in the destination, but still pastee children */
                            (Some(_s), None) => {
                                mapping.push(dst_node.children.len());
                                let mut child = src_siblings.next().unwrap();
                                child.offset-= src_begin.0;
                                child.offset+= *dst_offset;
                                child
                            },
                            /* there are no more pastee children, but still destination children */
                            (None, Some(_d)) => dst_siblings.next().unwrap(),
                            /* done! */
                            (None, None) => break,
                        });
                    }

                    Ok(ApplyRecord::Paste(PasteApplyRecord {
                        parent: dst.clone(),
                        dst_begin: (*dst_offset, *dst_index),
                        dst_end: (*dst_offset + (src_end.0 - src_begin.0), mapping.last().map(|i| i+1).unwrap_or(*dst_index)),
                        mapping,
                    }))
                })?;

                document.root = result.root;

                Ok(result.output)
            },
            ChangeType::Repeat { path, pitch, count, name_prefix, name_postfix, props } => {
                if path.len() == 0 {
                    return Err(ApplyErrorType::InvalidParameters("cannot repeat the root node"));
                }
                if *count == 0 {
                    return Err(ApplyErrorType::InvalidParameters("cannot repeat zero times"));
                }
                
                let result = rebuild::rebuild_node_tree_visiting_path_simple(&document.root, &path[0..path.len()-1], |parent_node| {
                    let template_child = &mut parent_node.children[*path.last().unwrap()];

                    if template_child.end() + (*pitch * (count-1) as u64) > parent_node.size {
                        return Err(ApplyErrorType::InvalidParameters("array would be too long to fit within parent node"));
                    }

                    let children: Vec<_> = (0..*count).into_iter().map(|i| {
                        let mut props = template_child.node.props.clone();
                        props.name = format!("{}{}{}", name_prefix, i, name_postfix);

                        structure::Childhood {
                            node: sync::Arc::new(structure::Node {
                                size: template_child.node.size,
                                children: template_child.node.children.clone(),
                                props,
                            }),
                            offset: *pitch * i as u64
                        }
                    }).collect();
                    
                    template_child.node = sync::Arc::new(structure::Node {
                        size: children.last().unwrap().end(),
                        children,
                        props: props.clone(),
                    });

                    Ok(ApplyRecord::Repeat { path: path.clone(), pitch: *pitch, count: *count })
                })?;

                document.root = result.root;

                Ok(result.output)
            },
        }
    }
}

struct RebuildNodeTreeVisitorForResize<'a> {
    path: &'a structure::Path,
    new_size: addr::Offset,
    expand_parents: bool,
    truncate_parents: bool,
    highest_ancestor_modified: usize,
}

impl<'a> rebuild::TargetVisitor for RebuildNodeTreeVisitorForResize<'a> {
    type Output = ApplyRecord;
    type Error = ApplyErrorType;
    type AncestorVisitor = Self;

    fn visit_target(self, target: &mut structure::Node) -> Result<Self::AncestorVisitor, Self::Error> {
        if target.children.iter().any(|c| c.end() > self.new_size) {
            return Err(ApplyErrorType::ResizedSmallerThanChildren);
        }
        
        target.size = self.new_size;
        
        Ok(self)
    }
}

impl<'a> rebuild::AncestorVisitor for RebuildNodeTreeVisitorForResize<'a> {
    type Output = ApplyRecord;
    type Error = ApplyErrorType;

    fn visit_ancestor(&mut self, ancestor: &mut structure::Node, ancestry: usize, old_childhood: structure::Childhood, child_index: usize) -> Result<(), Self::Error> {
        if ancestor.children[child_index].node.size == old_childhood.node.size {
            return Ok(());
        }

        if ancestor.children[child_index].node.size > old_childhood.node.size {
            /* Child was resized to be larger */
            if ancestor.children[child_index].end() > ancestor.size {
                if self.expand_parents && !ancestor.props.locked {
                    self.highest_ancestor_modified = ancestry;
                    ancestor.size = ancestor.children[child_index].end();
                } else {
                    return Err(ApplyErrorType::ResizedLargerThanParent);
                }
            }
        } else {
            /* Child was shrunken. */
            if old_childhood.end() == ancestor.size && self.truncate_parents && !ancestor.props.locked {
                /* It's possible that if two children overlap and we shrink one of
                 * them, the other one will now extend past the end of the shrunken
                 * one. We wouldn't want to truncate the parent too far in that
                 * case. */
                self.highest_ancestor_modified = ancestry;
                ancestor.size = ancestor.children.iter().map(structure::Childhood::end).max().expect("there should definitely be at least one child if this node is an ANCESTOR of a node we're modifying during a tree rebuild");
            }
        }
        
        Ok(())
    }

    fn finalize(self) -> Self::Output {
        ApplyRecord::Resize {
            path: self.path.clone(),
            new_size: self.new_size,
            parents_resized: self.highest_ancestor_modified,
        }
    }
}

impl ApplyRecord {
    #[must_use]
    pub fn update_path(&self, path: &mut structure::Path) -> UpdatePathResult {
        match self {
            ApplyRecord::AlterNode { .. } => UpdatePathResult::Unmoved,
            ApplyRecord::AlterNodesBulk { .. } => UpdatePathResult::Unmoved,
            ApplyRecord::InsertNode { parent, index: affected_index, child: _ } => {
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
            ApplyRecord::Nest { range, .. } => {
                if path.len() > range.parent.len() && &path[0..range.parent.len()] == &range.parent[..] {
                    let child_index = &mut path[range.parent.len()];
                    
                    if range.contains_index(*child_index) {
                        *child_index-= range.first;
                        path.insert(range.parent.len(), range.first);
                        UpdatePathResult::Moved
                    } else if *child_index > range.last {
                        *child_index-= range.count() - 1;
                        UpdatePathResult::Moved
                    } else {
                        UpdatePathResult::Unmoved
                    }
                } else {
                    UpdatePathResult::Unmoved
                }
            },
            ApplyRecord::Destructure(dsr) => {
                if path.len() > dsr.parent.len() && &path[0..dsr.parent.len()] == &dsr.parent[..] {
                    if path[dsr.parent.len()] < dsr.child_index {
                        /* Path was pointing to a sibling of the node being destructured that comes before it */
                        UpdatePathResult::Unmoved
                    } else if path[dsr.parent.len()] == dsr.child_index {
                        if path.len() == dsr.parent.len() + 1 {
                            /* Path was pointing to the node that got deleted. */
                            path.pop();
                            UpdatePathResult::Destructured
                        } else {
                            /* Path was pointing to a grandchild. */

                            /* Delete the node that got destructured from the path. */
                            path.remove(dsr.parent.len());
                          
                            /* Use the mapping to figure out what the grandchild's new index is. */
                            path[dsr.parent.len()] = dsr.mapping[path[dsr.parent.len()]];
                            
                            UpdatePathResult::Moved
                        }
                    } else {
                        /* Path was pointing to a sibling of the node being destructured that comes after it. */
                        dsr.adjust_sibling_index(&mut path[dsr.parent.len()]);
                        
                        UpdatePathResult::Moved
                    }
                } else {
                    /* Path is unrelated */
                    UpdatePathResult::Unmoved
                }
            },
            ApplyRecord::DeleteRange { range } => {
                if path.len() > range.parent.len() && &path[0..range.parent.len()] == &range.parent[..] {
                    let child_index = &mut path[range.parent.len()];
                    
                    if range.contains_index(*child_index) {
                        path.truncate(range.parent.len());

                        UpdatePathResult::Deleted
                    } else if *child_index > range.last {
                        *child_index-= range.count();

                        UpdatePathResult::Moved
                    } else {
                        UpdatePathResult::Unmoved
                    }
                } else {
                    UpdatePathResult::Unmoved
                }
            },
            ApplyRecord::StackFilter { .. } => UpdatePathResult::Unmoved,
            ApplyRecord::Resize { .. } => UpdatePathResult::Unmoved,
            ApplyRecord::Paste(par) => {
                if path.len() > par.parent.len() && &path[0..par.parent.len()] == &par.parent[..] {
                    if par.adjust_sibling_index(&mut path[par.parent.len()]) {
                        UpdatePathResult::Moved
                    } else {
                        UpdatePathResult::Unmoved
                    }
                } else {
                    /* Path is unrelated */
                    UpdatePathResult::Unmoved
                }
            },
            ApplyRecord::Repeat { path: repeated_path, .. } => {
                if path.len() >= repeated_path.len() && path[0..repeated_path.len()] == repeated_path[..] {
                    path.insert(repeated_path.len(), 0);
                    UpdatePathResult::Moved
                } else {
                    UpdatePathResult::Unmoved
                }
            }
        }
    }

    #[must_use]
    pub fn update_range(&self, mut subject: structure::SiblingRange) -> UpdateRangeResult {
        match self {
            ApplyRecord::AlterNode { .. } => UpdateRangeResult::Unmoved(subject),
            ApplyRecord::AlterNodesBulk { .. } => UpdateRangeResult::Unmoved(subject),
            
            ApplyRecord::InsertNode { parent, index, .. } => {
                if subject.parent[..] == parent[..] {
                    if subject.contains_index(*index) {
                        subject.last+= 1;
                        UpdateRangeResult::Inserted { range: subject, absolute_index: *index }
                    } else if *index < subject.first {
                        subject.first+= 1;
                        subject.last+= 1;
                        UpdateRangeResult::Moved(subject)
                    } else {
                        UpdateRangeResult::Unmoved(subject)
                    }
                } else if subject.parent.len() > parent.len() && parent[0..subject.parent.len()] == subject.parent[..] {
                    let path_index = &mut subject.parent[parent.len()];

                    if *path_index >= *index {
                        *path_index+= 1;
                        UpdateRangeResult::Moved(subject)
                    } else {
                        UpdateRangeResult::Unmoved(subject)
                    }
                } else {
                    UpdateRangeResult::Unmoved(subject)
                }
            },
            
            ApplyRecord::Nest { .. } => todo!(),

            ApplyRecord::Destructure { .. } => todo!(),

            ApplyRecord::DeleteRange { range } => {
                if subject.parent == range.parent {
                    if range.last < subject.first {
                        /* Siblings before the subject range were deleted. Shift indices. */
                        subject.first-= range.count();
                        subject.last-= range.count();
                        UpdateRangeResult::Moved(subject)
                    } else if range.first > subject.last {
                        /* Siblings after the subject range were deleted. Don't care. */
                        UpdateRangeResult::Unmoved(subject)
                    } else if range.indices() == subject.indices() {
                        /* The same range was deleted. */
                        UpdateRangeResult::AllDeleted
                    } else {
                        /* Ranges overlapped. Sad times. */
                        UpdateRangeResult::Split
                    }
                } else if subject.parent.len() > range.parent.len() && subject.parent[0..range.parent.len()] == range.parent[..] {
                    /* An ancestor or ancestor's sibling was deleted. */
                    let child_index = &mut subject.parent[range.parent.len()];

                    if range.contains_index(*child_index) {
                        /* Ancestor was deleted. */
                        UpdateRangeResult::ParentDeleted
                    } else if *child_index > range.last {
                        /* Ancestor's sibling before ancestor was deleted, need to fixup path. */
                        *child_index-= range.count();
                        UpdateRangeResult::Moved(subject)
                    } else {
                        UpdateRangeResult::Unmoved(subject)
                    }
                } else {
                    UpdateRangeResult::Unmoved(subject)
                }
            },

            ApplyRecord::StackFilter { .. } => UpdateRangeResult::Unmoved(subject),
            ApplyRecord::Resize { .. } => UpdateRangeResult::Unmoved(subject),

            ApplyRecord::Paste(_par) => todo!(),

            ApplyRecord::Repeat { .. } => todo!(),
        }
    }

    pub fn summarize(&self, document: &document::Document) -> String {
        match self {
            ApplyRecord::AlterNode { path, .. } => format!("Alter properties on {}", document.describe_path(path)),
            ApplyRecord::AlterNodesBulk { .. } => format!("Alter properties on multiple nodes"),
            ApplyRecord::InsertNode { parent, child, .. } => format!("Insert '{}' under {}", child.node.props.name, document.describe_path(parent)),
            ApplyRecord::Nest { range, .. } => format!("Nest children under {}", document.describe_path(&range.parent)),
            ApplyRecord::Destructure(dsr) => format!("Destructure child under {}", document.describe_path(&dsr.parent)),
            ApplyRecord::DeleteRange { range, .. } => format!("Delete children under {}", document.describe_path(&range.parent)),
            ApplyRecord::StackFilter { filter } => format!("Add '{}' to datapath", filter.human_details()),
            ApplyRecord::Resize { path, new_size, .. } => format!("Resize {} to size {}", document.describe_path(path), new_size),
            ApplyRecord::Paste(par) => format!("Paste structure nodes into {}", document.describe_path(&par.parent)),
            ApplyRecord::Repeat { path, count, .. } => format!("Repeat {} {} times", document.describe_path(path), count),
        }
    }
}

impl DestructureApplyRecord {
    /// Transforms the index of a sibling of the destructured node, dealing with the destructured node's children potentially interspersing with the indexed sibling. If the destructured node itself is indexed, the index will be unaffected (and will point to either the first grandchild or one-past-the-end).
    pub fn adjust_sibling_index(&self, index: &mut usize) {
        if *index > self.child_index {
            /* Subtract 1 to account for the destructured node being removed. */
            *index-= 1;

            for inserted_index in &self.mapping {
                /* For each grandchild that got inserted before the target node, adjust the index by 1. Note
                 * that we can't tell whether a grandchild was inserted before the target node without
                 * processing the earlier grandchildren first... */
                if *inserted_index <= *index {
                    *index+= 1;
                }
            }
        }
    }

    pub fn offset(&self) -> addr::Offset {
        self.childhood.offset
    }
    
    pub fn end(&self) -> addr::Offset {
        self.childhood.end()
    }

    pub fn end_index(&self) -> usize {
        self.mapping.last().copied().unwrap_or(self.child_index)
    }
}

impl PasteApplyRecord {
    /// Transforms the index of a sibling in the node that got pasted into.
    pub fn adjust_sibling_index(&self, index: &mut usize) -> bool {
        let mut moved = false;
        for inserted_index in &self.mapping {
            /* For each pastee that got inserted before the target node, adjust the index by 1. Note
             * that we can't tell whether a pastee was inserted before the target node without
             * processing the earlier pastee first... */
            if *inserted_index <= *index {
                *index+= 1;
                moved = true;
            }
        }
        moved
    }
}

impl versioned::Change<document::Document> for Change {
    type ApplyError = ApplyError;
    type ApplyRecord = ApplyRecord;
    
    fn apply(mut self, document: &mut document::Document) -> Result<(Change, ApplyRecord), ApplyError> {
        self = self.rebase(document).map_err(|(update_error, change, incompatible_change)| ApplyErrorType::UpdateFailed { error: update_error, incompatible_change }.complete(change))?;

        assert_eq!(self.generation, document.generation());

        match self.apply_impl(document) {
            Ok(record) => Ok((self, record)),
            Err(ty) => Err(ty.complete(self)),
        }
    }
}

impl ApplyErrorType {
    fn complete(self, change: Change) -> ApplyError {
        ApplyError {
            ty: self,
            change,
        }
    }
}

impl From<structure::RangeInvalidity> for ApplyErrorType {
    fn from(invalidity: structure::RangeInvalidity) -> ApplyErrorType {
        ApplyErrorType::InvalidRange(invalidity)
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

        assert_eq!(ApplyRecord::AlterNode {
            path: vec![1, 0],
            node: sync::Arc::new(structure::Node::default()),
        }.update_path(&mut path), UpdatePathResult::Unmoved);
        
        assert_eq!(path, vec![1, 0, 2]);
    }

    #[test]
    fn test_update_path_through_alter_nodes_bulk() {
        let mut path = vec![1, 0, 2];

        assert_eq!(ApplyRecord::AlterNodesBulk {
            selection: sync::Arc::new(selection::TreeSelection::new(sync::Arc::new(create_test_document_1()))),
            prop_changes: structure::MaybeProperties::new(structure::Properties::default())
        }.update_path(&mut path), UpdatePathResult::Unmoved);
        
        assert_eq!(path, vec![1, 0, 2]);
    }
    
    #[test]
    fn test_update_path_through_insert_node() {
        let mut path = vec![1, 0, 2];

        assert_eq!(ApplyRecord::InsertNode {
            parent: vec![1, 0],
            index: 1,
            child: structure::Node::builder().build_child(addr::Offset::NULL),
        }.update_path(&mut path), UpdatePathResult::Moved);
        
        assert_eq!(path, vec![1, 0, 3]);

        assert_eq!(ApplyRecord::InsertNode {
            parent: vec![1, 0],
            index: 4,
            child: structure::Node::builder().build_child(addr::Offset::NULL)
        }.update_path(&mut path), UpdatePathResult::Unmoved);

        assert_eq!(path, vec![1, 0, 3]);

        assert_eq!(ApplyRecord::InsertNode {
            parent: vec![1, 0],
            index: 3,
            child: structure::Node::builder().build_child(addr::Offset::NULL)
        }.update_path(&mut path), UpdatePathResult::Moved);

        assert_eq!(path, vec![1, 0, 4]);

        assert_eq!(ApplyRecord::InsertNode {
            parent: vec![1, 1],
            index: 0,
            child: structure::Node::builder().build_child(addr::Offset::NULL)
        }.update_path(&mut path), UpdatePathResult::Unmoved);

        assert_eq!(path, vec![1, 0, 4]);
    }

    #[test]
    fn test_update_path_through_nest() {
        let extent = addr::Extent::between(addr::Offset::NULL, addr::Offset::NULL);
        let child = structure::Node::builder().build_child(addr::Offset::NULL);
        
        /* siblings before but not including the path */
        let mut path = vec![1, 0, 2];
        assert_eq!(ApplyRecord::Nest {
            range: structure::SiblingRange::new(vec![1, 0], 0, 1),
            extent,
            child: child.clone()
        }.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 0, 1]);

        /* siblings after but not including the path */
        let mut path = vec![1, 0, 2];
        assert_eq!(ApplyRecord::Nest {
            range: structure::SiblingRange::new(vec![1, 0], 3, 10),
            extent,
            child: child.clone()
        }.update_path(&mut path), UpdatePathResult::Unmoved);
        assert_eq!(path, vec![1, 0, 2]);

        /* including the path */
        let mut path = vec![1, 0, 5];
        assert_eq!(ApplyRecord::Nest {
            range: structure::SiblingRange::new(vec![1, 0], 3, 10),
            extent,
            child: child.clone()
        }.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 0, 3, 2]);

        /* ancestor of the path */
        let mut path = vec![1, 0, 5, 4];
        assert_eq!(ApplyRecord::Nest {
            range: structure::SiblingRange::new(vec![1, 0], 3, 10),
            extent,
            child: child.clone()
        }.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 0, 3, 2, 4]);        

        /* unrelated path */
        let mut path = vec![1, 0, 5, 4];
        assert_eq!(ApplyRecord::Nest {
            range: structure::SiblingRange::new(vec![1, 1], 3, 10),
            extent,
            child: child.clone()
        }.update_path(&mut path), UpdatePathResult::Unmoved);
        assert_eq!(path, vec![1, 0, 5, 4]);
    }

    #[test]
    fn test_update_path_through_destructure() {
        let record = ApplyRecord::Destructure(DestructureApplyRecord {
            parent: vec![1],
            child_index: 1,
            childhood: structure::Node::builder().build_child(addr::Offset::NULL),
            mapping: vec![1, 2, 4],
        });
        
        /* sibling of destructured node (before) */
        let mut path = vec![1, 0];
        assert_eq!(record.update_path(&mut path), UpdatePathResult::Unmoved);
        assert_eq!(path, vec![1, 0]);

        /* descendant of sibling of destructured node (before) */
        let mut path = vec![1, 0, 6, 7];
        assert_eq!(record.update_path(&mut path), UpdatePathResult::Unmoved);
        assert_eq!(path, vec![1, 0, 6, 7]);
        
        /* sibling of destructured node (interspersed) */
        let mut path = vec![1, 2];
        assert_eq!(record.update_path(&mut path), UpdatePathResult::Moved);
        /* 1 before: [0, [0, 1, 2], 2, 3, ...] */
        /* 1  after: [0, old 0, old 1, 2, old 2, 3, ...] */
        assert_eq!(path, vec![1, 3]);

        /* descendant of sibling of destructured node (interspersed) */
        let mut path = vec![1, 2, 6, 7];
        assert_eq!(record.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 3, 6, 7]);
        
        /* sibling of destructured node (after) */
        let mut path = vec![1, 3];
        assert_eq!(record.update_path(&mut path), UpdatePathResult::Moved);
        /* 1 before: [0, [0, 1, 2], 2, 3, ...] */
        /* 1  after: [0, old 0, old 1, 2, old 2, 3, ...] */
        assert_eq!(path, vec![1, 5]);

        /* descendant of sibling of destructured node (after) */
        let mut path = vec![1, 3, 6, 7];
        assert_eq!(record.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 5, 6, 7]);
        
        /* destructured node */
        let mut path = vec![1, 1];
        assert_eq!(record.update_path(&mut path), UpdatePathResult::Destructured);
        assert_eq!(path, vec![1]);

        /* descendant of destructured node */
        let mut path = vec![1, 1, 0, 4];
        assert_eq!(record.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 1, 4]);

        /* descendant of destructured node (interspersed) */
        let mut path = vec![1, 1, 2, 4];
        assert_eq!(record.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 4, 4]);

        /* unrelated path */
        let mut path = vec![2, 2, 6, 7];
        assert_eq!(record.update_path(&mut path), UpdatePathResult::Unmoved);
        assert_eq!(path, vec![2, 2, 6, 7]);
    }

    #[test]
    fn test_update_path_through_delete_range() {
        /* siblings before but not including the path */
        let mut path = vec![1, 0, 2];
        assert_eq!(ApplyRecord::DeleteRange {
            range: structure::SiblingRange::new(vec![1, 0], 0, 1)
        }.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 0, 0]);

        /* siblings after but not including the path */
        let mut path = vec![1, 0, 2];
        assert_eq!(ApplyRecord::DeleteRange {
            range: structure::SiblingRange::new(vec![1, 0], 3, 10)
        }.update_path(&mut path), UpdatePathResult::Unmoved);
        assert_eq!(path, vec![1, 0, 2]);

        /* including the path */
        let mut path = vec![1, 0, 5];
        assert_eq!(ApplyRecord::DeleteRange {
            range: structure::SiblingRange::new(vec![1, 0], 3, 10)
        }.update_path(&mut path), UpdatePathResult::Deleted);
        /* don't care what the path is */

        /* ancestor of the path */
        let mut path = vec![1, 0, 5, 4];
        assert_eq!(ApplyRecord::DeleteRange {
            range: structure::SiblingRange::new(vec![1, 0], 3, 10)
        }.update_path(&mut path), UpdatePathResult::Deleted);
        /* don't care what the path is */

        /* unrelated path */
        let mut path = vec![1, 0, 5, 4];
        assert_eq!(ApplyRecord::DeleteRange {
            range: structure::SiblingRange::new(vec![1, 1], 3, 10)
        }.update_path(&mut path), UpdatePathResult::Unmoved);
        assert_eq!(path, vec![1, 0, 5, 4]);
    }

    #[test]
    fn test_update_path_through_paste() {
        let record = ApplyRecord::Paste(PasteApplyRecord {
            parent: vec![1],
            dst_begin: (0x20.into(), 2),
            dst_end: (0x30.into(), 6),
            mapping: vec![2, 3, 5],
        });

        /*
        src0
        src1
        paste0
        paste1
        src2
        paste3
        src3
        ...
         */

        /* unrelated path */
        let mut path = vec![2];
        assert_eq!(record.update_path(&mut path), UpdatePathResult::Unmoved);
        assert_eq!(path, vec![2]);

        /* paths in destination */
        let mut path = vec![1, 1];
        assert_eq!(record.update_path(&mut path), UpdatePathResult::Unmoved);
        assert_eq!(path, vec![1, 1]);

        let mut path = vec![1, 2];
        assert_eq!(record.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 4]);

        let mut path = vec![1, 3];
        assert_eq!(record.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 6]);

        let mut path = vec![1, 4];
        assert_eq!(record.update_path(&mut path), UpdatePathResult::Moved);
        assert_eq!(path, vec![1, 7]);
    }
    
    /* This exists to produce errors if another ApplyRecord gets added without corresponding tests. */
    fn update_path_exhaustiveness(ty: ApplyRecord) {
        match ty {
            ApplyRecord::AlterNode { .. } => test_update_path_through_alter_node(),
            ApplyRecord::AlterNodesBulk { .. } => test_update_path_through_alter_nodes_bulk(),
            ApplyRecord::InsertNode { .. } => test_update_path_through_insert_node(),
            ApplyRecord::Nest { .. } => test_update_path_through_nest(),
            ApplyRecord::Destructure { .. } => test_update_path_through_destructure(),
            ApplyRecord::DeleteRange { .. } => test_update_path_through_delete_range(),
            ApplyRecord::StackFilter { .. } => { /* not structural; n/a */ },
            ApplyRecord::Resize { .. } => { /* doesn't affect paths; n/a */ },
            ApplyRecord::Paste { .. } => test_update_path_through_paste(),
            /* Make tests for your new ApplyRecord! */
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

        document::Builder::new(root).build()
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

        document::Builder::new(root).build()
    }

    fn create_test_document_3() -> document::Document {
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
                   .size(0x1c)
                   .child(0x0, |b| b
                          .name("child2.0")
                          .size(0x4))
                   .child(0x4, |b| b
                          .name("child2.1")
                          .size(0x10))
                   .child(0x8, |b| b
                          .name("child2.2")
                          .size(0x10))
                   .child(0xc, |b| b
                          .name("child2.3")
                          .size(0x10)))
            .build();

        document::Builder::new(root).build()
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
    fn test_structural_change_alter_nodes_bulk() {
        let orig_doc = sync::Arc::new(create_test_document_3());

        let mut selection = selection::TreeSelection::new(orig_doc.clone());
        selection.add_single(&[]); /* root */
        selection.add_single(&[1, 2]); /* child 1.2 */
        selection.add_single(&[2]); /* child 2 */
        
        let props = structure::MaybeProperties::new_name("altered".to_string());
        let mut doc = (*orig_doc).clone();
        
        Change {
            ty: ChangeType::AlterNodesBulk { selection: sync::Arc::new(selection), prop_changes: props },
            generation: doc.generation(),
        }.apply(&mut doc).unwrap();

        assert_eq!(doc.root.props.name, "altered");
        assert!(sync::Arc::ptr_eq(doc.lookup_node(&[0]).0, orig_doc.lookup_node(&[0]).0));
        assert_eq!(doc.lookup_node(&[1]).0.props, orig_doc.lookup_node(&[1]).0.props);
        assert!(sync::Arc::ptr_eq(doc.lookup_node(&[1, 0]).0, orig_doc.lookup_node(&[1, 0]).0));
        assert!(sync::Arc::ptr_eq(doc.lookup_node(&[1, 1]).0, orig_doc.lookup_node(&[1, 1]).0));
        assert_eq!(doc.lookup_node(&vec![1, 2]).0.props.name, "altered");
        assert!(sync::Arc::ptr_eq(doc.lookup_node(&[1, 3]).0, orig_doc.lookup_node(&[1, 3]).0));
        assert_eq!(doc.lookup_node(&vec![2]).0.props.name, "altered");
        assert!(sync::Arc::ptr_eq(doc.lookup_node(&[2, 0]).0, orig_doc.lookup_node(&[2, 0]).0));
        assert!(sync::Arc::ptr_eq(doc.lookup_node(&[2, 1]).0, orig_doc.lookup_node(&[2, 1]).0));
        assert!(sync::Arc::ptr_eq(doc.lookup_node(&[2, 2]).0, orig_doc.lookup_node(&[2, 2]).0));
        assert!(sync::Arc::ptr_eq(doc.lookup_node(&[2, 3]).0, orig_doc.lookup_node(&[2, 3]).0));
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
            }.apply(&mut doc), Err(ApplyError { ty: ApplyErrorType::InvalidParameters("attempted to insert node at out-of-bounds index"), .. }));
            assert!(doc.lookup_node(&vec![0]).0.size < 0x21.into());
        }

        {
            let mut doc = doc.clone();
            assert_matches!(Change {
                ty: ChangeType::InsertNode { parent: vec![0], index: 0, child: builder.build_child(addr::Address::from(0x21)) },
                generation: doc.generation(),
            }.apply(&mut doc), Err(ApplyError { ty: ApplyErrorType::InvalidParameters("attempted to insert node beginning beyond parent's size"), .. }));
            assert_eq!(doc.lookup_node(&vec![0]).0.size, 0x20.into());
        }

        {
            let mut doc = document::Builder::new(structure::Node::builder()
                                                 .name("root")
                                                 .size(addr::Offset::MAX)
                                                 .build()).build();
            assert_matches!(Change {
                ty: ChangeType::InsertNode { parent: vec![], index: 0, child: builder.build_child(0xfffffffffffffff8) },
                generation: doc.generation(),
            }.apply(&mut doc), Err(ApplyError { ty: ApplyErrorType::InvalidParameters("attempted to insert node at a place where its end would overflow"), .. }));
        }
        
        {
            let mut doc = doc.clone();
            assert_matches!(Change {
                ty: ChangeType::InsertNode { parent: vec![0], index: 0, child: builder.build_child(addr::Address::from(0x11)) },
                generation: doc.generation(),
            }.apply(&mut doc), Err(ApplyError { ty: ApplyErrorType::InvalidParameters("attempted to insert node extending beyond parent's size"), .. }));
            assert_eq!(doc.lookup_node(&vec![1, 1]).1, addr::Address::from(0x18));
        }

        {
            let mut doc = doc.clone();
            assert_matches!(Change {
                ty: ChangeType::InsertNode { parent: vec![1], index: 0, child: builder.build_child(addr::Address::from(0x2)) },
                generation: doc.generation(),
            }.apply(&mut doc), Err(ApplyError { ty: ApplyErrorType::InvalidParameters("attempted to insert node at an index that would break offset monotonicity"), .. }));
        }

        {
            let mut doc = doc.clone();
            assert_matches!(Change {
                ty: ChangeType::InsertNode { parent: vec![1], index: 2, child: builder.build_child(addr::Address::from(0x2)) },
                generation: doc.generation(),
            }.apply(&mut doc), Err(ApplyError { ty: ApplyErrorType::InvalidParameters("attempted to insert node at an index that would break offset monotonicity"), .. }));
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
            ty: ChangeType::Nest { range: structure::SiblingRange::new(vec![1], 0, 2), extent: addr::Extent::between(0x0, 0x20), props: structure::Properties::default() },
            generation: doc.generation(),
        }.apply(&mut doc.clone()), Err(ApplyError { ty: ApplyErrorType::InvalidRange(structure::RangeInvalidity::IndexExceedsNumberOfChildren), .. }));

        assert_matches!(Change {
            ty: ChangeType::Nest { range: structure::SiblingRange::new(vec![1], 1, 0), extent: addr::Extent::between(0x0, 0x20), props: structure::Properties::default() },
            generation: doc.generation(),
        }.apply(&mut doc.clone()), Err(ApplyError { ty: ApplyErrorType::InvalidRange(structure::RangeInvalidity::Inverted), .. }));

        assert_matches!(Change {
            ty: ChangeType::Nest { range: structure::SiblingRange::new(vec![1], 1, 1), extent: addr::Extent::between(0x5, 0x20), props: structure::Properties::default() },
            generation: doc.generation(),
        }.apply(&mut doc.clone()), Err(ApplyError { ty: ApplyErrorType::InvalidParameters("data extent does not contain all nested children"), .. }));

        assert_matches!(Change {
            ty: ChangeType::Nest { range: structure::SiblingRange::new(vec![1], 0, 1), extent: addr::Extent::between(0x0, 0x13), props: structure::Properties::default() },
            generation: doc.generation(),
        }.apply(&mut doc.clone()), Err(ApplyError { ty: ApplyErrorType::InvalidParameters("data extent does not contain all nested children"), .. }));
    }

    #[test]
    fn test_structural_change_nest() {
        let doc = create_test_document_2();

        let orig_child1 = doc.root.children[1].node.clone();
        let mut props = structure::Properties::default();
        props.name = "nest".to_string();

        let mut new_doc = doc.clone();
        Change {
            ty: ChangeType::Nest { range: structure::SiblingRange::new(vec![1], 1, 2), extent: addr::Extent::between(0x2, 0x18), props: structure::Properties::default() },
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
            ty: ChangeType::Destructure { parent: vec![], child_index: 30 },
            generation: doc.generation(),
        }.apply(&mut doc.clone()), Err(ApplyError { ty: ApplyErrorType::InvalidParameters("attemped to destructure child that doesn't exist"), .. }));
    }
    
    #[test]
    fn test_structural_change_destructure() {
        let doc = create_test_document_2();
        let orig_child1 = doc.root.children[1].node.clone();
        
        let mut new_doc = doc.clone();
        Change {
            ty: ChangeType::Destructure { parent: vec![], child_index: 1 },
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
    fn test_structural_change_destructure_interleaving() {
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
            .child(0x18, |b| b
                   .name("child2")
                   .size(0x4))
            .child(0x19, |b| b
                   .name("child3")
                   .size(0x4))
            .build();

        let doc = document::Builder::new(root).build();

        let mut new_doc = doc.clone();
        Change {
            ty: ChangeType::Destructure { parent: vec![], child_index: 1 },
            generation: doc.generation(),
        }.apply(&mut new_doc).unwrap();

        let expected_mapping = [
            (vec![0], vec![0]), // child0
            (vec![1, 0], vec![1]), // child1.0 at 0x14
            (vec![1, 1], vec![2]), // child1.1 at 0x18
            (vec![2], vec![3]), // child2 at 0x18
            (vec![3], vec![4]), // child3 at 0x19
            (vec![1, 2], vec![5]), // child1.2 at 0x1c
            (vec![1, 3], vec![6]), // child1.3 at 0x20
        ];

        println!("{:#?}", new_doc.root);

        for (old_path, new_path) in &expected_mapping {
            let old_lookup = doc.lookup_node(old_path);
            let new_lookup = new_doc.lookup_node(new_path);
            assert_eq!(old_lookup.1, new_lookup.1);
            assert!(sync::Arc::ptr_eq(&old_lookup.0, &new_lookup.0));
        }
    }
    
    #[test]
    fn test_structural_change_delete_range() {
        let doc = create_test_document_2();

        /* try deleting just child1 */
        let mut new_doc = doc.clone();
        Change {
            ty: ChangeType::DeleteRange { range: structure::SiblingRange::new(vec![], 1, 1) },
            generation: doc.generation(),
        }.apply(&mut new_doc).unwrap();

        assert_eq!(new_doc.root.children.len(), 2);
        assert!(sync::Arc::ptr_eq(&doc.root.children[0].node, &new_doc.root.children[0].node));
        assert!(sync::Arc::ptr_eq(&doc.root.children[2].node, &new_doc.root.children[1].node));

        /* try deleting child1.1 through child1.2 */
        let mut new_doc = doc.clone();
        Change {
            ty: ChangeType::DeleteRange { range: structure::SiblingRange::new(vec![1], 1, 2) },
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

    #[test]
    fn test_structural_change_resize() {
        let doc = create_test_document_2();

        /* Try growing a child and its ancestors */
        let mut new_doc = doc.clone();
        let record = Change {
            ty: ChangeType::Resize { path: vec![1, 2], new_size: 0x100.into(), expand_parents: true, truncate_parents: true },
            generation: doc.generation(),
        }.apply(&mut new_doc).unwrap().1;

        assert_matches!(record, ApplyRecord::Resize { parents_resized: 2, .. });
        assert_eq!(new_doc.root.children[1].node.children[2].node.size, 0x100.into());
        assert_eq!(new_doc.root.children[1].node.size, 0x108.into());
        assert_eq!(new_doc.root.size, 0x11c.into());

        /* Try growing a child beyond its ancestors, to make sure that gets rejected. */
        let mut new_doc = doc.clone();
        assert_matches!(Change {
            ty: ChangeType::Resize { path: vec![1, 2], new_size: 0x100.into(), expand_parents: false, truncate_parents: true },
            generation: doc.generation(),
        }.apply(&mut new_doc), Err(ApplyError { ty: ApplyErrorType::ResizedLargerThanParent, .. }));
        
        /* Try shrinking a child and its ancestors */
        let mut new_doc = doc.clone();
        let record = Change {
            ty: ChangeType::Resize { path: vec![1, 3], new_size: 0x8.into(), expand_parents: true, truncate_parents: true },
            generation: doc.generation(),
        }.apply(&mut new_doc).unwrap().1;

        assert_matches!(record, ApplyRecord::Resize { parents_resized: 1, .. });
        assert_eq!(new_doc.root.children[1].node.children[3].node.size, 0x8.into());
        assert_eq!(new_doc.root.children[1].node.size, 0x18.into()); /* child 1.2 overlaps 1.3, and after shrinking 1.3, ends after 1.3 */
        assert_eq!(new_doc.root.size, 0x40.into());

        /* Try shrinking a parent smaller than its children, to make sure that gets rejected. */
        let mut new_doc = doc.clone();
        assert_matches!(Change {
            ty: ChangeType::Resize { path: vec![1], new_size: 0x1b.into(), expand_parents: true, truncate_parents: true },
            generation: doc.generation(),
        }.apply(&mut new_doc), Err(ApplyError { ty: ApplyErrorType::ResizedSmallerThanChildren, .. }));
    }

    
    #[test]
    fn test_structural_change_paste() {
        let root = structure::Node::builder()
            .name("root")
            .size(0x100)
            .child(0x20, |b| b
                   .name("dst0")
                   .size(0x10))
            .child(0x30, |b| b
                   .name("dst1")
                   .size(0x10))
            .build();

        let paste = structure::Node::builder()
            .name("paste")
            .size(0x1000000)
            .child(0x90, |b| b
                   .name("not_pasted0")
                   .size(0x100))
            .child(0x100, |b| b
                   .name("src0")
                   .size(0x4))
            .child(0x110, |b| b
                   .name("src1")
                   .size(0x4))
            .child(0x110, |b| b
                   .name("not_pasted1")
                   .size(0x100))
            .build();
        
        let old_doc = document::Builder::new(root).build();

        let mut new_doc = old_doc.clone();
        let record = match old_doc.paste(
            paste.clone(),
            (0x100.into(), 1),
            (0x114.into(), 3),
            vec![],
            0x20.into(),
            1
        ).apply(&mut new_doc) {
            Ok((_change, ApplyRecord::Paste(record))) => record,
            e => panic!("did not get valid ApplyRecord::Paste from applying paste operation: {:?}", e),
        };

        assert_eq!(record.parent, vec![]);
        assert_eq!(record.dst_begin, (0x20.into(), 1));
        assert_eq!(record.dst_end, (0x34.into(), 3));
        assert_eq!(record.mapping, vec![1, 2]);

        assert!(sync::Arc::ptr_eq(&old_doc.root.children[0].node, &new_doc.root.children[0].node));
        assert_eq!(old_doc.root.children[0].offset, new_doc.root.children[0].offset);
        assert!(sync::Arc::ptr_eq(&paste.children[1].node, &new_doc.root.children[1].node));
        assert_eq!(new_doc.root.children[1].offset, 0x20.into());
        assert!(sync::Arc::ptr_eq(&paste.children[2].node, &new_doc.root.children[2].node));
        assert_eq!(new_doc.root.children[2].offset, 0x30.into());
        assert!(sync::Arc::ptr_eq(&old_doc.root.children[1].node, &new_doc.root.children[3].node));
        assert_eq!(old_doc.root.children[1].offset, new_doc.root.children[3].offset);
    }

    /* This exists to produce errors if another ChangeType gets added without corresponding tests. */
    fn structural_change_exhaustiveness(ty: ChangeType) {
        match ty {
            ChangeType::AlterNode { .. } => test_structural_change_alter_node(),
            ChangeType::AlterNodesBulk { .. } => test_structural_change_alter_nodes_bulk(),
            ChangeType::InsertNode { .. } => test_structural_change_insert_node(),
            ChangeType::Nest { .. } => test_structural_change_nest(),
            ChangeType::Destructure { .. } => test_structural_change_destructure(),
            ChangeType::DeleteRange { .. } => test_structural_change_delete_range(),
            ChangeType::StackFilter { .. } => { /* n/a; not structural */ },
            ChangeType::Resize { .. } => test_structural_change_resize(),
            ChangeType::Paste { .. } => test_structural_change_paste(),
            /* Make tests for your new ChangeType! */
        }
    }
}
