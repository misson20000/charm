use std::sync;

use crate::model::document;
use crate::model::document::change as doc_change;
use crate::model::document::structure;
use crate::model::versioned;
use crate::model::versioned::Versioned;

#[derive(Clone)]
pub struct Selection {
    pub document: sync::Arc<document::Document>,
    root: SparseNode,

    version: versioned::Version<Selection>,
}

pub trait TreeVisitor<'a> {
    type NodeContext: Sized + 'a;
    type VisitResult: Sized + 'a;

    fn root_context(&mut self, root: &'a sync::Arc<structure::Node>) -> Self::NodeContext;

    /// Returning None skips descending.
    fn descend<'b>(&mut self, parent: &'b mut Self::NodeContext, child: &'a sync::Arc<structure::Node>, child_index: usize, child_selected: bool) -> Option<Self::NodeContext>;
    fn visit_child<'b>(&mut self, parent_context: &'b mut Self::NodeContext, child: &'a sync::Arc<structure::Node>, child_index: usize) -> Self::VisitResult;
    fn visit<'b>(&mut self, context: &'b mut Self::NodeContext, node: &'a sync::Arc<structure::Node>) -> Self::VisitResult;
    fn ascend<'b>(&mut self, parent: &'b mut Self::NodeContext, child: Self::NodeContext, child_index: usize);
}

pub struct TreeWalker<'a, Visitor: TreeVisitor<'a>> {
    selection: &'a Selection,
    stack: Vec<(usize, SparseNodeOrAllGrandchildren<'a>, &'a sync::Arc<structure::Node>, Visitor::NodeContext)>,
    current_sparse: SparseNodeOrAllGrandchildren<'a>,
    current_struct: &'a sync::Arc<structure::Node>,
    current_context: Visitor::NodeContext,
    child_index: Option<usize>,
    visitor: Visitor,
}

pub struct TreeIter<'a>(TreeWalker<'a, ()>);

#[derive(Clone, Debug)]
struct SparseNode {
    self_selected: bool,
    children_selected: ChildrenMode,
}

#[derive(Clone, Debug)]
enum ChildrenMode {
    None,

    Mixed(Vec<SparseNode>),

    /// For nodes with one child and no grandchildren, we use this as the canonical form to make it easier to compact the tree.
    AllDirect,

    /// For nodes with no children, there isn't really any difference between any of these modes. In order to make it easier to compact the tree, we use this as the canonical choice.
    AllGrandchildren,
}

enum SparseNodeOrAllGrandchildren<'a> {
    Node(&'a SparseNode),
    AllGrandchildren
}

impl Selection {
    pub fn new(document: sync::Arc<document::Document>) -> Self {
        Selection {
            document,
            root: SparseNode {
                self_selected: false,
                children_selected: ChildrenMode::None
            },

            version: Default::default(),
        }
    }

    pub fn walker<'a, Visitor: TreeVisitor<'a>>(&'a self, mut visitor: Visitor) -> TreeWalker<'a, Visitor> {
        TreeWalker {
            selection: self,
            stack: Vec::new(),
            current_sparse: SparseNodeOrAllGrandchildren::Node(&self.root),
            current_struct: &self.document.root,
            current_context: visitor.root_context(&self.document.root),
            child_index: None,
            visitor,
        }
    }

    /// If any descendants of the node pointed to by subtree_root are selected,
    /// returns Ok with a TreeWalker that will visit only subtree_root and its
    /// descendants. If no descendants are selected, returns Err with whether
    /// the subtree root itself was selected or not.
    pub fn subtree_walker<'a, Visitor: TreeVisitor<'a>>(&'a self, subtree_root: structure::PathSlice, mut visitor: Visitor) -> Result<TreeWalker<'a, Visitor>, bool> {
        /* None means that the node is selected but none of its children are, so we should report Err(true) */
        let mut sparse_node = Some(SparseNodeOrAllGrandchildren::Node(&self.root));
        let mut struct_node = &self.document.root;

        for child_index in subtree_root {
            sparse_node = match sparse_node {
                None => return Err(false),
                Some(SparseNodeOrAllGrandchildren::Node(SparseNode { self_selected: _, children_selected: ChildrenMode::None })) => return Err(false),
                Some(SparseNodeOrAllGrandchildren::Node(SparseNode { self_selected: _, children_selected: ChildrenMode::Mixed(vec) })) => Some(SparseNodeOrAllGrandchildren::Node(&vec[*child_index])),
                Some(SparseNodeOrAllGrandchildren::Node(SparseNode { self_selected: _, children_selected: ChildrenMode::AllDirect })) => None,
                Some(SparseNodeOrAllGrandchildren::Node(SparseNode { self_selected: _, children_selected: ChildrenMode::AllGrandchildren })) => Some(SparseNodeOrAllGrandchildren::AllGrandchildren),
                Some(SparseNodeOrAllGrandchildren::AllGrandchildren) => Some(SparseNodeOrAllGrandchildren::AllGrandchildren),
            };
            struct_node = &struct_node.children[*child_index].node;
        }

        if let Some(current_sparse) = sparse_node {
            Ok(TreeWalker {
                selection: self,
                stack: Vec::new(),
                current_sparse,
                current_struct: struct_node,
                current_context: visitor.root_context(struct_node),
                child_index: None,
                visitor,
            })
        } else {
            Err(true)
        }
    }
    
    pub fn node_iter(&self) -> TreeIter<'_> {
        TreeIter(self.walker(()))
    }

    
    /// Returns whether or not the node at the specified path is selected.
    pub fn path_selected(&self, path: structure::PathSlice) -> bool {
        self.root.path_selected(path)
    }

    /// Returns whether or not the root node is selected. Equivalent to calling `path_selected(&[])`.
    pub fn root_selected(&self) -> bool {
        self.root.self_selected
    }

    /// Returns true if the selection is not completely empty.
    pub fn any_selected(&self) -> bool {
        self.root.any_selected(&self.document.root)
    }

    /// If only one node is selected, returns the path to it.
    pub fn single_selected(&self) -> Option<structure::Path> {
        self.root.single_selected(&self.document.root, vec![])
    }

    /// If only one contiguous range of siblings within a single node (and either none or all of their grandchildren) is
    /// selected, return that range.
    pub fn one_range_selected(&self) -> Option<structure::SiblingRange> {
        self.root.one_range_selected(&self.document.root, vec![])
    }

    /// Invokes the callback for every range of contiguous selected siblings with no selected ancestors.  Because the
    /// root node doesn't have a parent, it can't be represented by this function's interface and is ignored.
    ///
    /// The callback can cause an early return by returning a [Result::Err].
    pub fn ancestor_ranges_selected<E, F>(&self, mut cb: F) -> Result<(), E>
    where F: FnMut(&structure::SiblingRange) -> Result<(), E> {
        self.root.ancestor_ranges_selected(&self.document.root, &mut vec![], &mut cb)
    }
    
    fn update_document(&mut self, new_doc: &sync::Arc<document::Document>) -> ChangeRecord {
        if self.document.is_outdated(new_doc) {
            let from_generation = self.document.generation();
            let mut selection_changed = false;
            new_doc.changes_since(&self.document.clone(), &mut |new_doc, change| selection_changed|= self.port_doc_change(new_doc, change));

            ChangeRecord {
                document_updated: Some((from_generation, new_doc.clone())),
                selection_changed
            }
        } else {
            ChangeRecord {
                document_updated: None,
                selection_changed: false
            }
        }
    }

    /// Upgrades the internal document reference if it's out of date, but doesn't bother trying to maintain state and just resets it. Returns a ChangeRecord that indicates the selection was changed.
    fn reset_document(&mut self, new_doc: &sync::Arc<document::Document>) -> ChangeRecord {
        if self.document.is_outdated(new_doc) {
            let from_generation = self.document.generation();
            self.document = new_doc.clone();
            self.root = SparseNode {
                self_selected: false,
                children_selected: ChildrenMode::None,
            };

            ChangeRecord {
                document_updated: Some((from_generation, new_doc.clone())),
                selection_changed: true
            }
        } else {
            ChangeRecord {
                document_updated: None,
                selection_changed: true
            }
        }
    }
    
    fn port_doc_change(&mut self, new_doc: &sync::Arc<document::Document>, change: &doc_change::Change) -> bool {
        self.document = new_doc.clone();

        match change.ty {
            doc_change::ChangeType::AlterNode { .. } => false,
            
            _ => {
                // TODO: actually handle structural changes
                self.root = SparseNode {
                    self_selected: false,
                    children_selected: ChildrenMode::None,
                };

                true
            }
        }
    }
}

impl<'a, Visitor: TreeVisitor<'a>> TreeWalker<'a, Visitor> {
    pub fn take(self) -> Visitor::NodeContext {
        self.current_context
    }
    
    pub fn visit_next(&mut self) -> Option<Visitor::VisitResult> {
        loop {
            match (self.child_index, &self.current_sparse) {
                /* If we haven't considered yielding this node yet (self.child_index == None) and either
                this node is selected, or a parent node was set to AllGrandchildren, yield the current node
                and start considering its children. */
                (None, SparseNodeOrAllGrandchildren::Node(SparseNode {
                    children_selected: _,
                    self_selected: true,
                })) | (None, SparseNodeOrAllGrandchildren::AllGrandchildren) => {
                    self.child_index = Some(0);
                    return Some(self.visitor.visit(&mut self.current_context, self.current_struct));
                },

                /* We're considering yielding this node, but the above pattern didn't match so we should skip it and move on to its children. */
                (None, _) => {
                    self.child_index = Some(0);
                    continue;
                },

                /* If we've reached the last child, try to pop. */
                (Some(x), _) if x == self.current_struct.children.len() => match self.try_pop() {
                    /* Reached the end of the tree! */
                    false => return None,
                    /* Continue in parent. */
                    true => continue,
                },

                /* If we're considering children but none of them are selected, pop. */
                (Some(_), SparseNodeOrAllGrandchildren::Node(
                    SparseNode {
                        children_selected: ChildrenMode::None,
                        self_selected: _,
                    }
                )) => match self.try_pop() {
                    false => return None,
                    true => continue,
                },

                /* Mixed selection. Consider each child separately. */
                (Some(x), SparseNodeOrAllGrandchildren::Node(
                    SparseNode {
                        children_selected: ChildrenMode::Mixed(vec),
                        self_selected: _,
                    }
                )) => {
                    /* If neither the child nor any of its children are selected, skip as a special case. */
                    /* This is sad, but helps TreeRewritingVisitor avoid rewriting parts of the tree that it doesn't have to. */
                    let child_sparse = &vec[x];
                    let node = &self.current_struct.children[x].node;
                    if !child_sparse.self_selected && !child_sparse.descendants_maybe_selected(node) {
                        self.child_index = Some(x + 1);
                        continue;
                    }
                    
                    /* Push current state onto the stack and descend into the child. */
                    if let Some(new_context) = self.visitor.descend(&mut self.current_context, node, x, child_sparse.self_selected) {
                        self.child_index = None;
                        self.stack.push((
                            x,
                            std::mem::replace(&mut self.current_sparse, SparseNodeOrAllGrandchildren::Node(&vec[x])),
                            std::mem::replace(&mut self.current_struct, node),
                            std::mem::replace(&mut self.current_context, new_context)));
                    } else {
                        self.child_index = Some(x + 1);
                    }
                    continue;
                },

                /* All direct children should be yielded but none of their descendants. */
                (Some(x), SparseNodeOrAllGrandchildren::Node(
                    SparseNode {
                        children_selected: ChildrenMode::AllDirect,
                        self_selected: _,
                    }
                )) => {
                    self.child_index = Some(x + 1);
                    return Some(self.visitor.visit_child(&mut self.current_context, &self.current_struct.children[x].node, x));
                },

                
                (Some(x), SparseNodeOrAllGrandchildren::Node(
                    SparseNode {
                        children_selected: ChildrenMode::AllGrandchildren,
                        self_selected: _,
                    }
                ) | SparseNodeOrAllGrandchildren::AllGrandchildren) => {
                    let node = &self.current_struct.children[x].node;
                    if let Some(new_context) = self.visitor.descend(&mut self.current_context, node, x, true) {
                        self.child_index = None;
                        self.stack.push((
                            x,
                            std::mem::replace(&mut self.current_sparse, SparseNodeOrAllGrandchildren::AllGrandchildren),
                            std::mem::replace(&mut self.current_struct, node),
                            std::mem::replace(&mut self.current_context, new_context)));
                    } else {
                        self.child_index = Some(x + 1);
                    }
                    continue;
                },
            }
        }
    }

    /// Returns whether or not anything was able to be popped.
    fn try_pop(&mut self) -> bool {
        match self.stack.pop() {
            None => false,
            Some((index, parent_sparse, parent_actual, parent_context)) => {
                self.child_index = Some(index + 1);
                self.current_sparse = parent_sparse;
                self.current_struct = parent_actual;
                let child_context = std::mem::replace(&mut self.current_context, parent_context);
                self.visitor.ascend(&mut self.current_context, child_context, index);
                true
            },
        }
    }
}

impl<'a> TreeVisitor<'a> for () {
    type NodeContext = ();
    type VisitResult = &'a sync::Arc<structure::Node>;

    fn root_context(&mut self, _root: &'a sync::Arc<structure::Node>) -> () {
        ()
    }

    fn descend<'b>(&mut self, _parent: &'b mut Self::NodeContext, _child: &'a sync::Arc<structure::Node>, _child_index: usize, _child_selected: bool) -> Option<()> {
        Some(())
    }

    fn visit_child<'b>(&mut self, _parent_context: &'b mut Self::NodeContext, child: &'a sync::Arc<structure::Node>, _child_index: usize) -> Self::VisitResult {
        child
    }

    fn visit<'b>(&mut self, _context: &'b mut Self::NodeContext, node: &'a sync::Arc<structure::Node>) -> Self::VisitResult {
        node
    }

    fn ascend<'b>(&mut self, _parent: &'b mut Self::NodeContext, _child: Self::NodeContext, _child_index: usize) {
    }
}

impl<'a> Iterator for TreeIter<'a> {
    type Item = &'a sync::Arc<structure::Node>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.visit_next()
    }
}

#[derive(Debug, Clone)]
pub enum Change {
    DocumentUpdated(sync::Arc<document::Document>),
    
    Clear,
    
    SetSingle(sync::Arc<document::Document>, structure::Path),
    AddSingle(sync::Arc<document::Document>, structure::Path),
    RemoveSingle(sync::Arc<document::Document>, structure::Path),
        
    SelectAll,
}

#[derive(Debug, Clone)]
pub struct ChangeRecord {
    pub document_updated: Option<(u64, sync::Arc<document::Document>)>,
    pub selection_changed: bool,
}

#[derive(Debug, Clone)]
pub enum ApplyError {
    WasUpToDate,
    NodeDeleted,
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

pub type Host = versioned::Host<Selection>;

impl versioned::Change<Selection> for Change {
    type ApplyError = ApplyError;
    type ApplyRecord = ChangeRecord;

    fn apply(mut self, selection: &mut Selection) -> Result<(Change, ChangeRecord), ApplyError> {
        let record = match &mut self {
            Change::DocumentUpdated(new_document) => if selection.document.is_outdated(new_document) {
                selection.update_document(new_document)
            } else {
                return Err(ApplyError::WasUpToDate);
            },

            Change::Clear => {
                selection.root = SparseNode {
                    self_selected: false,
                    children_selected: ChildrenMode::None,
                };

                ChangeRecord {
                    document_updated: None,
                    selection_changed: true,
                }
            },

            Change::SetSingle(document, path) => {
                /* Handle outdated Selection document */
                let cr = selection.reset_document(document);

                /* Handle outdated Change document */
                selection.document.changes_since_fallible(&document.clone(), &mut |doc, change| {
                    *document = doc.clone();
                    match change.update_path(path) {
                        doc_change::UpdatePathResult::Unmoved | doc_change::UpdatePathResult::Moved => Ok(()),
                        doc_change::UpdatePathResult::Deleted | doc_change::UpdatePathResult::Destructured => Err(ApplyError::NodeDeleted),
                    }
                })?;
                
                /* Apply change */
                selection.root = SparseNode::new_single(&document.root, &path[..]);

                cr
            },

            Change::AddSingle(document, path) => {
                /* Handle outdated Selection document */
                let mut cr = selection.update_document(document);

                /* Handle outdated Change document */
                selection.document.changes_since_fallible(&document.clone(), &mut |doc, change| {
                    *document = doc.clone();
                    match change.update_path(path) {
                        doc_change::UpdatePathResult::Unmoved | doc_change::UpdatePathResult::Moved => Ok(()),
                        doc_change::UpdatePathResult::Deleted | doc_change::UpdatePathResult::Destructured => Err(ApplyError::NodeDeleted),
                    }
                })?;

                /* Apply change */
                selection.root.add_single(&document.root, &path[..]);
                
                cr.selection_changed = true;
                
                cr
            },
            
            Change::RemoveSingle(document, path) => {
                /* Handle outdated Selection document */
                let mut cr = selection.update_document(document);

                /* Handle outdated Change document */
                selection.document.changes_since_fallible(&document.clone(), &mut |doc, change| {
                    *document = doc.clone();
                    match change.update_path(path) {
                        doc_change::UpdatePathResult::Unmoved | doc_change::UpdatePathResult::Moved => Ok(()),
                        doc_change::UpdatePathResult::Deleted | doc_change::UpdatePathResult::Destructured => Err(ApplyError::NodeDeleted),
                    }
                })?;

                /* Apply change */
                selection.root.remove_single(&document.root, &path[..]);
                
                cr.selection_changed = true;
                
                cr
            },
            
            Change::SelectAll => {
                selection.root = SparseNode {
                    self_selected: true,
                    children_selected: ChildrenMode::AllGrandchildren,
                };

                ChangeRecord {
                    document_updated: None,
                    selection_changed: true,
                }
            },
        };

        Ok((self, record))
    }
}

impl SparseNode {
    fn new_canonical_unselected(node: &structure::Node) -> Self {
        Self {
            self_selected: false,
            children_selected: if node.children.len() == 0 {
                ChildrenMode::AllGrandchildren
            } else {
                ChildrenMode::None
            },
        }
    }
    
    fn new_canonical_selected(node: &structure::Node) -> Self {
        Self {
            self_selected: true,
            children_selected: if node.children.len() == 0 {
                ChildrenMode::AllGrandchildren
            } else {
                ChildrenMode::None
            },
        }
    }
    
    fn new_single(node: &structure::Node, remaining_path: structure::PathSlice) -> Self {
        if remaining_path.len() == 0 {
            Self {
                self_selected: true,
                children_selected: if node.children.len() == 0 {
                    ChildrenMode::AllGrandchildren
                } else {
                    ChildrenMode::None
                },
            }
        } else {
            let mut vec: Vec<Self> = node.children.iter().map(|childhood| Self::new_canonical_unselected(&childhood.node)).collect();
            vec[remaining_path[0]] = Self::new_single(&node.children[remaining_path[0]].node, &remaining_path[1..]);
            
            Self {
                self_selected: false,
                children_selected: ChildrenMode::Mixed(vec),
            }
        }
    }

    fn add_single(&mut self, node: &structure::Node, remaining_path: structure::PathSlice) {
        if remaining_path.len() == 0 {
            self.self_selected = true;
        } else {
            match &mut self.children_selected {
                /* Nothing needs to be done. */
                ChildrenMode::AllGrandchildren => (),
                ChildrenMode::AllDirect if remaining_path.len() == 1 => (),

                ChildrenMode::None => {
                    let mut vec: Vec<Self> = node.children.iter().map(|childhood| Self::new_canonical_unselected(&childhood.node)).collect();
                    vec[remaining_path[0]] = Self::new_single(&node.children[remaining_path[0]].node, &remaining_path[1..]);
                    self.children_selected = ChildrenMode::Mixed(vec);
                },

                ChildrenMode::AllDirect => {
                    let mut vec: Vec<Self> = node.children.iter().map(|childhood| Self::new_canonical_selected(&childhood.node)).collect();
                    vec[remaining_path[0]].add_single(&node.children[remaining_path[0]].node, &remaining_path[1..]);
                    self.children_selected = ChildrenMode::Mixed(vec);
                },

                ChildrenMode::Mixed(vec) => {
                    vec[remaining_path[0]].add_single(&node.children[remaining_path[0]].node, &remaining_path[1..]);
                },
            }
        }

        self.canonicalize(node);
    }

    fn remove_single(&mut self, node: &structure::Node, remaining_path: structure::PathSlice) {
        if remaining_path.len() == 0 {
            self.self_selected = false;
        } else {
            match &mut self.children_selected {
                /* Nothing needs to be done. */
                ChildrenMode::None => (),

                ChildrenMode::Mixed(vec) => {
                    vec[remaining_path[0]].remove_single(&node.children[remaining_path[0]].node, &remaining_path[1..]);
                },

                ChildrenMode::AllDirect => {
                    let mut vec: Vec<Self> = node.children.iter().map(|childhood| Self::new_canonical_selected(&childhood.node)).collect();
                    vec[remaining_path[0]].remove_single(&node.children[remaining_path[0]].node, &remaining_path[1..]);
                    self.children_selected = ChildrenMode::Mixed(vec);
                },

                ChildrenMode::AllGrandchildren => {
                    let mut vec = Vec::new();
                    vec.resize(node.children.len(), Self {
                        self_selected: true,
                        children_selected: ChildrenMode::AllGrandchildren,
                    });
                    vec[remaining_path[0]].remove_single(&node.children[remaining_path[0]].node, &remaining_path[1..]);
                    self.children_selected = ChildrenMode::Mixed(vec);
                },
            }
        }

        self.canonicalize(node);
    }

    fn descendants_maybe_selected(&self, node: &structure::Node) -> bool {
        match (node.children.len(), &self.children_selected) {
            (0, _) => false,
            (_, ChildrenMode::None) => false,
            _ => true
        }
    }
    
    fn canonicalize(&mut self, node: &structure::Node) {
        loop {
            match &mut self.children_selected {
                /* If we don't have any children, then the children mode doesn't matter. Upgrade it as much as possible. */
                _ if node.children.len() == 0 => {
                    self.children_selected = ChildrenMode::AllGrandchildren;
                    return;
                }

                /* If we don't have any grandchildren, upgrade AllDirect to AllGrandchildren. */
                ChildrenMode::AllDirect if node.children.iter().all(|ch| ch.node.children.len() == 0) => {
                    self.children_selected = ChildrenMode::AllGrandchildren;
                    return;
                },

                ChildrenMode::Mixed(vec) => {
                    /* If none of our mixed entries are selected or might have children selected, change to None. */
                    if !vec.iter().zip(node.children.iter()).any(
                        |(sparse_child, struct_child)|
                        sparse_child.descendants_maybe_selected(&struct_child.node) ||
                            sparse_child.self_selected) {
                        self.children_selected = ChildrenMode::None;
                        continue;
                    }

                    /* If all of our mixed entries are selected and AllGrandchildren, propagate AllGrandchildren to us. */
                    if vec.iter().all(
                        |sparse_child|
                        sparse_child.self_selected && sparse_child.children_selected.is_all_grandchildren()) {
                        self.children_selected = ChildrenMode::AllGrandchildren;
                        continue;
                    }
                    
                    /* If all of our mixed entries are selected but none of their descendants are, change to AllDirect. */
                    if vec.iter().zip(node.children.iter()).all(
                        |(sparse_child, struct_child)|
                        sparse_child.self_selected &&
                            !sparse_child.descendants_maybe_selected(&struct_child.node)) {
                        self.children_selected = ChildrenMode::AllDirect;
                        continue;
                    }

                    return;
                },

                /* No more optimizations to run. */
                _ => return,
            }
        }
    }

    fn path_selected(&self, remaining_path: structure::PathSlice) -> bool {
        if remaining_path.len() == 0 {
            self.self_selected
        } else {
            match &self.children_selected {
                ChildrenMode::None => false,
                ChildrenMode::Mixed(vec) => vec[remaining_path[0]].path_selected(&remaining_path[1..]),
                ChildrenMode::AllDirect if remaining_path.len() == 1 => true,
                ChildrenMode::AllDirect => false,
                ChildrenMode::AllGrandchildren => true,
            }
        }
    }

    fn any_selected(&self, node: &structure::Node) -> bool {
        self.self_selected || match &self.children_selected {
            ChildrenMode::None => false,
            ChildrenMode::Mixed(vec) => vec.iter().zip(node.children.iter()).any(|(sp, st)| sp.any_selected(&st.node)),
            ChildrenMode::AllDirect => node.children.len() > 0,
            ChildrenMode::AllGrandchildren => node.children.len() > 0,
        }
    }

    fn none_selected(&self, node: &structure::Node) -> bool {
        !self.any_selected(node)
    }

    fn single_selected(&self, node: &structure::Node, mut path: structure::Path) -> Option<structure::Path> {
        match (self.self_selected, &self.children_selected) {
            (true, ChildrenMode::None) => Some(path),
            (true, _) if node.children.len() == 0 => Some(path),

            (false, ChildrenMode::AllDirect) if node.children.len() == 1 => {
                path.push(0);
                Some(path)
            },

            (false, ChildrenMode::AllGrandchildren) if node.children.len() == 1 && node.children[0].node.children.len() == 0 => {
                path.push(0);
                Some(path)
            },

            (false, ChildrenMode::Mixed(vec)) => {
                let mut single_found = None;
                for (i, (sparse_child, struct_child)) in vec.iter().zip(node.children.iter()).enumerate() {
                    if sparse_child.self_selected || sparse_child.descendants_maybe_selected(&struct_child.node) {
                        if single_found.is_some() {
                            /* More than one child was selected. */
                            return None;
                        } else {
                            single_found = Some((i, sparse_child, &struct_child.node));
                        }
                    }
                }
                
                single_found.and_then(|(i, sparse_child, struct_child)| {
                    path.push(i);
                    sparse_child.single_selected(struct_child, path)
                })
            },

            _ => None,
        }
    }

    fn one_range_selected(&self, node: &structure::Node, mut path: structure::Path) -> Option<structure::SiblingRange> {
        if self.self_selected {
            None
        } else {
            match &self.children_selected {
                ChildrenMode::None => None,
                ChildrenMode::AllDirect if node.children.len() == 0 => None,
                ChildrenMode::AllGrandchildren if node.children.len() == 0 => None,
                
                ChildrenMode::Mixed(vec) => {
                    let mut start = None;
                    let mut end = None;

                    for (i, (sparse_child, struct_child)) in vec.iter().zip(node.children.iter()).enumerate() {
                        match start {
                            None => {
                                if !sparse_child.self_selected && sparse_child.descendants_maybe_selected(&struct_child.node) {
                                    path.push(i);
                                    return sparse_child.one_range_selected(&struct_child.node, path);
                                }
                                
                                if sparse_child.self_selected {
                                    if !match &sparse_child.children_selected {
                                        /* Is this sparse child suitable for being part of a sibling range? */
                                        ChildrenMode::None => true,
                                        ChildrenMode::Mixed(_) => false,
                                        ChildrenMode::AllDirect => false,
                                        ChildrenMode::AllGrandchildren => true,
                                    } {
                                        return None;
                                    }

                                    start = Some((i, sparse_child.children_selected.is_all_grandchildren()));
                                }
                            },
                            Some((_, all_grandchildren)) => {
                                /* If we think we've already found the end of the range, we need to make sure nothing beyond that end is selected. */
                                if end.is_some() {
                                    if sparse_child.self_selected || sparse_child.descendants_maybe_selected(&struct_child.node) {
                                        return None;
                                    } else {
                                        continue;
                                    }
                                }
                                
                                if sparse_child.self_selected {
                                    /* Is this sparse child suitable for being part of the sibling range we already started? */
                                    if !match &sparse_child.children_selected {
                                        ChildrenMode::None => !all_grandchildren,
                                        ChildrenMode::Mixed(_) => false,
                                        ChildrenMode::AllDirect => false,
                                        ChildrenMode::AllGrandchildren => all_grandchildren || struct_child.node.children.len() == 0,
                                    } {
                                        return None;
                                    }
                                } else if !sparse_child.any_selected(&struct_child.node) {
                                    /* Not selected, and none of its children are. Must be the end. */
                                    end = Some(i-1);
                                } else {
                                    /* Not selected, but some children were, so invalid. */
                                    return None;
                                }
                            },
                        }
                    }

                    start.map(|(begin, _)| structure::SiblingRange::new(path, begin, end.unwrap_or(node.children.len()-1)))
                },
                
                ChildrenMode::AllDirect => Some(structure::SiblingRange::new(path, 0, node.children.len()-1)),
                ChildrenMode::AllGrandchildren => Some(structure::SiblingRange::new(path, 0, node.children.len()-1)),
            }
        }
    }

    /// This function can mutate path, but has to put it back to the way it was before returning.
    pub fn ancestor_ranges_selected<E, F>(&self, node: &structure::Node, path: &mut structure::Path, cb: &mut F) -> Result<(), E>
    where F: FnMut(&structure::SiblingRange) -> Result<(), E> {
        match &self.children_selected {
            ChildrenMode::None => Ok(()),
            ChildrenMode::AllDirect if node.children.len() == 0 => Ok(()),
            ChildrenMode::AllGrandchildren if node.children.len() == 0 => Ok(()),
            
            ChildrenMode::Mixed(vec) => {
                let mut range = None;

                for (i, (sparse_child, struct_child)) in vec.iter().zip(node.children.iter()).enumerate() {
                    match range {
                        /* Not part of a range. */
                        None if !sparse_child.self_selected => {
                            path.push(i);
                            let r = sparse_child.ancestor_ranges_selected(&struct_child.node, path, cb);
                            path.pop();
                            r?;
                        },
                        
                        /* Start of a range */
                        None => {
                            range = Some((i, i));
                        },

                        /* Middle of a range */
                        Some((start_index, _)) if sparse_child.self_selected => {
                            range = Some((start_index, i));
                        },

                        /* End of a range */
                        Some((start_index, end_index)) => {
                            let range_obj = structure::SiblingRange::new(std::mem::replace(path, Vec::new()), start_index, end_index);
                            cb(&range_obj)?;
                            /* Fish the vector back out so we can use it again. */
                            *path = range_obj.parent;
                            
                            range = None;

                            path.push(i);
                            let r = sparse_child.ancestor_ranges_selected(&struct_child.node, path, cb);
                            path.pop();
                            r?;
                        },
                    }
                }

                if let Some((start_index, end_index)) = range {
                    let range_obj = structure::SiblingRange::new(std::mem::replace(path, Vec::new()), start_index, end_index);
                    cb(&range_obj)?;
                    *path = range_obj.parent;
                }

                Ok(())
            },

            ChildrenMode::AllDirect | ChildrenMode::AllGrandchildren => {
                let range_obj = structure::SiblingRange::new(std::mem::replace(path, Vec::new()), 0, node.children.len()-1);
                cb(&range_obj)?;
                *path = range_obj.parent;

                Ok(())
            },
        }
    }
}

impl ChildrenMode {
    fn is_all_grandchildren(&self) -> bool {
        match self {
            ChildrenMode::AllGrandchildren => true,
            _ => false
        }
    }
}

impl std::fmt::Debug for Selection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("TreeSelection [")?;

        for (i, node) in self.node_iter().enumerate() {
            if i > 0 {
                f.write_str(", ")?;
            }
            
            if i > 5 {
                f.write_str("...")?;
                break;
            }

            f.write_fmt(format_args!("\"{}\"", node.props.name))?;
        }

        f.write_str("]")?;
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_simple_test_structure() -> sync::Arc<structure::Node> {
        structure::Node::builder()
            .name("root")
            .child(0x0, |b| b
                   .name("child0"))
            .child(0x0, |b| b
                   .name("child1")
                   .child(0x0, |b| b
                          .name("child1.0")
                          .child(0x0, |b| b
                                 .name("child1.0.0"))
                          .child(0x0, |b| b
                                 .name("child1.0.1"))
                          .child(0x0, |b| b
                                 .name("child1.0.2"))))
            .build()
    }
    
    #[test]
    fn test_create_sparse_node_for_single() {
        let root = create_simple_test_structure();
        let document = sync::Arc::new(document::Document::new_for_structure_test(root));

        for path in [
            /* Test SparseNode::new_single for each of these paths */
            vec![0],
            vec![1],
            vec![1, 0],
            vec![1, 0, 0],
            vec![1, 0, 1],
            vec![1, 0, 2],
        ] {
            /* We'll test it by making a selection with the SparseNode and iterating over all the nodes in the
             * selection. There should only ever be one. */
            itertools::assert_equal(Selection {
                document: document.clone(),
                root: SparseNode::new_single(&document.root, &path),
                version: Default::default(),
            }.node_iter().map(|node| sync::Arc::as_ptr(node)), [sync::Arc::as_ptr(document.lookup_node(&path).0)]);
        }
    }

    #[test]
    fn test_add_single() {
        let root = create_simple_test_structure();
        let document = sync::Arc::new(document::Document::new_for_structure_test(root));

        let paths = [
            vec![0],
            vec![1],
            vec![1, 0],
            vec![1, 0, 0],
            vec![1, 0, 1],
            vec![1, 0, 2],
        ];

        {
            let mut paths_in_selection = vec![];
            let mut selection = Selection::new(document.clone());

            for path in paths.iter() {
                paths_in_selection.push(path);
                selection.root.add_single(&document.root, &path[..]);
                
                itertools::assert_equal(selection.node_iter().map(|node| sync::Arc::as_ptr(node)), paths_in_selection.iter().map(|path| sync::Arc::as_ptr(document.lookup_node(&path[..]).0)));
            }
        }

        {
            let mut paths_in_selection = vec![];
            let mut selection = Selection::new(document.clone());

            for path in paths.iter().rev() {
                paths_in_selection.insert(0, path);
                selection.root.add_single(&document.root, &path[..]);
                
                itertools::assert_equal(selection.node_iter().map(|node| sync::Arc::as_ptr(node)), paths_in_selection.iter().map(|path| sync::Arc::as_ptr(document.lookup_node(&path[..]).0)));
            }
        }        
    }

    #[test]
    fn test_remove_single() {
        let root = create_simple_test_structure();
        let document = sync::Arc::new(document::Document::new_for_structure_test(root));

        let paths = vec![
            vec![],
            vec![0],
            vec![1],
            vec![1, 0],
            vec![1, 0, 0],
            vec![1, 0, 1],
            vec![1, 0, 2],
        ];

        {
            let mut paths_in_selection: Vec<&Vec<usize>> = paths.iter().collect();
            let mut selection = Selection::new(document.clone());
            selection.root.self_selected = true;
            selection.root.children_selected = ChildrenMode::AllGrandchildren;

            for path in paths.iter() {
                paths_in_selection.retain(|candidate| !std::ptr::eq(*candidate, path));
                
                selection.root.remove_single(&document.root, &path[..]);
                
                itertools::assert_equal(selection.node_iter().map(|node| sync::Arc::as_ptr(node)), paths_in_selection.iter().map(|path| sync::Arc::as_ptr(document.lookup_node(&path[..]).0)));
            }
        }

        {
            let mut paths_in_selection: Vec<&Vec<usize>> = paths.iter().collect();
            let mut selection = Selection::new(document.clone());
            selection.root.self_selected = true;
            selection.root.children_selected = ChildrenMode::AllGrandchildren;

            for path in paths.iter().rev() {
                paths_in_selection.retain(|candidate| !std::ptr::eq(*candidate, path));
                
                selection.root.remove_single(&document.root, &path[..]);
                
                itertools::assert_equal(selection.node_iter().map(|node| sync::Arc::as_ptr(node)), paths_in_selection.iter().map(|path| sync::Arc::as_ptr(document.lookup_node(&path[..]).0)));
            }
        }
    }
    
    mod iter {
        use super::*;
        
        #[test]
        fn test_iter_correctness() {
            let root = structure::Node::builder()
                /* Selected, mixed */
                .name("root")
                .child(0x0, |b| b
                       /* Not selected, AllDirect */
                       .name("child0")
                       .child(0x0, |b| b
                              /* Selected because parent is AllDirect */
                              .name("child0.0")
                              .child(0x0, |b| b
                                     /* Not selected because it's a grandchild of an AllDirect */
                                     .name("child0.0.0"))
                              .child(0x0, |b| b
                                     /* Not selected because it's a grandchild of an AllDirect */
                                     .name("child0.0.1")))
                       .child(0x0, |b| b
                              /* Selected because parent is AllDirect */
                              .name("child0.1")))
                .child(0x0, |b| b
                       /* Selected, None */
                       .name("child1")
                       .child(0x0, |b| b
                              /* Not selected because parent is None */
                              .name("child1.0")
                              .child(0x0, |b| b
                                     /* Not selected because ancestor is None */
                                     .name("child1.0.0"))))
                .child(0x0, |b| b
                       /* Selected, AllGrandchildren */
                       .name("child2")
                       .child(0x0, |b| b
                              /* Selected because ancestor is AllGrandchildren */
                              .name("child2.0")
                              .child(0x0, |b| b
                                     /* Selected because ancestor is AllGrandchildren */
                                     .name("child2.0.0"))
                              .child(0x0, |b| b
                                     /* Selected because ancestor is AllGrandchildren */
                                     .name("child2.0.1")))
                       .child(0x0, |b| b
                              /* Selected because ancestor is AllGrandchildren */
                              .name("child2.1")
                              .child(0x0, |b| b
                                     /* Selected because ancestor is AllGrandchildren */
                                     .name("child2.1.0"))))
                .child(0x0, |b| b
                       /* Selected, Mixed */
                       .name("child3")
                       .child(0x0, |b| b
                              /* Not selected, None */
                              .name("child3.0"))
                       .child(0x0, |b| b
                              /* Selected, None */
                              .name("child3.1"))
                       .child(0x0, |b| b
                              /* Selected, AllGrandchildren */
                              .name("child3.2")))
                .build();

            let root_sparse = SparseNode {
                self_selected: true,
                children_selected: ChildrenMode::Mixed(vec![
                    /* child0 */
                    SparseNode {
                        self_selected: false,
                        children_selected: ChildrenMode::AllDirect,
                    },
                    /* child1 */
                    SparseNode {
                        self_selected: true,
                        children_selected: ChildrenMode::None,
                    },
                    /* child2 */
                    SparseNode {
                        self_selected: true,
                        children_selected: ChildrenMode::AllGrandchildren,
                    },
                    /* child3 */
                    SparseNode {
                        self_selected: true,
                        children_selected: ChildrenMode::Mixed(vec![
                            /* child3.0 */
                            SparseNode {
                                self_selected: false,
                                children_selected: ChildrenMode::None,
                            },
                            /* child3.1 */
                            SparseNode {
                                self_selected: true,
                                children_selected: ChildrenMode::None,
                            },
                            /* child3.2 */
                            SparseNode {
                                self_selected: true,
                                children_selected: ChildrenMode::AllGrandchildren,
                            },
                        ]),
                    }
                ]),
            };

            let selection = Selection {
                document: sync::Arc::new(document::Document::new_for_structure_test(root)),
                root: root_sparse,
                version: Default::default(),
            };
            
            itertools::assert_equal(
                selection.node_iter().map(|node| &node.props.name),
                &[
                    "root",
                    "child0.0",
                    "child0.1",
                    "child1",
                    "child2",
                    "child2.0",
                    "child2.0.0",
                    "child2.0.1",
                    "child2.1",
                    "child2.1.0",
                    "child3",
                    "child3.1",
                    "child3.2"
                ])
        }
    }
}
