use std::sync;

use crate::model::document;
use crate::model::document::change as doc_change;
use crate::model::document::structure;
use crate::model::versioned;
use crate::model::versioned::Versioned;

#[derive(Clone)]
pub struct SparseNode {
    self_selected: bool,
    children_selected: ChildrenMode,
}

#[derive(Clone)]
pub enum ChildrenMode {
    None,
    Mixed(Vec<SparseNode>),
    AllDirect,
    AllGrandchildren,
}

#[derive(Clone)]
pub struct Selection {
    pub document: sync::Arc<document::Document>,
    pub root: SparseNode,

    version: versioned::Version<Selection>,
}

enum SparseNodeOrAllGrandchildren<'a> {
    Node(&'a SparseNode),
    AllGrandchildren
}

pub struct TreeIter<'a> {
    selection: &'a Selection,
    stack: Vec<(usize, SparseNodeOrAllGrandchildren<'a>, &'a sync::Arc<structure::Node>)>,
    current_sparse: SparseNodeOrAllGrandchildren<'a>,
    current_struct: &'a sync::Arc<structure::Node>,
    child_index: Option<usize>,
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

    pub fn node_iter(&self) -> TreeIter {
        TreeIter {
            selection: self,
            stack: Vec::new(),
            current_sparse: SparseNodeOrAllGrandchildren::Node(&self.root),
            current_struct: &self.document.root,
            child_index: None,
        }
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

    fn port_doc_change(&mut self, _new_doc: &sync::Arc<document::Document>, _change: &doc_change::Change) -> bool {
        todo!();
    }
}

impl<'a> Iterator for TreeIter<'a> {
    type Item = &'a sync::Arc<structure::Node>;

    fn next(&mut self) -> Option<Self::Item> {
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
                    return Some(self.current_struct);
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
                    /* Push current state onto the stack and descend into the child. */
                    let node = &self.current_struct.children[x].node;
                    self.child_index = None;
                    self.stack.push((
                        x,
                        std::mem::replace(&mut self.current_sparse, SparseNodeOrAllGrandchildren::Node(&vec[x])),
                        std::mem::replace(&mut self.current_struct, node)));
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
                    return Some(&self.current_struct.children[x].node);
                },

                
                (Some(x), SparseNodeOrAllGrandchildren::Node(
                    SparseNode {
                        children_selected: ChildrenMode::AllGrandchildren,
                        self_selected: _,
                    }
                ) | SparseNodeOrAllGrandchildren::AllGrandchildren) => {
                    let node = &self.current_struct.children[x].node;
                    self.child_index = None;
                    self.stack.push((
                        x,
                        std::mem::replace(&mut self.current_sparse, SparseNodeOrAllGrandchildren::AllGrandchildren),
                        std::mem::replace(&mut self.current_struct, node)));
                    continue;
                },
            }
        }
    }
}

impl<'a> TreeIter<'a> {
    /// Returns whether or not anything was able to be popped.
    fn try_pop(&mut self) -> bool {
        match self.stack.pop() {
            None => false,
            Some((index, parent_sparse, parent_actual)) => {
                self.child_index = Some(index + 1);
                self.current_sparse = parent_sparse;
                self.current_struct = parent_actual;
                true
            },
        }        
    }
}

#[derive(Debug, Clone)]
pub enum Change {
    DocumentUpdated(sync::Arc<document::Document>),
    
    Clear,
    
    SetSingle(sync::Arc<document::Document>, structure::Path),
    AddSingle(sync::Arc<document::Document>, structure::Path),
        
    SelectAll,
}

#[derive(Debug, Clone)]
pub struct ChangeRecord {
    pub document_updated: Option<(u64, sync::Arc<document::Document>)>,
    pub selection_changed: bool,
}

#[derive(Debug, Clone)]
pub enum ApplyError {
    NodeDeleted
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
            Change::DocumentUpdated(new_document) => selection.update_document(new_document),

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
                let document_updated = match selection.document.generation() {
                    old_gen if old_gen == document.generation() => None,
                    old_gen => Some((old_gen, document.clone())),
                };
                
                selection.document = document.clone();
                selection.root = SparseNode::new_single(&document.root, &path[..]);

                ChangeRecord {
                    document_updated,
                    selection_changed: true,
                }
            },

            Change::AddSingle(_document, _path) => todo!(),
            
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
    fn new() -> Self {
        Self {
            self_selected: false,
            children_selected: ChildrenMode::None,
        }
    }
    
    fn new_single(node: &structure::Node, remaining_path: structure::PathSlice) -> Self {
        if remaining_path.len() == 0 {
            Self {
                self_selected: true,
                children_selected: ChildrenMode::None,
            }
        } else {
            let mut vec = Vec::new();
            vec.resize(node.children.len(), Self::new());

            vec[remaining_path[0]] = Self::new_single(&node.children[remaining_path[0]].node, &remaining_path[1..]);
            
            Self {
                self_selected: false,
                children_selected: ChildrenMode::Mixed(vec),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
