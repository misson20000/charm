use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;

use std::sync;

#[derive(Debug, Clone, Copy)]
pub enum Traversal {
    PreOrder,
    PostOrder,
    LeavesOnly
}

#[derive(Debug, Clone)]
struct TraversalStackEntry<'a> {
    node: &'a sync::Arc<structure::Node>,
    node_addr: addr::Address,
    is_leaf: bool,
    child: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct AddressSearch<'a> {
    document: &'a document::Document,
    needle: addr::Address,
    traversal: Traversal,

    stack: Vec<TraversalStackEntry<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Hit {
    pub path: structure::Path,
    pub offset: addr::Size,
}

#[derive(Debug, Clone, Copy)]
pub enum SetupError {
    NeedleNotInHaystack
}

impl<'a> AddressSearch<'a> {
    pub fn new(document: &'a document::Document, needle: addr::Address, traversal: Traversal) -> Result<AddressSearch<'a>, SetupError> {
        if needle >= addr::unit::NULL + document.root.size {
            return Err(SetupError::NeedleNotInHaystack);
        }
        
        Ok(AddressSearch {
            document,
            needle,
            traversal,

            stack: vec![TraversalStackEntry {
                node: &document.root,
                node_addr: addr::unit::NULL,
                is_leaf: true,
                child: None,
            }],
        })
    }

    /*
    If I had generators, I would write it roughly like this:

    fn generate_recursive(path, node) {
        if preorder {
            yield path, node
        }

        let mut is_leaf = true;
        for ch in node.children {
            if ch contains needle {
                generate_recursive(path .. index, ch)
                is_leaf = false;
            }
        }

        if postorder || leaves_only && is_leaf {
            yield path, node
        }
    }
     */

    fn child_contains(child: &structure::Childhood, parent_node_addr: addr::Address, needle: addr::Address) -> bool {
        addr::Extent::sized(parent_node_addr + child.offset.to_size(), child.node.size).includes(needle)
    }
    
}

impl<'a> Iterator for AddressSearch<'a> {
    type Item = Hit;
    
    fn next(&mut self) -> Option<Hit> {
        loop {
            let state = match self.stack.last_mut() {
                Some(entry) => entry,
                None => return None
            };

            match &mut state.child {
                None => {
                    state.child = Some(0);
                    
                    match self.traversal {
                        Traversal::PreOrder => return Some(Hit {
                            offset: self.needle - state.node_addr,
                            path: self.stack.iter().enumerate().filter_map(|(i, e)| if (i+1) < self.stack.len() { e.child } else { None }).map(|i| i-1).collect(),
                        }),
                        _ => {}
                    };
                },
                Some(ref mut index) if *index < state.node.children.len() => {
                    let old_index = *index;
                    *index+= 1;
                    
                    if Self::child_contains(&state.node.children[old_index], state.node_addr, self.needle) {
                        state.is_leaf = false;

                        let entry = TraversalStackEntry {
                            node: &state.node.children[old_index].node,
                            node_addr: state.node_addr + state.node.children[old_index].offset.to_size(),
                            is_leaf: true,
                            child: None,
                        };
                        
                        self.stack.push(entry);
                    }
                }
                Some(_) => {
                    let state = self.stack.pop().unwrap();
                    
                    if match self.traversal {
                        Traversal::PostOrder => true,
                        Traversal::LeavesOnly if state.is_leaf => true,
                        _ => false
                    } {
                        return Some(Hit {
                            offset: self.needle - state.node_addr,
                            path: self.stack.iter().filter_map(|e| e.child).map(|i| i-1).collect(),
                        });
                    }
                }
            }
        }

        
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::sync;

    fn test_document_1() -> sync::Arc<document::Document> {
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

        document::Builder::new(root).arc()
    }
    
    #[test]
    fn basic_preorder() {
        assert_eq!(
            test_document_1().search_addr(0x1a, Traversal::PreOrder).unwrap().collect::<Vec<Hit>>(),
            vec![
                Hit { path: vec![],     offset: 0x1a.into() },
                Hit { path: vec![0],    offset:  0xa.into() },
                Hit { path: vec![1],    offset:  0x6.into() },
                /* child1.0 should not be considered since it doesn't include the needle */
                Hit { path: vec![1, 1], offset:  0x2.into() },
                /* child2 should not be considered */
            ]);
    }

    #[test]
    fn basic_postorder() {
        assert_eq!(
            test_document_1().search_addr(0x1a, Traversal::PostOrder).unwrap().collect::<Vec<Hit>>(),
            vec![
                Hit { path: vec![0],    offset:  0xa.into() },
                /* child1.0 should not be considered since it doesn't include the needle */
                Hit { path: vec![1, 1], offset:  0x2.into() },
                Hit { path: vec![1],    offset:  0x6.into() },
                /* child2 should not be considered */
                Hit { path: vec![],     offset: 0x1a.into() },
            ]);
    }

    #[test]
    fn basic_leaves_only() {
        assert_eq!(
            test_document_1().search_addr(0x1a, Traversal::LeavesOnly).unwrap().collect::<Vec<Hit>>(),
            vec![
                Hit { path: vec![0],    offset:  0xa.into() },
                /* child1.0 should not be considered since it doesn't include the needle */
                Hit { path: vec![1, 1], offset:  0x2.into() },
                /* child1 is not a leaf so it won't be reported */
                /* child2 should not be considered */
                /* root is not a leaf so it won't be reported */
            ]);
    }
}
