use std::fmt::Write;
use std::sync;
use std::vec;

use lazy_static::lazy_static;

use crate::model::addr;

pub mod props;
pub mod builder;
pub mod path;

pub use path::Path;
pub use path::PathSlice;
pub use path::PathIter;
pub use path::SiblingRange;
pub use path::RangeInvalidity;
pub use props::MaybeProperties;
pub use props::Properties;
pub use props::TitleDisplay;
pub use props::ChildrenDisplay;
pub use props::ContentDisplay;

#[derive(Debug, Clone)]
pub struct Childhood {
    pub node: sync::Arc<Node>,
    pub offset: addr::Offset,
}

impl Childhood {
    pub fn new(node: sync::Arc<Node>, offset: addr::Offset) -> Childhood {
        Childhood { node, offset }
    }
    
    pub fn end(&self) -> addr::Offset {
        self.offset + self.node.size
    }

    pub fn extent(&self) -> addr::Extent {
        addr::Extent::sized(self.offset, self.node.size)
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    /* reference to parent causes a lot of problems, so we don't have one and
       opt to refer to nodes by path when necessary. */
    pub props: props::Properties,
    pub size: addr::Offset,
    pub children: vec::Vec<Childhood>
}

impl Node {
    pub fn default_sized(size: addr::Offset) -> Node {
        Node {
            props: props::Properties::default(),
            size,
            children: vec::Vec::new(),
        }
    }

    pub fn builder() -> builder::StructureBuilder {
        builder::StructureBuilder::default()
    }

    pub fn child_at_offset(&self, offset: addr::Offset) -> usize {
        self.children.partition_point(|ch| ch.offset < offset)
    }

    pub fn successor(&self, path: &mut Path, index: usize) -> bool {
        if path.len() > index {
            if self.children[path[index]].node.successor(path, index+1) {
                return true;
            }
            path.resize(index+1, 0);
            path[index]+= 1;
        } else {
            assert!(path.len() == index);
            path.push(0);
        }

        if path[index] >= self.children.len() {
            return false;
        }

        true
    }

    pub fn assert_validity(&self) {
        let mut last_offset = addr::Offset::ZERO;

        let mut bad_child = None;
        
        for (i, child) in self.children.iter().enumerate() {
            if child.offset < last_offset || child.end() > self.size {
                if bad_child.is_none() {
                    bad_child = Some(i);
                }
            }
            last_offset = child.offset;
        }

        if let Some(bad_index) = bad_child {
            let mut msg = String::new();
            write!(msg, "{:?} (size {:?})\n", self.props.name, self.size).unwrap();
            for (i, child) in self.children.iter().enumerate() {
                if i == bad_index {
                    write!(msg, "  - +{:?} {:?} (ends at {:?}) (BAD)\n", child.offset, child.node.props.name, child.end()).unwrap();
                } else {
                    write!(msg, "  - +{:?} {:?} (ends at {:?})\n", child.offset, child.node.props.name, child.end()).unwrap();
                }
            }
            panic!("structure invariants broken: {}", msg);
        }
    }
}

impl Default for Node {
    fn default() -> Node {
        Node {
            props: Properties::default(),
            size: addr::Offset::MAX,
            children: vec::Vec::new(),
        }
    }
}

impl Default for Childhood {
    fn default() -> Self {
        lazy_static! {
            static ref DEFAULT_NODE: sync::Arc<Node> = sync::Arc::new(Node::default());
        }
        
        Childhood {
            node: DEFAULT_NODE.clone(),
            offset: addr::Offset::ZERO,
        }
    }
}
