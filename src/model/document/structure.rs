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

#[derive(Debug, Clone)]
pub struct ChildhoodRef<'a> {
    pub node: &'a sync::Arc<Node>,
    pub offset: addr::Offset,
}

impl Childhood {
    pub fn new(node: sync::Arc<Node>, offset: addr::Offset) -> Childhood {
        Childhood { node, offset }
    }
    
    pub fn end(&self) -> addr::Offset {
        self.offset + self.node.size()
    }

    pub fn extent(&self) -> addr::Extent {
        addr::Extent::sized(self.offset, self.node.size())
    }

    pub fn borrow(&self) -> ChildhoodRef {
        ChildhoodRef {
            node: &self.node,
            offset: self.offset,
        }
    }
}

impl<'a> ChildhoodRef<'a> {
    pub fn end(&self) -> addr::Offset {
        self.offset + self.node.size()
    }

    pub fn extent(&self) -> addr::Extent {
        addr::Extent::sized(self.offset, self.node.size())
    }

    pub fn into_owned(&self) -> Childhood {
        Childhood::new(self.node.clone(), self.offset)
    }
}

pub trait NodeKind: Into<Node> {
    /// Returns a string that succinctly describes the node, for debugging purposes. Usually the name.
    fn inspect(&self) -> String;

    /// Returns the size of the node.
    fn size(&self) -> addr::Offset;

    /// Returns the number of children the node has.
    fn num_children(&self) -> usize;

    /// Gets one of the node's children. Panics if index is out of bounds. Note that there is no function for retrieving an iterator over the children. Arrays can have unreasonable amounts of children, so care must be taken to never iterate over all of them.
    fn child(&self, index: usize) -> ChildhoodRef;

    fn try_get_child(&self, index: usize) -> Option<ChildhoodRef> {
        if index < self.num_children() {
            Some(self.child(index))
        } else {
            None
        }
    }

    /// Returns the lowest index that a new child could have if it were inserted at the specified offset.
    fn child_index_at_offset(&self, offset: addr::Offset) -> usize;

    /// Returns the node's properties, if this is a kind of node that has properties.
    fn props(&self) -> Option<&Properties>;

    /// Returns a mutable reference to the node's properties, if this is a kind of node that has properties.
    fn props_mut(&mut self) -> Option<&mut Properties>;

    /// Assuming `self` is the node at path[index], attempts to mutate the path to point to `self`'s successor (possibly recursing), or returns false if the node doesn't know about any successor.
    fn successor(&self, path: &mut Path, index: usize) -> bool;

    /// Asserts that the node is internally valid and consistent.
    fn assert_validity(&self);

    /// Converts the Node into a sync::Arc<Node>.
    fn into_arc(self) -> sync::Arc<Node> {
        sync::Arc::new(self.into())
    }
}

#[derive(Debug, Clone)]
pub enum Node {
    Hierarchy(HierarchyNode),
    Comment(CommentNode),
}

impl NodeKind for Node {
    fn inspect(&self) -> String {
        match self {
            Node::Hierarchy(h) => h.inspect(),
            Node::Comment(c) => c.inspect(),
        }
    }
    
    fn size(&self) -> addr::Offset {
        match self {
            Node::Hierarchy(h) => h.size(),
            Node::Comment(c) => c.size(),
        }
    }

    fn num_children(&self) -> usize {
        match self {
            Node::Hierarchy(h) => h.num_children(),
            Node::Comment(c) => c.num_children(),
        }
    }

    fn child(&self, index: usize) -> ChildhoodRef {
        match self {
            Node::Hierarchy(h) => h.child(index),
            Node::Comment(c) => c.child(index),
        }
    }

    fn child_index_at_offset(&self, offset: addr::Offset) -> usize {
        match self {
            Node::Hierarchy(h) => h.child_index_at_offset(offset),
            Node::Comment(c) => c.child_index_at_offset(offset),
        }
    }
    
    fn props(&self) -> Option<&Properties> {
        match self {
            Node::Hierarchy(h) => h.props(),
            Node::Comment(c) => c.props(),
        }
    }

    fn props_mut(&mut self) -> Option<&mut Properties> {
        match self {
            Node::Hierarchy(h) => h.props_mut(),
            Node::Comment(c) => c.props_mut(),
        }
    }

    fn successor(&self, path: &mut Path, index: usize) -> bool {
        match self {
            Node::Hierarchy(h) => h.successor(path, index),
            Node::Comment(c) => c.successor(path, index),
        }
    }

    fn assert_validity(&self) {
        match self {
            Node::Hierarchy(h) => h.assert_validity(),
            Node::Comment(c) => c.assert_validity(),
        }
    }
}

/*
pub trait AncestralNodeKind: NodeKind {
    fn child(&self, index: usize) -> &Childhood;
}

#[derive(Debug, Clone)]
/// Nodes that are capable of having children
pub enum AncestralNode {
    Hierarchy(HierarchyNode),
}

impl NodeKind for AncestralNode {
    fn inspect(&self) -> String {
        match self {
            AncestralNode::Hierarchy(h) => h.inspect(),
        }
    }
    
    fn size(&self) -> addr::Offset {
        match self {
            AncestralNode::Hierarchy(h) => h.size(),
        }
    }

    fn num_children(&self) -> usize {
        match self {
            AncestralNode::Hierarchy(h) => h.num_children()
        }
    }

    fn props_mut(&mut self) -> Option<&mut Properties> {
        match self {
            AncestralNode::Hierarchy(h) => h.props_mut(),
        }
    }
}

impl Into<Node> for AncestralNode {
    fn into(self) -> Node {
        match self {
            AncestralNode::Hierarchy(h) => Node::Hierarchy(h),
        }
    }
}

impl AncestralNodeKind for AncestralNode {
    fn child(&self, index: usize) -> &Childhood {
        match self {
            NodeKind::Hierarchy(h) => h.child(index),
        }
    }
}
*/

#[derive(Debug, Clone)]
pub struct HierarchyNode {
    /* reference to parent causes a lot of problems, so we don't have one and
       opt to refer to nodes by path when necessary. */
    pub props: props::Properties,
    pub size: addr::Offset,
    pub children: vec::Vec<Childhood>
}

impl NodeKind for HierarchyNode {
    fn inspect(&self) -> String {
        format!("'{}'", self.props.name)
    }
    
    fn size(&self) -> addr::Offset {
        self.size
    }

    fn num_children(&self) -> usize {
        self.children.len()
    }

    fn child(&self, index: usize) -> ChildhoodRef {
        self.children[index].borrow()
    }

    fn child_index_at_offset(&self, offset: addr::Offset) -> usize {
        self.children.partition_point(|ch| ch.offset < offset)
    }
    
    fn props(&self) -> Option<&Properties> {
        Some(&self.props)
    }

    fn props_mut(&mut self) -> Option<&mut Properties> {
        Some(&mut self.props)
    }

    fn successor(&self, path: &mut Path, index: usize) -> bool {
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
    
    fn assert_validity(&self) {
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
                    write!(msg, "  - +{:?} {} (ends at {:?}) (BAD)\n", child.offset, child.node.inspect(), child.end()).unwrap();
                } else {
                    write!(msg, "  - +{:?} {} (ends at {:?})\n", child.offset, child.node.inspect(), child.end()).unwrap();
                }
            }
            panic!("structure invariants broken: {}", msg);
        }
    }
}

/*
impl AncestralNodeKind for HierarchyNode {
    fn child(&self, index: usize) -> &Childhood {
        self.children[index]
    }
}
*/

impl HierarchyNode {
    pub fn default_sized(size: addr::Offset) -> HierarchyNode {
        HierarchyNode {
            props: props::Properties::default(),
            size,
            children: vec::Vec::new(),
        }
    }

    pub fn builder() -> builder::StructureBuilder {
        builder::StructureBuilder::default()
    }
}

impl Into<Node> for HierarchyNode {
    fn into(self) -> Node {
        Node::Hierarchy(self)
    }
}

impl Default for HierarchyNode {
    fn default() -> HierarchyNode {
        HierarchyNode {
            props: Properties::default(),
            size: addr::Offset::MAX,
            children: vec::Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CommentNode {
    pub text: String,
}

impl NodeKind for CommentNode {
    fn inspect(&self) -> String {
        format!("comment({})", self.text)
    }

    fn size(&self) -> addr::Offset {
        addr::Offset::ZERO
    }

    fn num_children(&self) -> usize {
        0
    }

    fn child(&self, _index: usize) -> ChildhoodRef {
        panic!("attempted to retrieve child of comment node");
    }

    fn child_index_at_offset(&self, _offset: addr::Offset) -> usize {
        0
    }

    fn props(&self) -> Option<&Properties> {
        None
    }

    fn props_mut(&mut self) -> Option<&mut Properties> {
        None
    }

    fn successor(&self, path: &mut Path, index: usize) -> bool {
        assert!(path.len() == index);
        false
    }

    fn assert_validity(&self) {
    }
}

impl Into<Node> for CommentNode {
    fn into(self) -> Node {
        Node::Comment(self)
    }
}

impl Default for Node {
    fn default() -> Node {
        Node::Hierarchy(HierarchyNode::default())
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
