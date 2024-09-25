use std::iter;
use std::sync;
use std::vec;

use lazy_static::lazy_static;

use crate::model::addr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TitleDisplay {
    Inline,
    Major,
    Minor,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ChildrenDisplay {
    None, //< fully collapsed
    Summary,
    //Recolor,
    Full
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ContentDisplay {
    None,
    Hexdump {
        line_pitch: addr::Size,
        gutter_pitch: addr::Size,
    },
    Hexstring
}

pub type Path = vec::Vec<usize>;
pub type PathSlice<'a> = &'a [usize];
pub type PathIter<'a> = std::vec::IntoIter<usize>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SiblingRange {
    pub parent: Path,
    pub first: usize,

    /// Inclusive
    pub last: usize
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RangeInvalidity {
    IndexExceedsNumberOfChildren,
    Inverted,
}

impl SiblingRange {
    pub fn new(parent: Path, first: usize, last: usize) -> Self {
        Self { parent, first, last }
    }

    pub fn check_validity(&self, parent_node: &Node) -> Result<(), RangeInvalidity> {
        if self.first >= parent_node.children.len() || self.last >= parent_node.children.len() {
            return Err(RangeInvalidity::IndexExceedsNumberOfChildren);
        }

        if self.last < self.first {
            return Err(RangeInvalidity::Inverted);
        }

        return Ok(());
    }
    
    /// How many nodes this range refers to (not counting descendants).
    pub fn count(&self) -> usize {
        self.last - self.first + 1
    }
    
    pub fn indices(&self) -> std::ops::RangeInclusive<usize> {
        self.first..=self.last
    }

    pub fn contains_index(&self, index: usize) -> bool {
        self.indices().contains(&index)
    }

    /// Returns true if the path refers to one of the siblings included in this range, but not if it refers to one of
    /// their descendants.
    pub fn contains_path(&self, path: PathSlice) -> bool {
        path.len() == self.parent.len() + 1 && &path[0..self.parent.len()] == &self.parent[..] && self.contains_index(path[self.parent.len()])
    }

    /// Returns true if the path refers to one of the siblings included in this range or one of their descendants.
    pub fn contains_descendant(&self, path: PathSlice) -> bool {
        path.len() > self.parent.len() && &path[0..self.parent.len()] == &self.parent[..] && self.contains_index(path[self.parent.len()])
    }

    pub fn overlaps(&self, other: &SiblingRange) -> bool {
        &self.parent[..] == &other.parent[..] && self.first <= other.last && other.first <= self.last
    }
}

#[derive(Debug, Clone)]
pub struct Childhood {
    pub node: sync::Arc<Node>,
    pub offset: addr::Address,
}

impl Childhood {
    pub fn new(node: sync::Arc<Node>, offset: addr::Address) -> Childhood {
        Childhood { node, offset }
    }
    
    pub fn end(&self) -> addr::Address {
        self.offset + self.node.size
    }

    pub fn extent(&self) -> addr::Extent {
        addr::Extent::sized(self.offset, self.node.size)
    }
}

/// These are separated from the rest of the Node because they can be changed
/// without rearranging nodes, which makes it easier to port paths across
/// Changes if we can use a different ChangeType when only properties are
/// affected.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Properties {
    pub name: String,
    pub title_display: TitleDisplay,
    pub children_display: ChildrenDisplay,
    pub content_display: ContentDisplay,
    pub locked: bool,    
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct MaybeProperties {
    pub name: Option<String>,
    pub title_display: Option<TitleDisplay>,
    pub children_display: Option<ChildrenDisplay>,
    pub content_display: Option<ContentDisplay>,
    pub locked: Option<bool>,
}

#[derive(Debug, Clone)]
pub struct Node {
    /* reference to parent causes a lot of problems, so we don't have one and
       opt to refer to nodes by path when necessary. */
    pub props: Properties,
    pub size: addr::Size,
    pub children: vec::Vec<Childhood>
}

impl TitleDisplay {
    pub fn has_blanks(&self) -> bool {
        match self {
            TitleDisplay::Inline => false,
            TitleDisplay::Major => true,
            TitleDisplay::Minor => false,
        }
    }

    pub fn is_inline(&self) -> bool {
        match self {
            TitleDisplay::Inline => true,
            TitleDisplay::Major => false,
            TitleDisplay::Minor => false,
        }
    }
}

impl Default for TitleDisplay {
    fn default() -> TitleDisplay {
        TitleDisplay::Major
    }
}

impl Default for ChildrenDisplay {
    fn default() -> ChildrenDisplay {
        ChildrenDisplay::Full
    }
}

impl ContentDisplay {
    pub fn preferred_pitch(&self) -> Option<addr::Size> {
        match self {
            ContentDisplay::None => None,
            ContentDisplay::Hexdump { line_pitch, .. } => Some(*line_pitch),
            ContentDisplay::Hexstring => None,
        }
    }

    /// An alternative to default() that explicitly returns a hexdump style.
    pub fn default_hexdump() -> ContentDisplay {
        ContentDisplay::Hexdump {
            line_pitch: addr::Size::from(16),
            gutter_pitch: addr::Size::from(8),
        }
    }
}

impl Default for ContentDisplay {
    fn default() -> ContentDisplay {
        Self::default_hexdump()
    }
}

impl Default for Properties {
    fn default() -> Properties {
        Properties {
            name: "default".to_string(),
            title_display: TitleDisplay::default(),
            children_display: ChildrenDisplay::default(),
            content_display: ContentDisplay::default(),
            locked: true,
        }
    }
}

impl Default for Node {
    fn default() -> Node {
        Node {
            props: Properties::default(),
            size: addr::unit::MAX,
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
            offset: addr::unit::NULL,
        }
    }
}

impl Node {
    pub fn default_sized(size: addr::Size) -> Node {
        Node {
            props: Properties::default(),
            size,
            children: vec::Vec::new(),
        }
    }

    pub fn builder() -> builder::StructureBuilder {
        builder::StructureBuilder::default()
    }

    pub fn child_at_offset(&self, offset: addr::Address) -> usize {
        self.children.partition_point(|ch| ch.offset < offset)
    }
}

impl Properties {
    pub fn clone_rename(&self, new_name: String) -> Properties {
        Properties {
            name: new_name,
            title_display: self.title_display.clone(),
            children_display: self.children_display.clone(),
            content_display: self.content_display.clone(),
            locked: self.locked,
        }
    }

    pub fn apply_changes(&mut self, changes: MaybeProperties) {
        if let Some(name) = &changes.name { self.name = name.clone(); }
        if let Some(title_display) = &changes.title_display { self.title_display = title_display.clone(); }
        if let Some(children_display) = &changes.children_display { self.children_display = children_display.clone(); }
        if let Some(content_display) = &changes.content_display { self.content_display = content_display.clone(); }
        if let Some(locked) = &changes.locked { self.locked = locked.clone(); }
    }
}

impl MaybeProperties {
    pub fn new(props: Properties) -> MaybeProperties {
        MaybeProperties {
            name: Some(props.name),
            title_display: Some(props.title_display),
            children_display: Some(props.children_display),
            content_display: Some(props.content_display),
            locked: Some(props.locked),
        }
    }

    pub fn common_between<'a>(i: impl iter::Iterator<Item = &'a sync::Arc<Node>>) -> Option<MaybeProperties> {
        let mut props = Option::<MaybeProperties>::None;

        for node in i {
            if let Some(props) = props.as_mut() {
                if props.empty() {
                    break;
                }
                
                props.merge(&node.props);
            } else {
                props = Some(Self::new(node.props.clone()));
            }
        }

        props
    }
    
    pub fn new_name(name: String) -> MaybeProperties {
        let mut props = Self::default();
        props.name = Some(name);
        props
    }

    pub fn new_title_display(title_display: TitleDisplay) -> MaybeProperties {
        let mut props = Self::default();
        props.title_display = Some(title_display);
        props
    }

    pub fn new_children_display(children_display: ChildrenDisplay) -> MaybeProperties {
        let mut props = Self::default();
        props.children_display = Some(children_display);
        props
    }

    pub fn new_content_display(content_display: ContentDisplay) -> MaybeProperties {
        let mut props = Self::default();
        props.content_display = Some(content_display);
        props
    }

    pub fn new_locked(locked: bool) -> MaybeProperties {
        let mut props = Self::default();
        props.locked = Some(locked);
        props
    }

    fn keep_if_eq<T: PartialEq>(option: &mut Option<T>, other: &T) {
        *option = match option.take() {
            Some(x) if x.eq(other) => Some(x),
            _ => None
        };
    }
    
    pub fn merge(&mut self, other: &Properties) {
        Self::keep_if_eq(&mut self.name, &other.name);
        Self::keep_if_eq(&mut self.title_display, &other.title_display);
        Self::keep_if_eq(&mut self.children_display, &other.children_display);
        Self::keep_if_eq(&mut self.content_display, &other.content_display);
        Self::keep_if_eq(&mut self.locked, &other.locked);
    }

    pub fn apply_changes(&mut self, changes: MaybeProperties) {
        if changes.name.is_some() { self.name = changes.name; }
        if changes.title_display.is_some() { self.title_display = changes.title_display; }
        if changes.children_display.is_some() { self.children_display = changes.children_display; }
        if changes.content_display.is_some() { self.content_display = changes.content_display; }
        if changes.locked.is_some() { self.locked = changes.locked; }
    }
    
    pub fn empty(&self) -> bool {
        self.name.is_none()
            && self.title_display.is_none()
            && self.children_display.is_none()
            && self.content_display.is_none()
            && self.locked.is_none()
    }
}

/* This is mostly useful for testcases */
pub mod builder {
    use super::*;

    #[derive(Default)]
    pub struct StructureBuilder {
        node: Node
    }

    impl StructureBuilder {
        pub fn props(mut self, props: Properties) -> Self {
            self.node.props = props;
            self
        }

        pub fn name<S: ToString>(mut self, name: S) -> Self {
            self.node.props.name = name.to_string();
            self
        }

        pub fn title_display(mut self, value: TitleDisplay) -> Self {
            self.node.props.title_display = value;
            self
        }

        pub fn children_display(mut self, value: ChildrenDisplay) -> Self {
            self.node.props.children_display = value;
            self
        }

        pub fn content_display(mut self, value: ContentDisplay) -> Self {
            self.node.props.content_display = value;
            self
        }

        pub fn lock(mut self) -> Self {
            self.node.props.locked = true;
            self
        }

        pub fn unlock(mut self) -> Self {
            self.node.props.locked = false;
            self
        }

        pub fn size<S: Into<addr::Size>>(mut self, size: S) -> Self {
            self.node.size = size.into();
            self
        }

        pub fn child<A: Into<addr::Address>, F: FnOnce(StructureBuilder) -> StructureBuilder>(mut self, offset: A, builder: F) -> Self {
            self.node.children.push(Childhood {
                offset: offset.into(),
                node: builder(Self::default()).build()
            });
            self
        }

        pub fn build(&self) -> sync::Arc<Node> {
            sync::Arc::new(self.node.clone())
        }

        pub fn build_child<T: Into<addr::Address>>(&self, offset: T) -> Childhood {
            Childhood::new(sync::Arc::new(self.node.clone()), offset.into())
        }
    }
}
