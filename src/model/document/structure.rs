use std::sync;
use std::vec;

use crate::model::addr;

#[derive(Debug)]
pub enum TitleDisplay {
    Inline,
    Major,
    Minor,
}

#[derive(Debug)]
pub enum ChildrenDisplay {
    None, //< fully collapsed
    Summary,
    //Recolor,
    Full
}

#[derive(Debug)]
pub enum ContentDisplay {
    None,
    Hexdump(addr::Size),
    Hexstring
}

#[derive(Debug, Clone)]
pub struct Childhood {
    pub node: sync::Arc<Node>,
    pub offset: addr::Offset,
}

impl Childhood {
    pub fn end(&self) -> addr::Offset {
        self.offset + self.node.size
    }
}

#[derive(Debug)]
pub struct Node {
    /* reference to parent causes a lot of problems */
    pub name: String,
    pub size: addr::Size,
    pub title_display: TitleDisplay,
    pub children_display: ChildrenDisplay,
    pub content_display: ContentDisplay,
    pub locked: bool,

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
            ContentDisplay::Hexdump(pitch) => Some(*pitch),
            ContentDisplay::Hexstring => None,
        }
    }
}

impl Default for ContentDisplay {
    fn default() -> ContentDisplay {
        ContentDisplay::Hexdump(addr::Size::from(16))
    }
}

impl Default for Node {
    fn default() -> Node {
        Node {
            name: "default".to_string(),
            size: addr::unit::REAL_MAX,
            title_display: TitleDisplay::default(),
            children_display: ChildrenDisplay::default(),
            content_display: ContentDisplay::default(),
            locked: true,
            children: vec::Vec::new(),
        }
    }
}
