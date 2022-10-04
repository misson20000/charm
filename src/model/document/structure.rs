use std::sync;
use std::vec;

use crate::model::addr;

#[derive(Debug, Clone)]
pub enum TitleDisplay {
    Inline,
    Major,
    Minor,
}

#[derive(Debug, Clone)]
pub enum ChildrenDisplay {
    None, //< fully collapsed
    Summary,
    //Recolor,
    Full
}

#[derive(Debug, Clone)]
pub enum ContentDisplay {
    None,
    Hexdump(addr::Size),
    Hexstring
}

pub type Path = vec::Vec<usize>;
pub type PathIter<'a> = std::vec::IntoIter<usize>;

#[derive(Debug, Clone)]
pub struct Childhood {
    pub node: sync::Arc<Node>,
    pub offset: addr::Address,
}

impl Childhood {
    pub fn end(&self) -> addr::Address {
        self.offset + self.node.size
    }
}

#[derive(Debug, Clone)]
pub struct Properties {
    pub name: String,
    pub title_display: TitleDisplay,
    pub children_display: ChildrenDisplay,
    pub content_display: ContentDisplay,
    pub locked: bool,    
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
            size: addr::unit::REAL_MAX,
            children: vec::Vec::new(),
        }
    }
}
