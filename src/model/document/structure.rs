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
    Hexdump(usize /* pitch */),
    Hexstring
}

#[derive(Debug)]
pub struct Childhood {
    pub node: sync::Arc<Node>,
    pub offset: addr::Offset,
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
