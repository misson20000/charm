use std::sync;

use crate::model::addr;
use crate::model::document::structure;

#[derive(Debug)]
pub enum TokenClass {
    Null,
    Title,
    Hexdump(addr::Extent),
    Hexstring(addr::Extent)
}

#[derive(Debug)]
pub struct Token {
    pub class: TokenClass,
    pub node: sync::Arc<structure::Node>,
    pub newline: bool, // before this token
}
