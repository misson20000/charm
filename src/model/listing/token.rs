use sync::Arc;

use crate::model::addr;
use crate::model::document::structure;

#[derive(Debug)]
pub enum TokenClass {
    Null,
    Title,
    Hexdump(addr::Extent),
    Hexstring(addr::Extent)
}

pub struct Token {
    pub class: TokenClass,
    pub node: sync::Arc<structure::Node>,
    pub newline: bool, // before this token
}
