use std::fmt;
use std::sync;

use crate::model::addr;
use crate::model::document::structure;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenClass {
    /// An empty token, used to create a blank line via the newline attribute.
    Null,
    /// A title block to show the name of the structure node.
    Title,
    /// Formatted two-column hexdump+asciidump. What you expect to see out of a hex editor.
    Hexdump(addr::Extent),
    /// Just a bunch of hex octets stuck together without any extra formatting.
    Hexstring(addr::Extent)
}

/// The smallest unit used to perform listing layout. Picture these as the words
/// which would be flowed into a textbox by a typesetting engine.
#[derive(Clone)]
pub struct Token {
    pub class: TokenClass,
    pub node: sync::Arc<structure::Node>,
    pub depth: usize,
    pub newline: bool, // after this token
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Token")
            .field("class", &self.class)
            .field("node", &self.node.name)
            .field("depth", &self.depth)
            .field("newline", &self.newline)
            .finish()
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.class == other.class &&
            sync::Arc::<structure::Node>::ptr_eq(&self.node, &other.node) &&
            self.newline == other.newline
    }
}

impl Eq for Token {
}
