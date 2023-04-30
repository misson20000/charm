use std::fmt;
use std::sync;

use crate::model::addr;
use crate::model::document::structure;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenClass {
    /// An empty token, used to create a blank line via the newline attribute.
    Punctuation(PunctuationClass),
    /// A title block to show the name of the structure node.
    Title,
    SummaryLabel,
    /// Formatted two-column hexdump+asciidump. What you expect to see out of a hex editor.
    Hexdump(addr::Extent),
    /// Just a bunch of hex octets stuck together without any extra formatting.
    Hexstring(addr::Extent)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PunctuationClass {
    Empty,
    Space,
    Comma,
    OpenBracket,
    CloseBracket,
}

/// The smallest unit used to perform listing layout. Picture these as the words
/// which would be flowed into a textbox by a typesetting engine.
#[derive(Clone)]
pub struct Token {
    pub class: TokenClass,
    pub node: sync::Arc<structure::Node>,
    pub node_path: structure::Path,
    pub node_addr: addr::Address,
    // TODO: colorization
    // colorization will be implemented by emitting multiple hexdump tokens on one line
    // all with the same "colorzation root" node, but different colorization vectors representing
    // all the children between the colorization root and where this token is.
    pub depth: usize,
    pub newline: bool, // after this token
}

impl Token {
    pub fn absolute_extent(&self) -> addr::Extent {
        match self.class {
            TokenClass::Hexdump(extent) => extent.rebase(self.node_addr),
            TokenClass::Hexstring(extent) => extent.rebase(self.node_addr),
            _ => addr::unit::EMPTY
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Token")
            .field("class", &self.class)
            .field("node", &self.node.props.name)
            .field("depth", &self.depth)
            .field("newline", &self.newline)
            .finish()
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.class == other.class &&
            sync::Arc::<structure::Node>::ptr_eq(&self.node, &other.node) &&
            self.node_addr == other.node_addr &&
            self.depth == other.depth &&
            self.newline == other.newline
    }
}

impl Eq for Token {
}

pub struct TokenTestFormat<'a>(pub &'a Token);

impl<'a> fmt::Display for TokenTestFormat<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.class {
            TokenClass::Punctuation(punct) => write!(f, "{}", match punct {
                PunctuationClass::Empty => "",
                PunctuationClass::Space => " ",
                PunctuationClass::Comma => ", ",
                PunctuationClass::OpenBracket => "{",
                PunctuationClass::CloseBracket => "}",
            }),
            TokenClass::Title => write!(f, "{}: ", &self.0.node.props.name),
            TokenClass::SummaryLabel => write!(f, "{}: ", &self.0.node.props.name),
            TokenClass::Hexdump(extent) => {
                for i in 0..extent.length().bytes {
                    write!(f, "{:02x}", (extent.begin.byte + i) & 0xff)?;
                    if i + 1 < extent.length().bytes {
                        write!(f, " ")?;
                    }
                }
                Ok(())
            },
            TokenClass::Hexstring(extent) => {
                for i in 0..extent.length().bytes {
                    write!(f, "{:02x}", (extent.begin.byte + i) & 0xff)?
                }
                Ok(())
            }
        }
    }
}
