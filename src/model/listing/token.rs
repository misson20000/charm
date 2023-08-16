use std::fmt;
use std::sync;

use crate::model::addr;
use crate::model::document::structure;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenClass {
    /// An empty token, used to create a blank line via the newline attribute.
    Punctuation {
        class: PunctuationClass,
        accepts_cursor: bool
    },
    /// A title block to show the name of the structure node.
    Title,
    SummaryLabel,
    /// Formatted two-column hexdump+asciidump. What you expect to see out of a hex editor.
    Hexdump(addr::Size),
    /// Just a bunch of hex octets stuck together without any extra formatting.
    Hexstring(addr::Size)
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
    /// The type of token.
    pub class: TokenClass,
    
    /// The structure node that this token belongs to.
    pub node: sync::Arc<structure::Node>,
    /// Path to [self.node].
    pub node_path: structure::Path, // TODO: remove this for memory efficiency
    /// The absolute address that [self.node] begins at in the document.
    pub node_addr: addr::Address,
    
    /// The offset within [self.node] that this token begins at.
    pub offset: addr::Size,
    /// The index of [self.node]'s child that this token either refers to or comes before. May equal number of children to represent tokens that come after the last child.
    pub index: usize,
    
    // TODO: colorization
    // colorization will be implemented by emitting multiple hexdump tokens on one line
    // all with the same "colorzation root" node, but different colorization vectors representing
    // all the children between the colorization root and where this token is.

    /// Depth within structure hierarchy that [self.owning_node] is at. Zero if [self.node] is the root node.
    pub depth: usize,
    /// True if this should be the last token on the line it appears on.
    pub newline: bool,

    // Don't forget to update impl Eq when adding new fields!!!
}

impl Token {
    /// The length of data that this token spans. For most tokens classes, this is [addr::unit::ZERO].
    pub fn size(&self) -> addr::Size {
        match self.class {
            TokenClass::Hexdump(size) => size,
            TokenClass::Hexstring(size) => size,
            _ => addr::unit::ZERO
        }
    }

    /// The range of data within [self.node] that this token spans. For most token classes, this is a zero-size extent.
    pub fn extent(&self) -> addr::Extent {
        addr::Extent::sized(self.offset.to_addr(), self.size())
    }

    /// The absolute address that this token begins at.
    pub fn absolute_addr(&self) -> addr::Address {
        self.node_addr + self.offset
    }

    /// The absolute range of data that this token spans. For most tokens classes, this is a zero-size extent.
    pub fn absolute_extent(&self) -> addr::Extent {
        addr::Extent::sized(self.node_addr + self.offset, self.size())
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Token")
            .field("class", &self.class)
            .field("node", &self.node.props.name)
            .field("node_ptr", &sync::Arc::as_ptr(&self.node))
            .field("node_path", &self.node_path)
            .field("node_addr", &self.node_addr)
            .field("offset", &self.offset)
            .field("index", &self.index)
            .field("depth", &self.depth)
            .field("newline", &self.newline)
            .finish()
    }
}

// We can't derive this because we need to use ptr_eq for [Token::node].
impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.class == other.class &&
            sync::Arc::<structure::Node>::ptr_eq(&self.node, &other.node) &&
            self.node_path == other.node_path &&
            self.node_addr == other.node_addr &&
            self.offset == other.offset &&
            self.index == other.index &&
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
            TokenClass::Punctuation { class, .. } => write!(f, "{}", class.as_str()),
            TokenClass::Title => write!(f, "{}: ", &self.0.node.props.name),
            TokenClass::SummaryLabel => write!(f, "{}: ", &self.0.node.props.name),
            TokenClass::Hexdump(size) => {
                for i in 0..size.bytes {
                    write!(f, "{:02x}", (self.0.offset.bytes + i) & 0xff)?;
                    if i + 1 < size.bytes {
                        write!(f, " ")?;
                    }
                }
                Ok(())
            },
            TokenClass::Hexstring(size) => {
                for i in 0..size.bytes {
                    write!(f, "{:02x}", (self.0.offset.bytes + i) & 0xff)?
                }
                Ok(())
            }
        }
    }
}

impl PunctuationClass {
    pub fn as_str(&self) -> &'static str {
        match self {
            PunctuationClass::Empty => "",
            PunctuationClass::Space => " ",
            PunctuationClass::Comma => ", ",
            PunctuationClass::OpenBracket => "{",
            PunctuationClass::CloseBracket => "}",            
        }
    }
}
