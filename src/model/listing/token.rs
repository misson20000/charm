use std::fmt;
use std::sync;

use crate::model::addr;
use crate::model::document::structure;

/// The smallest unit used to perform listing layout. Picture these as the words
/// which would be flowed into a textbox by a typesetting engine.
#[derive(Clone, PartialEq, Eq)]
pub enum Token {
    /// An empty token, used to create a blank line
    BlankLine(BlankLineToken),

    /// Meta-tokens so line builder can tell where summaries start and end
    SummaryPreamble(SummaryPreambleToken),
    SummaryEpilogue(SummaryEpilogueToken),
    
    /// Punctuation for summaries.
    SummaryPunctuation(SummaryPunctuationToken),
    
    /// A title block to show the name of the structure node.
    Title(TitleToken),

    /// The name of a child in a summary.
    SummaryLabel(SummaryLabelToken),
    
    /// Formatted two-column hexdump+asciidump. What you expect to see out of a hex editor.
    Hexdump(HexdumpToken),
    
    /// Just a bunch of hex octets stuck together without any extra formatting.
    Hexstring(HexstringToken),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TokenRef<'a> {
    BlankLine(&'a BlankLineToken),
    SummaryPreamble(&'a SummaryPreambleToken),
    SummaryEpilogue(&'a SummaryEpilogueToken),
    SummaryPunctuation(&'a SummaryPunctuationToken),
    Title(&'a TitleToken),
    SummaryLabel(&'a SummaryLabelToken),
    Hexdump(&'a HexdumpToken),
    Hexstring(&'a HexstringToken),
}

pub trait TokenKind {
    fn into_token(self) -> Token;
    fn as_ref(&self) -> TokenRef<'_>;

    fn common(&self) -> &TokenCommon;
    
    fn node(&self) -> &sync::Arc<structure::Node> {
        &self.common().node
    }

    fn node_path(&self) -> &structure::Path {
        &self.common().node_path
    }

    fn node_addr(&self) -> addr::Address {
        self.common().node_addr
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlankLineToken {
    pub common: TokenCommon,
    pub accepts_cursor: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SummaryPreambleToken {
    pub common: TokenCommon,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SummaryEpilogueToken {
    pub common: TokenCommon,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SummaryPunctuationToken {
    pub common: TokenCommon,
    pub kind: PunctuationKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TitleToken {
    pub common: TokenCommon,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SummaryLabelToken {
    pub common: TokenCommon,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HexdumpToken  {
    pub common: TokenCommon,
    /// Index of next child after this hexdump
    pub index: usize,
    pub extent: addr::Extent,
    pub line: addr::Extent,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HexstringToken {
    pub common: TokenCommon,
    pub extent: addr::Extent,
}

/// Various forms of punctuation used ONLY in summaries.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PunctuationKind {
    /* should accept cursor */
    Space, ///< Used when a leaf node has content display set to None.
    
    /* should not accept cursor */
    Comma,
    
    /* should accept cursor */
    OpenBracket,
    
    /* should accept cursor */
    CloseBracket,
}

#[derive(Clone)]
pub struct TokenCommon {
    pub node: sync::Arc<structure::Node>,
    pub node_path: structure::Path,
    pub node_addr: addr::Address,
    // TODO: colorization
    // colorization will be implemented by emitting multiple hexdump tokens on one line
    // all with the same "colorzation root" node, but different colorization vectors representing
    // all the children between the colorization root and where this token is.
    pub depth: usize,
}

impl Token {
    pub fn absolute_extent(&self) -> addr::Extent {
        match self {
            Token::Hexdump(token) => token.absolute_extent(),
            Token::Hexstring(token) => token.extent.rebase(token.common.node_addr),
            _ => addr::unit::EMPTY
        }
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            Token::BlankLine(_) => "BlankLine",
            Token::SummaryPreamble(_) => "SummaryPreamble",
            Token::SummaryEpilogue(_) => "SummaryEpilogue",
            Token::SummaryPunctuation(_) => "SummaryPunctuation",
            Token::Title(_) => "Title",
            Token::SummaryLabel(_) => "SummaryLabel",
            Token::Hexdump(_) => "Hexdump",
            Token::Hexstring(_) => "Hexstring",
        }
    }
}

impl<'a> TokenRef<'a> {
    pub fn common(&self) -> &'a TokenCommon {
        match self {
            /* Please keep 'common' as the first field in each of these structs. */
            TokenRef::BlankLine(t) => t.common(),
            TokenRef::SummaryPreamble(t) => t.common(),
            TokenRef::SummaryEpilogue(t) => t.common(),
            TokenRef::SummaryPunctuation(t) => t.common(),
            TokenRef::Title(t) => t.common(),
            TokenRef::SummaryLabel(t) => t.common(),
            TokenRef::Hexdump(t) => t.common(),
            TokenRef::Hexstring(t) => t.common(),
        }
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            TokenRef::BlankLine(_) => "BlankLine",
            TokenRef::SummaryPreamble(_) => "SummaryPreamble",
            TokenRef::SummaryEpilogue(_) => "SummaryEpilogue",
            TokenRef::SummaryPunctuation(_) => "SummaryPunctuation",
            TokenRef::Title(_) => "Title",
            TokenRef::SummaryLabel(_) => "SummaryLabel",
            TokenRef::Hexdump(_) => "Hexdump",
            TokenRef::Hexstring(_) => "Hexstring",
        }
    }
    
    pub fn node(&self) -> &'a sync::Arc<structure::Node> {
        &self.common().node
    }

    pub fn node_path(&self) -> &'a structure::Path {
        &self.common().node_path
    }

    pub fn node_addr(&self) -> addr::Address {
        self.common().node_addr
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let common = self.common();
        
        let mut ds = f.debug_struct("Token");
        
        let ds = ds.field("kind", &self.kind_name())
            .field("node", &common.node.props.name)
            .field("node_path", &common.node_path)
            .field("node_addr", &common.node_addr)
            .field("depth", &common.depth);

        match self {
            Token::Hexdump(hdt) => ds
                .field("index", &hdt.index)
                .field("extent", &hdt.extent)
                .field("line", &hdt.line),
            _ => ds
        }.finish()
    }
}

impl<'a> fmt::Debug for TokenRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let common = self.common();
        
        let mut ds = f.debug_struct("TokenRef");
        
        let ds = ds.field("kind", &self.kind_name())
            .field("node", &common.node.props.name)
            .field("node_path", &common.node_path)
            .field("node_addr", &common.node_addr)
            .field("depth", &common.depth);

        match self {
            TokenRef::Hexdump(hdt) => ds
                .field("index", &hdt.index)
                .field("extent", &hdt.extent)
                .field("line", &hdt.line),
            _ => ds
        }.finish()
    }
}

impl fmt::Debug for TokenCommon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TokenCommon")
            .field("node", &self.node.props.name)
            .field("depth", &self.depth)
            .finish()
    }
}

impl PartialEq for TokenCommon {
    fn eq(&self, other: &Self) -> bool {
        sync::Arc::<structure::Node>::ptr_eq(&self.node, &other.node) &&
            self.node_addr == other.node_addr &&
            self.depth == other.depth
    }
}

impl Eq for TokenCommon {
}

pub struct TokenTestFormat<'a>(pub TokenRef<'a>);

impl<'a> fmt::Display for TokenTestFormat<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            TokenRef::BlankLine(_) => write!(f, "<blank line>"),
            TokenRef::SummaryPreamble(_) => write!(f, "<summary preamble>"),
            TokenRef::SummaryEpilogue(_) => write!(f, "<summary epilogue>"),
            TokenRef::SummaryPunctuation(token) => write!(f, "{}", token.kind.as_str()),
            TokenRef::Title(token) => write!(f, "{}: ", &token.common.node.props.name),
            TokenRef::SummaryLabel(token) => write!(f, "{}: ", &token.common.node.props.name),
            TokenRef::Hexdump(token) => {
                for i in 0..token.extent.length().bytes {
                    write!(f, "{:02x}", (token.extent.begin.byte + i) & 0xff)?;
                    if i + 1 < token.extent.length().bytes {
                        write!(f, " ")?;
                    }
                }
                Ok(())
            },
            TokenRef::Hexstring(token) => {
                for i in 0..token.extent.length().bytes {
                    write!(f, "{:02x}", (token.extent.begin.byte + i) & 0xff)?
                }
                Ok(())
            },
        }
    }
}

impl<'a> fmt::Debug for TokenTestFormat<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl PunctuationKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            PunctuationKind::Space => "<space>",
            PunctuationKind::Comma => ", ",
            PunctuationKind::OpenBracket => "{",
            PunctuationKind::CloseBracket => "}",            
        }
    }

    pub fn accepts_cursor(&self) -> bool {
        match self {
            PunctuationKind::Space => false,
            _ => true
        }
    }
}

impl TokenKind for Token {
    fn common(&self) -> &TokenCommon {
        match self {
            /* Please keep 'common' as the first field in each of these structs. */
            Token::BlankLine(t) => t.common(),
            Token::SummaryPreamble(t) => t.common(),
            Token::SummaryEpilogue(t) => t.common(),
            Token::SummaryPunctuation(t) => t.common(),
            Token::Title(t) => t.common(),
            Token::SummaryLabel(t) => t.common(),
            Token::Hexdump(t) => t.common(),
            Token::Hexstring(t) => t.common(),
        }
    }

    fn into_token(self) -> Token {
        self
    }

    fn as_ref(&self) -> TokenRef<'_> {
        match &self {
            Token::BlankLine(t) => TokenRef::BlankLine(&t),
            Token::SummaryPreamble(t) => TokenRef::SummaryPreamble(&t),
            Token::SummaryEpilogue(t) => TokenRef::SummaryEpilogue(&t),
            Token::SummaryPunctuation(t) => TokenRef::SummaryPunctuation(&t),
            Token::Title(t) => TokenRef::Title(&t),
            Token::SummaryLabel(t) => TokenRef::SummaryLabel(&t),
            Token::Hexdump(t) => TokenRef::Hexdump(&t),
            Token::Hexstring(t) => TokenRef::Hexstring(&t),
        }
    }
}

impl TokenKind for BlankLineToken {
    fn common(&self) -> &TokenCommon {
        &self.common
    }
    
    fn into_token(self) -> Token {
        Token::BlankLine(self)
    }

    fn as_ref(&self) -> TokenRef<'_> {
        TokenRef::BlankLine(self)
    }
}

impl TokenKind for SummaryPreambleToken {
    fn common(&self) -> &TokenCommon {
        &self.common
    }
    
    fn into_token(self) -> Token {
        Token::SummaryPreamble(self)
    }

    fn as_ref(&self) -> TokenRef<'_> {
        TokenRef::SummaryPreamble(self)
    }
}

impl TokenKind for SummaryEpilogueToken {
    fn common(&self) -> &TokenCommon {
        &self.common
    }
    
    fn into_token(self) -> Token {
        Token::SummaryEpilogue(self)
    }

    fn as_ref(&self) -> TokenRef<'_> {
        TokenRef::SummaryEpilogue(self)
    }
}

impl TokenKind for SummaryPunctuationToken {
    fn common(&self) -> &TokenCommon {
        &self.common
    }
    
    fn into_token(self) -> Token {
        Token::SummaryPunctuation(self)
    }

    fn as_ref(&self) -> TokenRef<'_> {
        TokenRef::SummaryPunctuation(self)
    }
}

impl TokenKind for TitleToken {
    fn common(&self) -> &TokenCommon {
        &self.common
    }
    
    fn into_token(self) -> Token {
        Token::Title(self)
    }

    fn as_ref(&self) -> TokenRef<'_> {
        TokenRef::Title(self)
    }
}

impl TokenKind for SummaryLabelToken {
    fn common(&self) -> &TokenCommon {
        &self.common
    }
        
    fn into_token(self) -> Token {
        Token::SummaryLabel(self)
    }

    fn as_ref(&self) -> TokenRef<'_> {
        TokenRef::SummaryLabel(self)
    }
}

impl TokenKind for HexdumpToken {
    fn common(&self) -> &TokenCommon {
        &self.common
    }
    
    fn into_token(self) -> Token {
        Token::Hexdump(self)
    }

    fn as_ref(&self) -> TokenRef<'_> {
        TokenRef::Hexdump(self)
    }
}

impl HexdumpToken {
    pub fn absolute_extent(&self) -> addr::Extent {
        self.extent.rebase(self.common.node_addr)
    }
}

impl TokenKind for HexstringToken {
    fn common(&self) -> &TokenCommon {
        &self.common
    }
    
    fn into_token(self) -> Token {
        Token::Hexstring(self)
    }

    fn as_ref(&self) -> TokenRef<'_> {
        TokenRef::Hexstring(self)
    }
}

impl TokenCommon {
    pub fn adjust_depth(mut self, by: isize) -> Self {
        // TODO: change me if we can ever AddAssign isize to usize
        if by >= 0 {
            self.depth+= by as usize;
        } else {
            self.depth-= (-by) as usize;
        }

        self
    }
}
