use std::fmt;
use std::sync;

use crate::model::addr;
use crate::model::document::structure;

pub trait TokenKind {
    fn kind_name(&self) -> &'static str;
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

    fn node_child_index(&self) -> usize {
        self.common().node_child_index
    }
}

pub trait AsTokenRef<'a, Ref> {
    fn as_token_ref(&'a self) -> Ref;
}

macro_rules! declare_token_set {
    {
        $(#[doc = $outer_doc:literal])?
        $v:vis enum $name:ident {
            type Ref = $ref_name:ident;
            $($(#[doc = $doc:literal])? $member:ident),*
            $(,)?
        }
    } => {
        $(#[doc = $outer_doc])?
        #[derive(Clone, PartialEq, Eq)]
        $v enum $name {
            $(
                $(#[doc = $doc])?
                $member($member)
            ),*
        }
        
        #[derive(Clone, Copy, PartialEq, Eq)]
        $v enum $ref_name<'a> {
            $(
                $(#[doc = $doc])?
                $member(&'a $member)
            ),*
        }

        impl TokenKind for $name {
            fn kind_name(&self) -> &'static str {
                match self {
                    $(
                        $name::$member(_) => stringify!($member)
                    ),*
                }
            }

            fn common(&self) -> &TokenCommon {
                match &self {
                    $(
                        $name::$member(t) => &t.common()
                    ),*
                }
            }
        }

        impl<'a> TokenKind for $ref_name<'a> {
            fn kind_name(&self) -> &'static str {
                match self {
                    $(
                        $ref_name::$member(_) => stringify!($member)
                    ),*
                }
            }

            fn common(&self) -> &TokenCommon {
                match self {
                    $(
                        $ref_name::$member(t) => &t.common()
                    ),*
                }
            }
        }
        
        impl<'a> AsTokenRef<'a, $ref_name<'a>> for $name {
            fn as_token_ref(&'a self) -> $ref_name<'a> {
                match &self {
                    $(
                        $name::$member(t) => $ref_name::$member(&t)
                    ),*
                }
            }
        }
        
        $(
            impl Into<$name> for $member {
                fn into(self) -> $name {
                    $name::$member(self)
                }
            }
            
            impl<'a> AsTokenRef<'a, $ref_name<'a>> for $member {
                fn as_token_ref(&'a self) -> $ref_name<'a> {
                    $ref_name::$member(self)
                }
            }
        )*
    }
}

macro_rules! declare_tokens {
    {
        $(
            $(#[doc = $doc:literal])?
            $(#[derive($($derives:tt)*)])?
            $name:ident {
                $($fields:tt)*
            }
        ),*

        $(,)?
    } => {
        declare_token_set! {
            /// The smallest unit used to perform listing layout. Picture these as words which would be flowed into a textbox by a typesetting engine.
            pub enum Token {
                type Ref = TokenRef;
                
                $(
                    $(#[doc = $doc])?
                    $name
                ),*
            }
        }
        
        $(
            $(#[doc = $doc])?
            #[derive(Clone, PartialEq, Eq$(, $($derives)*)?)]
            pub struct $name {
                pub common: TokenCommon,
                $($fields)*
            }
            
            impl TokenKind for $name {
                fn kind_name(&self) -> &'static str {
                    stringify!($name)
                }
                
                fn common(&self) -> &TokenCommon {
                    &self.common
                }
            }
        )*

    }
}

declare_tokens! {
    /// An empty token, used to create a blank line
    #[derive(Debug)]
    BlankLine {
        pub accepts_cursor: bool,
    },

    /// Meta-token so line builder can tell where summaries start and end
    #[derive(Debug)]
    SummaryPreamble {
    },

    /// Meta-token so line builder can tell where summaries start and end
    #[derive(Debug)]
    SummaryEpilogue {
    },

    /// Punctuation for summaries.
    #[derive(Debug)]
    SummaryPunctuation {
        pub kind: PunctuationKind,
    },

    /// A title block to show the name of the structure node.
    #[derive(Debug)]
    Title {
    },

    /// The name of a child in a summary.
    #[derive(Debug)]
    SummaryLabel {
    },

    /// Formatted two-column hexdump+asciidump. What you expect to see out of a hex editor.
    #[derive(Debug)]
    Hexdump {
        pub extent: addr::Extent,
        pub line: addr::Extent,
    },
    
    /// Just a bunch of hex octets stuck together without any extra formatting.
    #[derive(Debug)]
    Hexstring {
        pub extent: addr::Extent,
        pub truncated: bool,
    },
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

    /* should not accept cursor */
    Ellipsis,
}

#[derive(Clone)]
pub struct TokenCommon {
    pub node: sync::Arc<structure::Node>,
    pub node_path: structure::Path,
    pub node_addr: addr::Address,
    pub node_child_index: usize,
    // TODO: colorization
    // colorization will be implemented by emitting multiple hexdump tokens on one line
    // all with the same "colorzation root" node, but different colorization vectors representing
    // all the children between the colorization root and where this token is.
    pub depth: usize,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let common = self.common();
        
        let mut ds = f.debug_struct("Token");
        
        let ds = ds.field("kind", &self.kind_name())
            .field("node", &common.node.props.name)
            .field("node_path", &common.node_path)
            .field("node_addr", &common.node_addr)
            .field("node_child_index", &common.node_child_index)
            .field("depth", &common.depth);

        match self {
            Token::Hexdump(hdt) => ds
                .field("extent", &hdt.extent)
                .field("line", &hdt.line),
            Token::SummaryPunctuation(pt) => ds
                .field("punctuation_kind", &pt.kind),
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
            .field("node_child_index", &common.node_child_index)
            .field("depth", &common.depth);

        match self {
            TokenRef::Hexdump(hdt) => ds
                .field("extent", &hdt.extent)
                .field("line", &hdt.line),
            TokenRef::SummaryPunctuation(pt) => ds
                .field("punctuation_kind", &pt.kind),
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
            self.node_child_index == other.node_child_index &&
            self.depth == other.depth
    }
}

impl Eq for TokenCommon {
}

pub struct TokenTestFormat<'a>(pub TokenRef<'a>);

impl<'a> fmt::Display for TokenTestFormat<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let i = self.0.node_child_index();
        
        match self.0 {
            TokenRef::BlankLine(_) => write!(f, "<blank line #{}>", i),
            TokenRef::SummaryPreamble(_) => write!(f, "<summary preamble #{}>", i),
            TokenRef::SummaryEpilogue(_) => write!(f, "<summary epilogue #{}>", i),
            TokenRef::SummaryPunctuation(token) => write!(f, "(#{}) {}", i, token.kind.as_str()),
            TokenRef::Title(token) => write!(f, "(#{}) {}: ", i, &token.common.node.props.name),
            TokenRef::SummaryLabel(token) => write!(f, "(#{}) {}: ", i, &token.common.node.props.name),
            TokenRef::Hexdump(token) => {
                write!(f, "(#{}) ", i)?;
                for j in 0..token.extent.length().bytes {
                    write!(f, "{:02x}", (token.extent.begin.byte + j) & 0xff)?;
                    if j + 1 < token.extent.length().bytes {
                        write!(f, " ")?;
                    }
                }
                Ok(())
            },
            TokenRef::Hexstring(token) => {
                write!(f, "(#{}) ", i)?;
                for j in 0..token.extent.length().bytes {
                    write!(f, "{:02x}", (token.extent.begin.byte + j) & 0xff)?
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
            PunctuationKind::Ellipsis => "...",
        }
    }

    pub fn accepts_cursor(&self) -> bool {
        match self {
            PunctuationKind::Space => false,
            _ => true
        }
    }
}

impl Hexdump {
    pub fn absolute_extent(&self) -> addr::Extent {
        self.extent.rebase(self.common.node_addr)
    }
}

impl Hexstring {
    pub fn new_maybe_truncate(common: TokenCommon, extent: addr::Extent) -> Hexstring {
        // TODO: make this configurable
        const LIMIT: addr::Size = addr::Size::new(16);

        if extent.length() > LIMIT {
            Hexstring {
                common,
                extent: addr::Extent::sized(extent.begin, LIMIT),
                truncated: true,
            }
        } else {
            Hexstring {
                common,
                extent,
                truncated: false,
            }
        }
    }

    pub fn absolute_extent(&self) -> addr::Extent {
        self.extent.rebase(self.common.node_addr)
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
