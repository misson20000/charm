//! This module includes the logic that converts from a document structure
//! hierarchy into a seekable stream of tokens.

// TODO: rework the concept of a tokenizer into a TokenCursor or
// something like it.  also, to reconcile the two different
// interpretations of movement (re: turning around directions, whether
// a position is on an token or on a border), we should expose two
// wrapper unit types that you have to do all movement through to
// specify which type of movement you want.

use std::sync;

use crate::model::addr;
use crate::model::listing::token;
use crate::model::document;
use crate::model::document::structure;
use crate::model::document::change;

#[derive(Clone, Debug)]
enum TokenizerState {
    PreBlank,
    Title,
    
    MetaContent(addr::Address, usize),
    Hexdump(addr::Extent, usize),
    Hexstring(addr::Extent, usize),
    
    SummaryOpener,
    /// The argument here is an index for which child is being labelled. Does not tolerate one-past-the-end.
    SummaryLabel(usize),
    /// The argument here is an index for which child comes before the separator. Does not tolerate one-past-the-end.
    /// We still go through this state for the last child, even though it doesn't have a separator after it,
    /// we just suppress that token when it comes time to generate it.
    SummarySeparator(usize),
    SummaryCloser,
    SummaryNewline,

    SummaryValueBegin,
    SummaryLeaf,
    SummaryValueEnd,

    PostBlank,
    End,
}

#[derive(Clone)]
pub struct TokenizerStackEntry {
    stack: Option<sync::Arc<TokenizerStackEntry>>,
    before_state: TokenizerState,
    after_state: TokenizerState,
    child_index: Option<usize>,
    depth: usize,
    node: sync::Arc<structure::Node>,
    node_addr: addr::Address,    
}

#[derive(Clone)]
pub struct Tokenizer {
    /* invariants:
       - stack should always contain a path all the way back to the root node
     */
    stack: Option<sync::Arc<TokenizerStackEntry>>,
    state: TokenizerState,
    depth: usize,
    node: sync::Arc<structure::Node>,
    node_addr: addr::Address,
}

pub enum TokenGenerationResult {
    Ok(token::Token),
    Skip,
    Boundary,
}

enum AscendDirection {
    Prev, Next
}

struct TokenizerStackDebugHelper<'a>(&'a Option<sync::Arc<TokenizerStackEntry>>);

impl std::fmt::Debug for Tokenizer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Tokenizer")
            .field("state", &self.state)
            .field("node", &self.node.props.name)
            .field("stack", &TokenizerStackDebugHelper(&self.stack))
            .finish_non_exhaustive()
    }
}

impl<'a> std::fmt::Debug for TokenizerStackDebugHelper<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut dl = f.debug_list();
        let mut i = self.0;

        while let Some(entry) = i {
            dl.entry(entry);
            i = &entry.stack;
        }

        dl.finish()
    }
}

impl std::fmt::Debug for TokenizerStackEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Entry")
            .field("before_state", &self.before_state)
            .field("after_state", &self.after_state)
            .field("node", &self.node.props.name)
            .finish_non_exhaustive()
    }
}

impl Tokenizer {
    /// Creates a new tokenizer seeked to the root of the structure hierarchy and the beginning of the token stream.
    pub fn at_beginning(root: sync::Arc<structure::Node>) -> Tokenizer {
        Tokenizer {
            stack: None,
            state: TokenizerState::PreBlank,
            depth: 0,
            node: root,
            node_addr: addr::unit::NULL,
        }
    }

    pub fn at_address(_root: sync::Arc<structure::Node>, _addr: addr::Address) -> Tokenizer {
        todo!();
    }

    pub fn port_doc(&mut self, old_doc: &document::Document, new_doc: &document::Document) {
        if old_doc.is_outdated(new_doc) {
            match &new_doc.previous {
                Some((prev_doc, change)) => {
                    self.port_doc(old_doc, prev_doc);
                    self.port_change(&new_doc.root, change);
                },
                None => panic!("no common ancestor")
            }
        }
    }
    
    fn port_change(&mut self, new_root: &sync::Arc<structure::Node>, change: &change::Change) {
        let (new_stack, node) = match &self.stack {
            Some(parent) => {
                let (s, n) = Self::port_descend(&parent, new_root, change);
                (Some(sync::Arc::new(s)), n)
            },
            None => (None, new_root.clone())
        };
            
        /* create Tokenizer from (new_stack, node) */
        *self = Tokenizer {
            stack: new_stack,
            state: std::mem::replace(&mut self.state, TokenizerState::PreBlank),
            depth: self.depth,
            node: node,
            node_addr: self.node_addr,
        }
    }

    fn port_node(old_tok: &TokenizerStackEntry, new_stack: Option<sync::Arc<TokenizerStackEntry>>, new_node: sync::Arc<structure::Node>, _change: &change::Change) -> (TokenizerStackEntry, sync::Arc<structure::Node>) {
        (TokenizerStackEntry {
            stack: new_stack,
            before_state: old_tok.before_state.clone(),
            child_index: old_tok.child_index,
            after_state: old_tok.after_state.clone(),
            depth: old_tok.depth,
            node: new_node.clone(),
            node_addr: old_tok.node_addr,
        }, match old_tok.before_state {
            TokenizerState::MetaContent(_, i) => new_node.children[i].node.clone(),
            _ => todo!(),
        })
    }
    
    fn port_descend(tok: &TokenizerStackEntry, new_root: &sync::Arc<structure::Node>, change: &change::Change) -> (TokenizerStackEntry, sync::Arc<structure::Node>) {
        match &tok.stack {
            Some(parent) => {
                let (new_stack, new_node) = Self::port_descend(&parent, new_root, change);
                Self::port_node(tok, Some(sync::Arc::new(new_stack)), new_node, change)
            },
            None => {
                /* reached root */
                Self::port_node(tok, None, new_root.clone(), change)
            }
        }
    }

    /// Creates a new tokenizer seeked to the end of the token stream.
    pub fn at_end(root: &sync::Arc<structure::Node>) -> Tokenizer {
        Tokenizer {
            stack: None,
            state: TokenizerState::End,
            depth: 0,
            node: root.clone(),
            node_addr: addr::unit::NULL,
        }
    }

    pub fn gen_token(&self) -> TokenGenerationResult {
        match self.state {
            TokenizerState::PreBlank => if self.node.props.title_display.has_blanks() {
                TokenGenerationResult::Ok(token::Token {
                    class: token::TokenClass::Punctuation(token::PunctuationClass::Empty),
                    node: self.node.clone(),
                    node_addr: self.node_addr,
                    depth: self.depth,
                    newline: true,
                })
            } else {
                TokenGenerationResult::Skip
            },
            TokenizerState::Title => TokenGenerationResult::Ok(token::Token {
                class: token::TokenClass::Title,
                node: self.node.clone(),
                node_addr: self.node_addr,
                depth: self.depth,
                newline: !self.node.props.title_display.is_inline(),
            }),
            
            TokenizerState::MetaContent(_, _) => TokenGenerationResult::Skip,
            TokenizerState::Hexdump(extent, _) => TokenGenerationResult::Ok(token::Token {
                class: token::TokenClass::Hexdump(extent),
                node: self.node.clone(),
                node_addr: self.node_addr,
                depth: self.depth + 1,
                newline: true,
            }),
            TokenizerState::Hexstring(extent, _) => TokenGenerationResult::Ok(token::Token {
                class: token::TokenClass::Hexstring(extent),
                node: self.node.clone(),
                node_addr: self.node_addr,
                depth: self.depth + 1,
                newline: true,
            }),
            
            TokenizerState::SummaryOpener => TokenGenerationResult::Ok(token::Token {
                class: token::TokenClass::Punctuation(token::PunctuationClass::OpenBracket),
                node: self.node.clone(),
                node_addr: self.node_addr,
                depth: self.depth,
                newline: false,
            }),
            TokenizerState::SummaryLabel(i) => {
                let ch = &self.node.children[i];
                TokenGenerationResult::Ok(token::Token {
                    class: token::TokenClass::SummaryLabel,
                    node: ch.node.clone(),
                    node_addr: self.node_addr + ch.offset.to_size(),
                    depth: self.depth,
                    newline: false,
                })
            },
            TokenizerState::SummarySeparator(i) => if i+1 < self.node.children.len() {
                TokenGenerationResult::Ok(token::Token {  
                    class: token::TokenClass::Punctuation(token::PunctuationClass::Comma),
                    node: self.node.clone(),
                    node_addr: self.node_addr,
                    depth: self.depth,
                    newline: false,
                })
            } else {
                TokenGenerationResult::Skip
            },
            TokenizerState::SummaryCloser => TokenGenerationResult::Ok(token::Token {
                class: token::TokenClass::Punctuation(token::PunctuationClass::CloseBracket),
                node: self.node.clone(),
                node_addr: self.node_addr,
                depth: self.depth,
                newline: false,
            }),
            TokenizerState::SummaryNewline => TokenGenerationResult::Ok(token::Token {
                class: token::TokenClass::Punctuation(token::PunctuationClass::Empty),
                node: self.node.clone(),
                node_addr: self.node_addr,
                depth: self.depth,
                newline: true,
            }),
            
            TokenizerState::SummaryValueBegin => TokenGenerationResult::Skip,
            TokenizerState::SummaryLeaf => {
                let limit = std::cmp::min(16.into(), self.node.size);
                let extent = addr::Extent::between(addr::unit::NULL, limit.to_addr());
                
                TokenGenerationResult::Ok(token::Token {
                    class: match self.node.props.content_display {
                        structure::ContentDisplay::None => token::TokenClass::Punctuation(token::PunctuationClass::Empty),
                        structure::ContentDisplay::Hexdump(_) => token::TokenClass::Hexdump(extent),
                        structure::ContentDisplay::Hexstring => token::TokenClass::Hexstring(extent),
                    },
                    node: self.node.clone(),
                    node_addr: self.node_addr,
                    depth: self.depth,
                    newline: false,
                })
            },
            TokenizerState::SummaryValueEnd => TokenGenerationResult::Skip,

            TokenizerState::PostBlank => if self.node.props.title_display.has_blanks() {
                TokenGenerationResult::Ok(token::Token {
                    class: token::TokenClass::Punctuation(token::PunctuationClass::Empty),
                    node: self.node.clone(),
                    node_addr: self.node_addr,
                    depth: self.depth,
                    newline: true,
                })
            } else {
                TokenGenerationResult::Skip
            },
            TokenizerState::End => TokenGenerationResult::Skip,
        }
    }

    /// Moves one (potential) token backwards in the stream.
    /// Returns true when successful, or false if hit the beginning of the token stream.
    fn move_prev(&mut self) -> bool {
        match self.state {
            TokenizerState::PreBlank => {
                self.try_ascend(AscendDirection::Prev)
            },
            TokenizerState::Title => {
                self.state = TokenizerState::PreBlank;
                true
            },
            
            TokenizerState::MetaContent(offset, index) => {
                let prev_child_option = match index {
                    0 => None,
                    /* Something is seriously wrong if index was farther than one-past-the-end. */
                    i => Some((i-1, &self.node.children[i-1]))
                };

                /* Descend, if we can. */
                if let Some((prev_child_index, prev_child)) = prev_child_option {
                    if prev_child.end() >= offset {
                        self.descend(
                            prev_child_index,
                            /* If we ascend going backwards, ascend to immediately before this child. */
                            TokenizerState::MetaContent(prev_child.offset, index - 1),
                            /* If we ascend going forwards, ascend to immediately after this child. */
                            TokenizerState::MetaContent(prev_child.end(), index),
                            /* Descend to the end of the child. */
                            TokenizerState::End);

                        return true;
                    }
                }

                /* Emit content, if we can. */
                if offset > addr::unit::NULL {
                    /* Where would we *like* to begin, as decided by our content's preferred pitch? */
                    let preferred_begin = self.node.props.content_display.preferred_pitch().map(|pitch| {
                        (pitch * ((offset - addr::unit::BIT).to_size() / pitch)).to_addr()
                    });

                    /* Where can we not begin before? */
                    let limit = match prev_child_option {
                        /* Can't include data from the child, so need to stop after its end. */
                        Some((_, prev_child)) => prev_child.end(),
                        /* Can't include data that belongs to the parent, so need to stop before our begin. */
                        None => addr::unit::NULL,
                    };

                    /* Pick a place to begin this line. */
                    let begin = preferred_begin.map_or(limit, |pb| std::cmp::max(pb, limit));

                    let extent = addr::Extent::between(begin, offset);
                        
                    self.state = match self.node.props.content_display {
                        structure::ContentDisplay::None => TokenizerState::MetaContent(limit, index),
                        structure::ContentDisplay::Hexdump(_) => TokenizerState::Hexdump(extent, index),
                        structure::ContentDisplay::Hexstring => TokenizerState::Hexstring(extent, index),
                    };
                    
                    return true;
                }
                
                /* We're pointed at the beginning. Emit the title block. */
                self.state = TokenizerState::Title;
                true
            },
            TokenizerState::Hexstring(extent, index) => {
                self.state = TokenizerState::MetaContent(extent.begin, index);
                true
            },
            TokenizerState::Hexdump(extent, index) => {
                self.state = TokenizerState::MetaContent(extent.begin, index);
                true
            },

            TokenizerState::SummaryOpener => {
                self.try_ascend(AscendDirection::Prev)
            },
            TokenizerState::SummaryLabel(i) => {
                if i == 0 {
                    self.state = TokenizerState::SummaryOpener;
                } else {
                    self.state = TokenizerState::SummarySeparator(i-1);
                }
                true
            },
            TokenizerState::SummarySeparator(i) => {
                self.descend(
                    i,
                    TokenizerState::SummaryLabel(i),
                    TokenizerState::SummarySeparator(i),
                    TokenizerState::SummaryValueEnd);
                true
            },
            TokenizerState::SummaryCloser => {
                if self.node.children.len() == 0 {
                    self.state = TokenizerState::SummaryOpener;
                } else {
                    self.state = TokenizerState::SummarySeparator(self.node.children.len()-1);
                }
                true
            },
            TokenizerState::SummaryNewline => {
                self.descend_self(
                    TokenizerState::Title,
                    TokenizerState::SummaryNewline,
                    TokenizerState::SummaryCloser);
                true
            },
            
            TokenizerState::SummaryValueBegin => {
                // should take us to SummaryLabel(i)
                self.try_ascend(AscendDirection::Prev)
            },
            TokenizerState::SummaryLeaf => {
                self.state = TokenizerState::SummaryValueBegin;
                true
            },
            TokenizerState::SummaryValueEnd => {
                if self.node.children.len() == 0 {
                    self.state = TokenizerState::SummaryLeaf;
                } else {
                    self.state = TokenizerState::SummaryCloser;
                }
                true
            },

            TokenizerState::PostBlank => {
                match self.node.props.children_display {
                    structure::ChildrenDisplay::None => {
                        self.state = TokenizerState::MetaContent(self.node.size.to_addr(), self.node.children.len());
                    },
                    structure::ChildrenDisplay::Summary => {
                        self.state = TokenizerState::SummaryNewline;
                    },
                    structure::ChildrenDisplay::Full => {
                        self.state = TokenizerState::MetaContent(self.node.size.to_addr(), self.node.children.len());
                    },
                }
                true
            },
            TokenizerState::End => {
                self.state = TokenizerState::PostBlank;
                true
            },
        }
    }

    /// Moves one (potential) token forwards in the stream.
    /// Returns true when successful, or false if hit the end of the token stream.
    fn move_next(&mut self) -> bool {
        match self.state {
            TokenizerState::PreBlank => {
                self.state = TokenizerState::Title;
                true
            },
            TokenizerState::Title => {
                match self.node.props.children_display {
                    structure::ChildrenDisplay::None => {
                        self.state = TokenizerState::MetaContent(addr::unit::NULL, 0);
                    },
                    structure::ChildrenDisplay::Summary => {
                        self.descend_self(
                            TokenizerState::Title,
                            TokenizerState::SummaryNewline,
                            TokenizerState::SummaryOpener);
                    },
                    structure::ChildrenDisplay::Full => {
                        self.state = TokenizerState::MetaContent(addr::unit::NULL, 0);
                    },
                }
                true
            },
            TokenizerState::MetaContent(offset, index) => {
                let next_child_option = self.node.children.get(index).map(|child| (index, child));
                
                /* Descend, if we can. */
                if let Some((next_child_index, next_child)) = next_child_option {
                    if next_child.offset <= offset {
                        self.descend(
                            next_child_index,
                            /* If we ascend going backwards, ascend to before this child. */
                            TokenizerState::MetaContent(next_child.offset, index),
                            /* If we ascend going forwards, ascend to after this child. */
                            TokenizerState::MetaContent(next_child.end(), index + 1),
                            /* Descend to the beginning of the child. */
                            TokenizerState::PreBlank);

                        return true;
                    }
                }

                /* Emit content, if we can. */
                if offset < self.node.size.to_addr() {
                    /* Where would we *like* to end, as decided by our content's preferred pitch? */
                    let preferred_end = self.node.props.content_display.preferred_pitch().map(|pitch| {
                        (pitch * ((offset.to_size() / pitch) + 1)).to_addr()
                    });

                    /* Where can we not end beyond? */
                    let limit = match next_child_option {
                        /* Can't include data from the child, so need to stop before it begins. */
                        Some((_, next_child)) => next_child.offset,
                        /* Can't include data that belongs to the parent, so need to stop before we end. */
                        None => self.node.size.to_addr(),
                    };

                    /* Pick a place to end this line. */
                    let end = preferred_end.map_or(limit, |pe| std::cmp::min(pe, limit));

                    let extent = addr::Extent::between(offset, end);

                    self.state = match self.node.props.content_display {
                        structure::ContentDisplay::None => TokenizerState::MetaContent(limit, index),
                        structure::ContentDisplay::Hexdump(_) => TokenizerState::Hexdump(extent, index),
                        structure::ContentDisplay::Hexstring => TokenizerState::Hexstring(extent, index),
                    };

                    return true;
                }

                /* We were pointed at (or past!) the end. */
                self.state = TokenizerState::PostBlank;
                true
            },
            TokenizerState::Hexstring(extent, index) => {
                self.state = TokenizerState::MetaContent(extent.end, index);
                true
            },
            TokenizerState::Hexdump(extent, index) => {
                self.state = TokenizerState::MetaContent(extent.end, index);
                true
            },

            TokenizerState::SummaryOpener => {
                if self.node.children.len() == 0 {
                    self.state = TokenizerState::SummaryCloser;
                } else {
                    self.state = TokenizerState::SummaryLabel(0);
                }
                true
            },
            TokenizerState::SummaryLabel(i) => {
                self.descend(
                    i,
                    TokenizerState::SummaryLabel(i),
                    TokenizerState::SummarySeparator(i),
                    TokenizerState::SummaryValueBegin);
                true
            },
            TokenizerState::SummarySeparator(i) => {
                if self.node.children.len() == i + 1 {
                    self.state = TokenizerState::SummaryCloser;
                } else {
                    self.state = TokenizerState::SummaryLabel(i+1);
                }
                true
            },
            TokenizerState::SummaryCloser => {
                self.try_ascend(AscendDirection::Next)
            },
            TokenizerState::SummaryNewline => {
                self.state = TokenizerState::PostBlank;
                true
            },

            TokenizerState::SummaryValueBegin => {
                if self.node.children.len() == 0 {
                    self.state = TokenizerState::SummaryLeaf;
                } else {
                    self.state = TokenizerState::SummaryOpener;
                }
                true
            },
            TokenizerState::SummaryLeaf => {
                self.state = TokenizerState::SummaryValueEnd;
                true
            },
            TokenizerState::SummaryValueEnd => {
                self.try_ascend(AscendDirection::Next)
            },

            TokenizerState::PostBlank => {
                self.state = TokenizerState::End;
                true
            },
            TokenizerState::End => {
                self.try_ascend(AscendDirection::Next)
            },
        }
    }
    
    pub fn prev(&mut self) -> Option<token::Token> {
        while self.move_prev() {
            match self.gen_token() {
                TokenGenerationResult::Ok(token) => return Some(token),
                TokenGenerationResult::Skip => continue,
                TokenGenerationResult::Boundary => return None,
            }
        }
        None
    }
    /// Use this when you're trying to have the tokenizer's position represent an element.
    pub fn next_preincrement(&mut self) -> Option<token::Token> {
        while {
            self.move_next()
        } {
            match self.gen_token() {
                TokenGenerationResult::Ok(token) => return Some(token),
                TokenGenerationResult::Skip => continue,
                TokenGenerationResult::Boundary => return None,
            }
        }
        None
    }
    
    /// Use this when you're trying to have the tokenizer's position represent a border between tokens.
    pub fn next_postincrement(&mut self) -> Option<token::Token> {
        let mut token;
        while {
            token = self.gen_token();
            self.move_next()
        } {
            match token {
                TokenGenerationResult::Ok(token) => return Some(token),
                TokenGenerationResult::Skip => continue,
                TokenGenerationResult::Boundary => return None,
            }
        }
        None
    }

    /// Pushes an entry onto the tokenizer stack and sets up for traversing
    /// a child node.
    ///
    /// # Arguments
    ///
    /// * `into_child` - The index of the child to descend into.
    /// * `state_before` - A TokenizerState positioned immediately before the child from the parent's perspective.
    /// * `state_after` - A TokenizerState positioned immediately after the child from the parent's perspective.
    /// * `state_within` - Where within the child to descend to.
    ///
    fn descend(
        &mut self,
        into_child: usize,
        state_before: TokenizerState,
        state_after: TokenizerState,
        state_within: TokenizerState) {
        let childhood = self.node.children[into_child].clone();
        
        let parent_node = std::mem::replace(&mut self.node, childhood.node);
        
        let parent_entry = TokenizerStackEntry {
            stack: self.stack.take(),
            before_state: state_before,
            child_index: Some(into_child),
            after_state: state_after,
            depth: self.depth,
            node: parent_node,
            node_addr: self.node_addr,
        };

        self.depth+= 1;
        self.stack = Some(sync::Arc::new(parent_entry));
        self.state = state_within;
        self.node_addr+= childhood.offset.to_size();
    }

    fn descend_self(
        &mut self,
        state_before: TokenizerState,
        state_after: TokenizerState,
        state_within: TokenizerState) {
        self.stack = Some(sync::Arc::new(TokenizerStackEntry {
            stack: self.stack.take(),
            before_state: state_before,
            child_index: None,
            after_state: state_after,
            depth: self.depth,
            node: self.node.clone(),
            node_addr: self.node_addr,
        }));
        self.state = state_within;
    }        
    
    /// Replaces our context with the parent's context, returning false if there
    /// was no parent.
    fn try_ascend(&mut self, dir: AscendDirection) -> bool {
        match std::mem::replace(&mut self.stack, None) {
            Some(replacement) => {
                // TODO: replace this with unwrap_or_clone when it gets stabilized
                //       https://github.com/rust-lang/rust/issues/93610
                let replacement = sync::Arc::try_unwrap(replacement).unwrap_or_else(|arc| (*arc).clone());
                *self = Tokenizer {
                    stack: replacement.stack,
                    state: match dir {
                        AscendDirection::Prev => replacement.before_state,
                        AscendDirection::Next => replacement.after_state
                    },
                    depth: replacement.depth,
                    node: replacement.node.clone(),
                    node_addr: replacement.node_addr,
                };
                true
            },
            None => false
        }
    }
    
    pub fn hit_bottom(&self) -> bool {
        match self.state {
            TokenizerState::End => self.stack.is_none(),
            _ => false
        }
    }

    pub fn hit_top(&self) -> bool {
        match self.state {
            TokenizerState::PreBlank => self.stack.is_none(),
            _ => false
        }
    }
    
    pub fn structure_path(&self) -> structure::Path {
        let mut path = Vec::new();

        TokenizerStackEntry::build_path(&self.stack, &mut path);
        
        path
    }

    pub fn structure_position_child(&self) -> usize {
        match self.state {
            TokenizerState::MetaContent(_, ch) => ch,
            TokenizerState::Hexdump(_, ch) => ch,
            TokenizerState::Hexstring(_, ch) => ch,
            TokenizerState::SummaryLabel(ch) => ch,
            TokenizerState::SummarySeparator(ch) => ch,
            TokenizerState::SummaryCloser => self.node.children.len(),
            TokenizerState::PostBlank => self.node.children.len(),
            TokenizerState::End => self.node.children.len(),
            _ => 0,
        }
    }

    pub fn structure_position_offset(&self) -> addr::Address {
        match self.state {
            TokenizerState::MetaContent(offset, _) => offset,
            TokenizerState::Hexdump(extent, _) => extent.begin,
            TokenizerState::Hexstring(extent, _) => extent.begin,
            // TODO: probably some missing here, need to figure out what is intuitive to the user.
            _ => addr::unit::NULL
        }
    }
}

impl TokenizerStackEntry {
    fn build_path(entry: &Option<sync::Arc<TokenizerStackEntry>>, path: &mut structure::Path) {
        if let Some(tse) = entry {
            Self::build_path(&tse.stack, path);
            if let Some(child_index) = tse.child_index {
                path.push(child_index);
            }
        }
    }
}

pub mod xml {
    use super::*;

    use std::collections;
    use std::sync;
    use std::vec;
    
    extern crate roxmltree;

    pub struct Testcase {
        pub structure: sync::Arc<structure::Node>,
        pub expected_tokens: vec::Vec<token::Token>,
    }

    struct TokenDef {
        class: token::TokenClass,
        node_name: String,
        depth: usize,
        newline: bool,
    }

    impl Testcase {
        pub fn from_xml(document: &roxmltree::Document) -> Testcase {
            let re = document.root_element();
            assert!(re.has_tag_name("testcase"));

            let mut lookup = collections::HashMap::new();
            let mut structure = None;
            let mut expected_tokens: Option<Vec<TokenDef>> = None;
            
            for child in re.children() {
                if !child.is_element() { continue; }
                match child.tag_name().name() {
                    "node" => {
                        structure = match structure {
                            Some(_) => panic!("multiple structure definitions"),
                            None => Some(inflate_structure(child, addr::unit::NULL, &mut lookup))
                        }
                    }
                    "tokens" => {
                        expected_tokens = match expected_tokens {
                            Some(_) => panic!("multiple expected tokens"),
                            None => {
                                let mut vec = vec::Vec::new();
                                inflate_token_tree(child, &mut vec, 0);
                                Some(vec)
                            }
                        }
                    },
                    tn => panic!("unexpected tag '{}'", tn)
                }
            }

            Testcase {
                structure: structure.expect("should've had a structure definition"),
                expected_tokens: expected_tokens.expect("should've had expected tokens").into_iter().map(|c| c.to_token(&lookup)).collect(),
            }
        }
    }

    fn inflate_token_tree(xml: roxmltree::Node, collection: &mut vec::Vec<TokenDef>, depth: usize) {
        for c in xml.children().filter(|c| c.is_element()) {
            if c.has_tag_name("indent") {
                inflate_token_tree(c, collection, depth + 1)
            } else {
                collection.push(TokenDef {
                    class: match c.tag_name().name() {
                        "null" => token::TokenClass::Punctuation(token::PunctuationClass::Empty),
                        "open" => token::TokenClass::Punctuation(token::PunctuationClass::OpenBracket),
                        "comma" => token::TokenClass::Punctuation(token::PunctuationClass::Comma),
                        "close" => token::TokenClass::Punctuation(token::PunctuationClass::CloseBracket),
                        "title" => token::TokenClass::Title,
                        "summlabel" => token::TokenClass::SummaryLabel,
                        "hexdump" => token::TokenClass::Hexdump(inflate_extent(&c)),
                        "hexstring" => token::TokenClass::Hexstring(inflate_extent(&c)),
                        tn => panic!("invalid token def: '{}'", tn)
                    },
                    node_name: c.attribute("node").unwrap().to_string(),
                    depth,
                    newline: c.attribute("nl").unwrap().eq("true"),
                })
            }
        }
    }

    fn inflate_extent(xml: &roxmltree::Node) -> addr::Extent {
        addr::Extent::between(
            addr::Address::parse(xml.attribute("begin").unwrap()).unwrap(),
            addr::Address::parse(xml.attribute("end").unwrap()).unwrap()
        )
    }
        
    fn inflate_childhood(xml: roxmltree::Node, parent_addr: addr::Address, map: &mut collections::HashMap<String, (addr::Address, sync::Arc<structure::Node>)>) -> structure::Childhood {
        let offset = match xml.attribute("offset") {
            Some(addr) => addr::Address::parse(addr).unwrap(),
            None => addr::unit::NULL
        };
        structure::Childhood {
            node: inflate_structure(xml, parent_addr + offset.to_size(), map),
            offset,
        }
    }
        
    pub fn inflate_structure(xml: roxmltree::Node, node_addr: addr::Address, map: &mut collections::HashMap<String, (addr::Address, sync::Arc<structure::Node>)>) -> sync::Arc<structure::Node> {
        let node = structure::Node {
            size: addr::Address::parse(xml.attribute("size").unwrap()).unwrap().to_size(),
            props: structure::Properties {
                name: xml.attribute("name").unwrap().to_string(),
                title_display: match xml.attribute("title") {
                    None => structure::TitleDisplay::Major,
                    Some("major") => structure::TitleDisplay::Major,
                    Some("minor") => structure::TitleDisplay::Minor,
                    Some("inline") => structure::TitleDisplay::Inline,
                    Some(invalid) => panic!("invalid title attribute: {}", invalid)
                },
                children_display: match xml.attribute("children") {
                    None => structure::ChildrenDisplay::Full,
                    Some("none") => structure::ChildrenDisplay::None,
                    Some("summary") => structure::ChildrenDisplay::Summary,
                    Some("full") => structure::ChildrenDisplay::Full,
                    Some(invalid) => panic!("invalid children attribute: {}", invalid)
                },
                content_display: match xml.attribute("content") {
                    None => structure::ContentDisplay::Hexdump(16.into()),
                    Some("hexstring") => structure::ContentDisplay::Hexstring,
                    Some("hexdump") => structure::ContentDisplay::Hexdump(
                        xml.attribute("pitch").map_or(
                            16.into(),
                            |p| addr::Address::parse(p).map_or_else(                                
                                |e| panic!("expected valid pitch, got '{}' ({:?})", p, e),
                                |a| a.to_size()))),
                    Some("none") => structure::ContentDisplay::None,
                    Some(invalid) => panic!("invalid content attribute: {}", invalid)
                },
                locked: true,
            },
            children: xml.children().filter(|c| c.is_element()).map(|c| inflate_childhood(c, node_addr, map)).collect()
        };
        let arc = sync::Arc::new(node);
        map.insert(arc.props.name.clone(), (node_addr, arc.clone()));
        arc
    }

    impl TokenDef {
        fn to_token(self, lookup: &collections::HashMap<String, (addr::Address, sync::Arc<structure::Node>)>) -> token::Token {
            let lookup_result = lookup.get(&self.node_name).expect(&format!("expected a node named '{}'", self.node_name));
            token::Token {
                class: self.class,
                node: lookup_result.1.clone(),
                node_addr: lookup_result.0,
                depth: self.depth,
                newline: self.newline
            }
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    extern crate roxmltree;

    use std::iter;
    use std::vec;

    struct DownwardTokenizerIterator(Tokenizer);
    struct UpwardTokenizerIterator(Tokenizer);

    impl iter::Iterator for DownwardTokenizerIterator {
        type Item = token::Token;
        
        fn next(&mut self) -> Option<token::Token> {
            let a = self.0.next_postincrement();
            if a.is_some() {
                let b = self.0.next_postincrement();
                if b.is_some() {
                    assert_eq!(b, self.0.prev());
                }
                assert_eq!(a, self.0.prev());
                assert_eq!(a, self.0.next_postincrement());
            }
            a
        }
    }

    impl iter::Iterator for UpwardTokenizerIterator {
        type Item = token::Token;
        
        fn next(&mut self) -> Option<token::Token> {
            let a = self.0.prev();
            if a.is_some() {
                let b = self.0.prev();
                if b.is_some() {
                    assert_eq!(b, self.0.next_postincrement());
                }
                assert_eq!(a, self.0.next_postincrement());
                assert_eq!(a, self.0.prev());
            }
            a
        }
    }
    
    fn parse_testcase(xml: &[u8]) -> xml::Testcase {
        let document = match roxmltree::Document::parse(std::str::from_utf8(xml).unwrap()) {
            Ok(document) => document,
            Err(e) => panic!("{}", e)
        };

        xml::Testcase::from_xml(&document)
    }

    fn test_forward(tc: &xml::Testcase) {
        itertools::assert_equal(
            tc.expected_tokens.iter().map(|x| x.clone()),
            &mut DownwardTokenizerIterator(Tokenizer::at_beginning(tc.structure.clone())));
    }

    fn test_backward(tc: &xml::Testcase) {
        itertools::assert_equal(
            tc.expected_tokens.iter().rev().map(|x| x.clone()),
            &mut UpwardTokenizerIterator(Tokenizer::at_end(&tc.structure)));
    }
    
    #[test]
    fn simple() {
        let tc = parse_testcase(include_bytes!("tokenizer_tests/simple.xml"));
        test_forward(&tc);
        test_backward(&tc);
    }

    #[test]
    fn nesting() {
        let tc = parse_testcase(include_bytes!("tokenizer_tests/nesting.xml"));
        test_forward(&tc);
        test_backward(&tc);
    }

    #[test]
    fn formatting() {
        let tc = parse_testcase(include_bytes!("tokenizer_tests/formatting.xml"));
        test_forward(&tc);
        test_backward(&tc);
    }

    #[test]
    fn content_display() {
        let tc = parse_testcase(include_bytes!("tokenizer_tests/content_display.xml"));
        test_forward(&tc);
        test_backward(&tc);
    }

    #[test]
    fn summary() {
        let tc = parse_testcase(include_bytes!("tokenizer_tests/summary.xml"));
        test_forward(&tc);
        test_backward(&tc);
    }
    
    #[test]
    fn hardcoded() {
        let mut root = structure::Node {
            size: addr::Size::from(0x70),
            props: structure::Properties {
                name: "root".to_string(),
                title_display: structure::TitleDisplay::Major,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::Hexdump(16.into()),
                locked: true,
            },
            children: vec::Vec::new()
        };

        let child = sync::Arc::new(structure::Node {
            size: addr::Size::from(0x18),
            props: structure::Properties {
                name: "child".to_string(),
                title_display: structure::TitleDisplay::Major,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::Hexdump(16.into()),
                locked: true,
            },
            children: vec::Vec::new()
        });
        
        root.children.push(structure::Childhood {
            node: child.clone(),
            offset: addr::Address::from(0x32)
        });

        let root = sync::Arc::new(root);

        let expected_tokens = vec![
            /* root */
            token::Token {
                class: token::TokenClass::Punctuation(token::PunctuationClass::Empty),
                node: root.clone(), node_addr: 0.into(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Title,
                node: root.clone(), node_addr: 0.into(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x0, 0x10)),
                node: root.clone(), node_addr: 0.into(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x10, 0x20)),
                node: root.clone(), node_addr: 0.into(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x20, 0x30)),
                node: root.clone(), node_addr: 0.into(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x30, 0x32)),
                node: root.clone(), node_addr: 0.into(), depth: 0, newline: true
            },
            /* child */
            token::Token {
                class: token::TokenClass::Punctuation(token::PunctuationClass::Empty),
                node: child.clone(), node_addr: 0x32.into(), depth: 1, newline: true
            },
            token::Token {
                class: token::TokenClass::Title,
                node: child.clone(), node_addr: 0x32.into(), depth: 1, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x0, 0x10)),
                node: child.clone(), node_addr: 0x32.into(), depth: 1, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x10, 0x18)),
                node: child.clone(), node_addr: 0x32.into(), depth: 1, newline: true
            },
            token::Token {
                class: token::TokenClass::Punctuation(token::PunctuationClass::Empty),
                node: child.clone(), node_addr: 0x32.into(), depth: 1, newline: true
            },
            /* root */
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x4a, 0x50)),
                node: root.clone(), node_addr: 0.into(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x50, 0x60)),
                node: root.clone(), node_addr: 0.into(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x60, 0x70)),
                node: root.clone(), node_addr: 0.into(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Punctuation(token::PunctuationClass::Empty),
                node: root.clone(), node_addr: 0.into(), depth: 0, newline: true
            },
        ];

        let testcase = xml::Testcase {
            structure: root,
            expected_tokens,
        };

        test_forward(&testcase);
        test_backward(&testcase);
    }
}
