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

use tracing::instrument;

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

#[derive(Clone, PartialEq, Eq, Debug)]
enum TokenizerDescent {
    Child(usize),
    ChildSummary(usize),
    MySummary,
}

#[derive(Clone)]
pub struct TokenizerStackEntry {
    stack: Option<sync::Arc<TokenizerStackEntry>>,
    descent: TokenizerDescent,
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
    pub node: sync::Arc<structure::Node>,
    node_addr: addr::Address,
}

#[derive(Debug)]
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
            .field("descent", &self.descent)
            .field("node", &self.node.props.name)
            .finish_non_exhaustive()
    }
}

#[derive(Clone, Debug, Default)]
pub struct PortOptions {
    additional_offset: addr::Size,
    prefer_after_new_node: bool,
}

pub struct PortOptionsBuilder {
    options: PortOptions,
}

#[derive(Debug)]
struct PortStackState {
    current_path: structure::Path,
    new_stack: Option<sync::Arc<TokenizerStackEntry>>,
    force_summary: bool,
    depth: usize,
    node_addr: addr::Address,
    node: sync::Arc<structure::Node>,
}

enum SummaryPortMapping {
    Beginning,
    Child(usize),
    Content(addr::Address),
    End,
}

enum IntermediatePortState {
    Finished(TokenizerState),
    SummaryFixup(TokenizerState, SummaryPortMapping),
    
    Content(addr::Address, usize),
    Child(usize),
}

impl PortOptionsBuilder {
    pub fn new() -> PortOptionsBuilder {
        PortOptionsBuilder {
            options: PortOptions::default()
        }
    }

    pub fn additional_offset(mut self, offset: addr::Size) -> PortOptionsBuilder {
        self.options.additional_offset = offset;
        self
    }

    pub fn prefer_after_new_node(mut self) -> PortOptionsBuilder {
        self.options.prefer_after_new_node = true;
        self
    }
    
    pub fn build(self) -> PortOptions {
        self.options
    }
}

impl Default for PortOptionsBuilder {
    fn default() -> PortOptionsBuilder {
        Self::new()
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

    /// Applies all changes between old_doc and new_doc to the tokenizer state.
    #[instrument]
    pub fn port_doc(&mut self, old_doc: &document::Document, new_doc: &document::Document, options: &PortOptions) {
        if old_doc.is_outdated(new_doc) {
            match &new_doc.previous {
                Some((prev_doc, change)) => {
                    self.port_doc(old_doc, prev_doc, options);
                    self.port_change(&new_doc.root, change, options);
                },
                None => panic!("no common ancestor")
            }
        }
    }
    
    /// Applies a single change to the tokenizer state.
    #[instrument]
    fn port_change(&mut self, new_root: &sync::Arc<structure::Node>, change: &change::Change, options: &PortOptions) {
        /* Update all of the stack entries */
        let (force_summary, stack_state) = match &self.stack {
            Some(parent) => Self::port_recurse(parent, new_root, change),
            None => (false, PortStackState::new(new_root.clone()))
        };
        
        /* Take a first whack at figuring out vaguely where we should be. */
        let intermediate_state = match &self.state {
            TokenizerState::PreBlank =>
                IntermediatePortState::Finished(TokenizerState::PreBlank),
            TokenizerState::Title =>
                IntermediatePortState::Finished(TokenizerState::Title),
            
            TokenizerState::MetaContent(offset, index) =>
                IntermediatePortState::Content(*offset, *index),
            TokenizerState::Hexdump(extent, index) =>
                IntermediatePortState::Content(extent.begin, *index),
            TokenizerState::Hexstring(extent, index) =>
                IntermediatePortState::Content(extent.begin, *index),

            TokenizerState::SummaryOpener =>
                IntermediatePortState::SummaryFixup(TokenizerState::SummaryOpener, SummaryPortMapping::Beginning),
            TokenizerState::SummaryLabel(i) =>
                IntermediatePortState::SummaryFixup(TokenizerState::SummaryLabel(*i), SummaryPortMapping::Child(*i)),
            TokenizerState::SummarySeparator(i) =>
                IntermediatePortState::SummaryFixup(TokenizerState::SummarySeparator(*i), SummaryPortMapping::Child(*i)),
            TokenizerState::SummaryCloser =>
                IntermediatePortState::SummaryFixup(TokenizerState::SummaryCloser, SummaryPortMapping::End),
            TokenizerState::SummaryNewline =>
                IntermediatePortState::SummaryFixup(TokenizerState::SummaryNewline, SummaryPortMapping::End),
            TokenizerState::SummaryValueBegin =>
                IntermediatePortState::SummaryFixup(TokenizerState::SummaryValueBegin, SummaryPortMapping::Beginning),
            TokenizerState::SummaryLeaf =>
                IntermediatePortState::SummaryFixup(TokenizerState::SummaryLeaf, SummaryPortMapping::Content(addr::unit::NULL)),
            TokenizerState::SummaryValueEnd =>
                IntermediatePortState::SummaryFixup(TokenizerState::SummaryValueEnd, SummaryPortMapping::End),

            TokenizerState::PostBlank =>
                IntermediatePortState::Finished(TokenizerState::PostBlank),
            TokenizerState::End =>
                IntermediatePortState::Finished(TokenizerState::End),
        };

        /* Check summary states. If we used to be part of a summary and we're not anymore, we have some cleanup to do. Also move the stack onto the heap here if we're not popping it. If we are popping it, it's already on the heap. */
        let (intermediate_state, new_stack) = match intermediate_state {
            IntermediatePortState::SummaryFixup(suggested_state, _) if force_summary =>
                (IntermediatePortState::Finished(suggested_state), stack_state.new_stack),
            IntermediatePortState::SummaryFixup(_, mapping) if !force_summary => {
                /* Need to pop away the MySummary descent if it exists. */
                let unwrapped = stack_state.new_stack.expect("was in summary state without a parent");

                (match mapping {
                    SummaryPortMapping::Beginning => IntermediatePortState::Finished(TokenizerState::Title),
                    SummaryPortMapping::Content(addr) => IntermediatePortState::Content(addr, 0), // leaf has no children
                    SummaryPortMapping::Child(i) => IntermediatePortState::Child(i),
                    SummaryPortMapping::End => IntermediatePortState::Finished(TokenizerState::End),
                }, match unwrapped.descent {
                    TokenizerDescent::MySummary => {
                        unwrapped.stack.clone()
                    },
                    _ => Some(unwrapped),
                })
            },
            other => (other, stack_state.new_stack),
        };

        /* Update most of our fields. */
        *self = Tokenizer {
            stack: new_stack,
            state: TokenizerState::End, /* this is a placeholder while we finalize details. */
            depth: stack_state.depth,
            node: stack_state.node,
            node_addr: stack_state.node_addr,
        };
        
        /* If intermediate state if Finished, go ahead and finish up. Otherwise extract our remaining hints. */
        let (mut offset, mut index) = match intermediate_state {
            IntermediatePortState::Finished(finalized_state) => {
                self.state = finalized_state;
                return;
            },
            IntermediatePortState::SummaryFixup(_, _) => panic!("summary fixups should've been handled already!"),
            
            IntermediatePortState::Content(offset, index) => (offset + options.additional_offset, index),
            IntermediatePortState::Child(index) => (addr::unit::NULL + options.additional_offset, index),
        };
        
        /* Ugh, we were somewhere in the middle. Check if InsertNode affects us. */
        match &change.ty {
            change::ChangeType::AlterNode(_path, _new_props) => {},
            change::ChangeType::InsertNode(affected_path, affected_index, new_node_offset, new_node) if affected_path == &stack_state.current_path => {
                /* A new child was added to the node we're on. */
                if index == *affected_index && options.prefer_after_new_node {
                    /* options said we should place after the new node, so do so. */
                    index+= 1;
                    offset = *new_node_offset + new_node.size;
                } else if addr::Extent::sized(*new_node_offset, new_node.size).includes(offset) {
                    /* if new node contains our offset, we need to descend into it. The state here is, once again, a placeholder. */
                    self.descend(if force_summary { TokenizerDescent::ChildSummary(*affected_index) } else { TokenizerDescent::Child(*affected_index) }, TokenizerState::End);

                    index = 0;
                    offset-= new_node_offset.to_size();
                } else if index >= *affected_index {
                    /* If the new node was inserted before the child we were on, need to bump our child index unless we already descended into the inserted child. */
                    index+= 1;
                }
            },
            change::ChangeType::Nest(parent, first_child, last_child, _props) if parent == &stack_state.current_path => {
                /* Children were nested on this node */
                let new_nest = &self.node.children[*first_child];
                
                if (*first_child..=*last_child).contains(&index) && options.prefer_after_new_node {
                    /* options said we should place after the new node, so do so. */
                    index = first_child + 1;
                    offset = new_nest.offset + new_nest.node.size;
                } else if addr::Extent::sized(new_nest.offset, new_nest.node.size).includes(offset) {
                    /* if newly nested node contains our offset, we need to descend into it. The state here is, once again, a placeholder. */
                    index = 0;
                    offset-= new_nest.offset.to_size();
                    self.descend(if force_summary { TokenizerDescent::ChildSummary(*first_child) } else { TokenizerDescent::Child(*first_child) }, TokenizerState::End);
                } else if index > *last_child {
                    /* If the new node was nested before the child we were on, need to adjust our child index */
                    index-= last_child-first_child;
                } else if index <= *first_child {
                    /* ok */
                } else {
                    todo!("what is going on here?");
                }
            },
            change::ChangeType::Nest(_, _, _, _) => {},
            change::ChangeType::InsertNode(_, _, _, _) => {},
        };

        self.seek_in_node(offset, index);
    }

    /* Used to recurse to the base of the tokenizer stack so we can start porting from the top down. Returns whether or not to keep going.*/
    #[instrument]
    fn port_recurse(tok: &TokenizerStackEntry, new_root: &sync::Arc<structure::Node>, change: &change::Change) -> (bool, PortStackState) {
        match &tok.stack {
            Some(parent) => {
                let (should_continue, mut state) = Self::port_recurse(parent, new_root, change);
                (should_continue && Self::port_stack_entry(&mut state, tok, change), state)
            },
            None => {
                /* reached root */
                let mut state = PortStackState::new(new_root.clone());
                (Self::port_stack_entry(&mut state, tok, change), state)
            }
        }
    }

    /// Applies a change to a single item in the tokenizer stack. Returns whether or not to keep going.
    #[instrument]
    fn port_stack_entry(state: &mut PortStackState, old_tok: &TokenizerStackEntry, change: &change::Change) -> bool {
        let (descent, summary) = match old_tok.descent {
            TokenizerDescent::Child(mut old_child_index) | TokenizerDescent::ChildSummary(mut old_child_index) => {
                match &change.ty {
                     change::ChangeType::AlterNode(_, _) => match state.node.props.children_display {
                        structure::ChildrenDisplay::None => return false,
                        _ if state.force_summary => (TokenizerDescent::ChildSummary(old_child_index), true),
                        structure::ChildrenDisplay::Summary => (TokenizerDescent::ChildSummary(old_child_index), true),
                        structure::ChildrenDisplay::Full => (TokenizerDescent::Child(old_child_index), false)
                    },
                    change::ChangeType::InsertNode(path, after_child, _offset, _node) => {
                        if path == &state.current_path {
                            if old_child_index >= *after_child {
                                old_child_index+= 1;
                            }
                        }

                        match state.node.props.children_display {
                            structure::ChildrenDisplay::None => return false,
                            _ if state.force_summary => (TokenizerDescent::ChildSummary(old_child_index), true),
                            structure::ChildrenDisplay::Summary => (TokenizerDescent::ChildSummary(old_child_index), true),
                            structure::ChildrenDisplay::Full => (TokenizerDescent::Child(old_child_index), false)
                        }
                    },
                    change::ChangeType::Nest(parent, first_child, last_child, props) => {
                        if parent == &state.current_path && (*first_child..=*last_child).contains(&old_child_index) {
                            let (descent_into_new_node, summary) = match props.children_display {
                                structure::ChildrenDisplay::None => return false,
                                _ if state.force_summary => (TokenizerDescent::ChildSummary(*first_child), true),
                                structure::ChildrenDisplay::Summary => (TokenizerDescent::ChildSummary(*first_child), true),
                                structure::ChildrenDisplay::Full => (TokenizerDescent::Child(*first_child), false)
                            };

                            descent_into_new_node.build_path(&mut state.current_path);

                            let childhood = descent_into_new_node.childhood(&state.node);
                            
                            let tse = TokenizerStackEntry {
                                stack: state.new_stack.take(),
                                descent: descent_into_new_node,
                                depth: state.depth,
                                node: std::mem::replace(&mut state.node, childhood.node),
                                node_addr: state.node_addr,
                            };
                            state.depth+= tse.descent.depth_change();
                            state.node_addr+= childhood.offset.to_size();
                            state.new_stack = Some(sync::Arc::new(tse));
                            state.force_summary = state.force_summary || summary;

                            old_child_index-= first_child;
                        }

                        match state.node.props.children_display {
                            structure::ChildrenDisplay::None => return false,
                            _ if state.force_summary => (TokenizerDescent::ChildSummary(old_child_index), true),
                            structure::ChildrenDisplay::Summary => (TokenizerDescent::ChildSummary(old_child_index), true),
                            structure::ChildrenDisplay::Full => (TokenizerDescent::Child(old_child_index), false)
                        }
                    },
                }
            },
            TokenizerDescent::MySummary => (TokenizerDescent::MySummary, true)
        };

        descent.build_path(&mut state.current_path);

        let childhood = descent.childhood(&state.node);
        
        let tse = TokenizerStackEntry {
            stack: state.new_stack.take(),
            descent,
            depth: state.depth,
            node: std::mem::replace(&mut state.node, childhood.node),
            node_addr: state.node_addr,
        };

        state.depth+= tse.descent.depth_change();
        state.node_addr+= childhood.offset.to_size();
        state.new_stack = Some(sync::Arc::new(tse));

        state.force_summary = state.force_summary || summary;
        
        true
    }

    fn seek_in_node(&mut self, offset: addr::Address, index: usize) {
        self.state = TokenizerState::MetaContent(self.get_line_begin(offset, index), index);

        while match self.gen_token() {
            TokenGenerationResult::Skip => true,
            _ => false
        } {
            self.move_next();
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

    fn get_line_begin(&self, offset: addr::Address, index: usize) -> addr::Address {
        /* Where would we *like* to begin, as decided by our content's preferred pitch? */
        let preferred_begin = self.node.props.content_display.preferred_pitch().map(|pitch| {
            (pitch * ((offset - addr::unit::BIT).to_size() / pitch)).to_addr()
        });

        /* Figure out whether there are any children that we need to not intrude upon. */
        let prev_child_option = match index {
            0 => None,
            /* Something is seriously wrong if index was farther than one-past-the-end. */
            i => Some((i-1, &self.node.children[i-1]))
        };

        /* Where can we not begin before? */
        let limit = match prev_child_option {
            /* Can't include data from the child, so need to stop after its end. */
            Some((_, prev_child)) => prev_child.end(),
            /* Can't include data that belongs to the parent, so need to stop before our begin. */
            None => addr::unit::NULL,
        };
        
        /* Pick a place to begin this line. */
        preferred_begin.map_or(limit, |pb| std::cmp::max(pb, limit))
    }

    fn get_line_end(&self, offset: addr::Address, index: usize) -> addr::Address {
        /* Where would we *like* to end, as decided by our content's preferred pitch? */
        let preferred_end = self.node.props.content_display.preferred_pitch().map(|pitch| {
            (pitch * ((offset.to_size() / pitch) + 1)).to_addr()
        });

        /* Figure out whether there are any children that we need to not intrude upon. */
        let next_child_option = self.node.children.get(index).map(|child| (index, child));

        /* Where can we not end beyond? */
        let limit = match next_child_option {
            /* Can't include data from the child, so need to stop before it begins. */
            Some((_, next_child)) => next_child.offset,
            /* Can't include data that belongs to the parent, so need to stop before we end. */
            None => self.node.size.to_addr(),
        };

        /* Pick a place to end this line. */
        preferred_end.map_or(limit, |pe| std::cmp::min(pe, limit))
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
                            TokenizerDescent::Child(prev_child_index),
                            /* Descend to thse end of the child. */
                            TokenizerState::End);

                        return true;
                    }
                }

                /* Emit content, if we can. */
                if offset > addr::unit::NULL {
                    let extent = addr::Extent::between(self.get_line_begin(offset, index), offset);
                        
                    self.state = match self.node.props.content_display {
                        structure::ContentDisplay::None => TokenizerState::MetaContent(extent.begin, index),
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
                    TokenizerDescent::ChildSummary(i),
                    TokenizerState::SummaryValueEnd);
                true
            },
            TokenizerState::SummaryCloser => {
                if self.node.children.is_empty() {
                    self.state = TokenizerState::SummaryOpener;
                } else {
                    self.state = TokenizerState::SummarySeparator(self.node.children.len()-1);
                }
                true
            },
            TokenizerState::SummaryNewline => {
                self.descend(
                    TokenizerDescent::MySummary,
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
                if self.node.children.is_empty() {
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
                        self.descend(
                            TokenizerDescent::MySummary,
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
                            TokenizerDescent::Child(next_child_index),
                            /* Descend to the beginning of the child. */
                            TokenizerState::PreBlank);

                        return true;
                    }
                }

                /* Emit content, if we can. */
                if offset < self.node.size.to_addr() {
                    let extent = addr::Extent::between(offset, self.get_line_end(offset, index));

                    self.state = match self.node.props.content_display {
                        structure::ContentDisplay::None => TokenizerState::MetaContent(extent.end, index),
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
                if self.node.children.is_empty() {
                    self.state = TokenizerState::SummaryCloser;
                } else {
                    self.state = TokenizerState::SummaryLabel(0);
                }
                true
            },
            TokenizerState::SummaryLabel(i) => {
                self.descend(
                    TokenizerDescent::ChildSummary(i),
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
                if self.node.children.is_empty() {
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
    /// * `descent` - The type of descent being performed.
    /// * `state_within` - Where within the child to descend to.
    ///
    fn descend(
        &mut self,
        descent: TokenizerDescent,
        state_within: TokenizerState) {
        let childhood = descent.childhood(&self.node);
        let parent_node = std::mem::replace(&mut self.node, childhood.node);
        let depth_change = descent.depth_change();
        
        let parent_entry = TokenizerStackEntry {
            stack: self.stack.take(),
            descent,
            depth: self.depth,
            node: parent_node,
            node_addr: self.node_addr,
        };

        self.depth+= depth_change;
        self.stack = Some(sync::Arc::new(parent_entry));
        self.state = state_within;
        self.node_addr+= childhood.offset.to_size();
    }
    
    /// Replaces our context with the parent's context, returning false if there
    /// was no parent.
    fn try_ascend(&mut self, dir: AscendDirection) -> bool {
        match std::mem::replace(&mut self.stack, None) {
            Some(stack_entry) => {
                // TODO: replace this with unwrap_or_clone when it gets stabilized
                //       https://github.com/rust-lang/rust/issues/93610
                let stack_entry = sync::Arc::try_unwrap(stack_entry).unwrap_or_else(|arc| (*arc).clone());
                *self = Tokenizer {
                    state: match dir {
                        AscendDirection::Prev => stack_entry.descent.before_state(&stack_entry),
                        AscendDirection::Next => stack_entry.descent.after_state(&stack_entry)
                    },
                    stack: stack_entry.stack,
                    depth: stack_entry.depth,
                    node: stack_entry.node,
                    node_addr: stack_entry.node_addr,
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

impl TokenizerDescent {
    fn childhood(&self, node: &sync::Arc<structure::Node>) -> structure::Childhood {
        match self {
            TokenizerDescent::Child(i) => node.children[*i].clone(),
            TokenizerDescent::ChildSummary(i) => node.children[*i].clone(),
            TokenizerDescent::MySummary => structure::Childhood {
                node: node.clone(),
                offset: addr::unit::NULL,
            },
        }
    }

    fn depth_change(&self) -> usize {
        match self {
            TokenizerDescent::Child(_) | TokenizerDescent::ChildSummary(_) => 1,
            TokenizerDescent::MySummary => 0,
        }
    }

    fn before_state(&self, stack_entry: &TokenizerStackEntry) -> TokenizerState {
        match self {
            TokenizerDescent::Child(i) => TokenizerState::MetaContent(stack_entry.node.children[*i].offset, *i),
            TokenizerDescent::ChildSummary(i) => TokenizerState::SummaryLabel(*i),
            TokenizerDescent::MySummary => TokenizerState::Title,
        }
    }

    fn after_state(&self, stack_entry: &TokenizerStackEntry) -> TokenizerState {
        match self {
            TokenizerDescent::Child(i) => TokenizerState::MetaContent(stack_entry.node.children[*i].end(), *i+1),
            TokenizerDescent::ChildSummary(i) => TokenizerState::SummarySeparator(*i),
            TokenizerDescent::MySummary => TokenizerState::SummaryNewline,
        }
    }

    fn build_path(&self, path: &mut structure::Path) {
        match self {
            TokenizerDescent::Child(i) | TokenizerDescent::ChildSummary(i) => path.push(*i),
            TokenizerDescent::MySummary => {},
        }        
    }
}

impl TokenizerStackEntry {
    fn build_path(entry: &Option<sync::Arc<TokenizerStackEntry>>, path: &mut structure::Path) {
        if let Some(tse) = entry {
            Self::build_path(&tse.stack, path);
            tse.descent.build_path(path);
        }
    }
}

impl PortStackState {
    fn new(root: sync::Arc<structure::Node>) -> PortStackState {
         PortStackState {
             current_path: structure::Path::new(),
             new_stack: None,
             force_summary: false,
             depth: 0,
             node_addr: addr::unit::NULL,
             node: root,
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
                expected_tokens: expected_tokens.expect("should've had expected tokens").into_iter().map(|c| c.into_token(&lookup)).collect(),
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
        fn into_token(self, lookup: &collections::HashMap<String, (addr::Address, sync::Arc<structure::Node>)>) -> token::Token {
            let lookup_result = lookup.get(&self.node_name).unwrap_or_else(|| panic!("expected a node named '{}'", self.node_name));
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
mod tests {
    use super::*;

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
