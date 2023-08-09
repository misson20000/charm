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
use crate::model::document::structure;
use crate::model::document::change;

use tracing::instrument;

#[derive(Clone, Debug, PartialEq, Eq)]
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
enum PortStackMode {
    Normal,
    Summary,
    // TODO: ChildrenDisplaySetToNone,

    /* Even if nodes get deleted, we need to descend into their stack entries and sum up their offsets to figure out where in the parent node we should be. */
    Deleted {
        /* PortStackState::node represents the parent node that had its child deleted. This represents a child on the old hierarchy, used for building offset_within_parent. */
        node: sync::Arc<structure::Node>,
        first_deleted_child_index: usize,
        offset_within_parent: addr::Size,
        summary: bool,
    },

    /* We processed a node that got destructured, but didn't push it onto the stack. */
    Destructuring {
        destructured_childhood: structure::Childhood,
        destructured_child_index: usize,
        summary: bool,
    },
}

struct PortStackState {
    mode: PortStackMode,
    current_path: structure::Path,
    new_stack: Option<sync::Arc<TokenizerStackEntry>>,
    depth: usize,
    node_addr: addr::Address,
    node: sync::Arc<structure::Node>,
}

impl std::fmt::Debug for PortStackState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PortStackState")
            .field("mode", &self.mode)
            .field("current_path", &self.current_path)
            .field("new_stack", &TokenizerStackDebugHelper(&self.new_stack))
            .field("depth", &self.depth)
            .field("node_addr", &self.node_addr)
            .field("node", &self.node.props.name)
            .finish_non_exhaustive()
    }
}

#[derive(Debug)]
enum SummaryPortMapping {
    Beginning,
    Child(usize),
    Content(addr::Address),
    End,
}

#[derive(Debug)]
enum IntermediatePortState {
    Finished(TokenizerState),

    NormalContent(Option<addr::Address>, usize),
    SummaryLabel(usize),
    SummarySeparator(usize),
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

    /// Creates a new tokenizer positioned at a specific offset within the node at the given path.
    pub fn at_path(root: sync::Arc<structure::Node>, path: &structure::Path, offset: addr::Address) -> Tokenizer {
        let mut node = &root;
        let mut node_addr = addr::unit::NULL;
        let mut depth = 0;
        let mut stack = None;
        let mut summary_prev = false;
        let mut summary_next = match root.props.children_display {
            structure::ChildrenDisplay::Summary => true,
            _ => false
        };
                
        for child_index in path {
            if !summary_prev && summary_next {
                stack = Some(sync::Arc::new(TokenizerStackEntry {
                    stack: stack.take(),
                    descent: TokenizerDescent::MySummary,
                    depth,
                    node: node.clone(),
                    node_addr,
                }));
            }

            summary_prev = summary_next;

            stack = Some(sync::Arc::new(TokenizerStackEntry {
                stack: stack.take(),
                descent: if summary_prev { TokenizerDescent::ChildSummary(*child_index) } else { TokenizerDescent::Child(*child_index) },
                depth,
                node: node.clone(),
                node_addr,
            }));

            let childhood = &node.children[*child_index];
            node = &childhood.node;
            node_addr+= childhood.offset.to_size();
            depth+= 1;

            summary_next = summary_next || match node.props.children_display {
                structure::ChildrenDisplay::Summary => true,
                _ => false
            };
        }
        
        let mut tokenizer = Tokenizer {
            stack,
            state: TokenizerState::PreBlank,
            depth,
            node: node.clone(),
            node_addr
        };

        tokenizer.seek_in_node_to_offset(offset);
        
        tokenizer
    }
    
    /// Applies a single change to the tokenizer state.
    #[instrument]
    pub fn port_change(&mut self, new_root: &sync::Arc<structure::Node>, change: &change::Change, options: &PortOptions) {
        /* Recreate our stack, processing descents and such, leaving off with some information about the node we're actually on now. */
        let stack_state = match &self.stack {
            Some(parent) => Self::port_recurse(parent, new_root, change),
            None => PortStackState::new(new_root.clone())
        };

        /* Convert our old state into an intermediate state that allows us to represent that we might not know the offset yet, or might not care about figuring it out. */
        let mut intermediate_state = match &stack_state.mode {
            /* We were in a child that got deleted. Our old state tells us about where we were in the child. */
            PortStackMode::Deleted { node: _, first_deleted_child_index, offset_within_parent, summary: false } => {
                let offset_within_child = match &self.state {
                    TokenizerState::MetaContent(offset, _) => *offset,
                    TokenizerState::Hexdump(extent, _) => extent.begin,
                    TokenizerState::Hexstring(extent, _) => extent.begin,
                    _ => addr::unit::NULL
                };
                
                IntermediatePortState::NormalContent(Some(offset_within_child + *offset_within_parent), *first_deleted_child_index)
            },
            PortStackMode::Deleted { node: _, first_deleted_child_index, offset_within_parent: _,  summary: true  } => IntermediatePortState::SummaryLabel(*first_deleted_child_index),

            /* We were in a child that got destructured. Our old state tells us about where we were in that child. */
            PortStackMode::Destructuring { destructured_childhood, destructured_child_index, summary: false } => match &self.state {
                TokenizerState::PreBlank | TokenizerState::Title | TokenizerState::SummaryOpener | TokenizerState::SummaryValueBegin => IntermediatePortState::NormalContent(Some(destructured_childhood.offset), *destructured_child_index),
                
                TokenizerState::MetaContent(offset, index) => IntermediatePortState::NormalContent(Some(destructured_childhood.offset + offset.to_size()), destructured_child_index + *index),
                TokenizerState::Hexdump(extent, index) => IntermediatePortState::NormalContent(Some(destructured_childhood.offset + extent.begin.to_size()), destructured_child_index + *index),
                TokenizerState::Hexstring(extent, index) => IntermediatePortState::NormalContent(Some(destructured_childhood.offset + extent.begin.to_size()), destructured_child_index + *index),
                TokenizerState::SummaryLeaf => IntermediatePortState::NormalContent(Some(destructured_childhood.offset), *destructured_child_index),

                TokenizerState::SummaryLabel(i) | TokenizerState::SummarySeparator(i) => IntermediatePortState::NormalContent(None, destructured_child_index + *i),

                TokenizerState::SummaryValueEnd | TokenizerState::SummaryNewline | TokenizerState::SummaryCloser | TokenizerState::PostBlank | TokenizerState::End => IntermediatePortState::NormalContent(Some(destructured_childhood.end()), destructured_child_index + 1),
            },
            // TODO: try harder here
            PortStackMode::Destructuring { destructured_child_index, summary: true, .. } => IntermediatePortState::SummaryLabel(*destructured_child_index),
            
            PortStackMode::Normal => match &self.state {
                TokenizerState::PreBlank =>
                    IntermediatePortState::Finished(TokenizerState::PreBlank),
                TokenizerState::Title =>
                    IntermediatePortState::Finished(TokenizerState::Title),
                
                TokenizerState::MetaContent(offset, index) => IntermediatePortState::NormalContent(Some(*offset), *index),
                TokenizerState::Hexdump(extent, index) => IntermediatePortState::NormalContent(Some(extent.begin), *index),
                TokenizerState::Hexstring(extent, index) => IntermediatePortState::NormalContent(Some(extent.begin), *index),

                TokenizerState::SummaryOpener => IntermediatePortState::NormalContent(Some(addr::unit::NULL), 0),
                TokenizerState::SummaryLabel(i) => IntermediatePortState::NormalContent(None, *i),
                TokenizerState::SummarySeparator(i) => IntermediatePortState::NormalContent(None, *i),
                TokenizerState::SummaryCloser => IntermediatePortState::Finished(TokenizerState::End),
                TokenizerState::SummaryNewline => IntermediatePortState::Finished(TokenizerState::PostBlank),
                TokenizerState::SummaryValueBegin => IntermediatePortState::Finished(TokenizerState::Title),
                TokenizerState::SummaryLeaf => IntermediatePortState::NormalContent(Some(addr::unit::NULL), 0),
                TokenizerState::SummaryValueEnd => IntermediatePortState::Finished(TokenizerState::End),

                TokenizerState::PostBlank => IntermediatePortState::Finished(TokenizerState::PostBlank),
                TokenizerState::End => IntermediatePortState::Finished(TokenizerState::End),
            },

            PortStackMode::Summary => match &self.state {
                TokenizerState::PreBlank =>
                    IntermediatePortState::Finished(TokenizerState::PreBlank),
                TokenizerState::Title =>
                    IntermediatePortState::Finished(TokenizerState::Title),
                
                TokenizerState::MetaContent(_, index) => IntermediatePortState::SummaryLabel(*index),
                TokenizerState::Hexdump(_, index) => IntermediatePortState::SummaryLabel(*index),
                TokenizerState::Hexstring(_, index) => IntermediatePortState::SummaryLabel(*index),

                TokenizerState::SummaryOpener => IntermediatePortState::Finished(TokenizerState::SummaryOpener),
                TokenizerState::SummaryLabel(i) => IntermediatePortState::SummaryLabel(*i),
                TokenizerState::SummarySeparator(i) => IntermediatePortState::SummarySeparator(*i),
                TokenizerState::SummaryCloser => IntermediatePortState::Finished(TokenizerState::SummaryCloser),
                TokenizerState::SummaryNewline => IntermediatePortState::Finished(TokenizerState::SummaryNewline),
                TokenizerState::SummaryValueBegin => IntermediatePortState::Finished(TokenizerState::SummaryValueBegin),
                TokenizerState::SummaryLeaf => IntermediatePortState::Finished(TokenizerState::SummaryLeaf),
                TokenizerState::SummaryValueEnd => IntermediatePortState::Finished(TokenizerState::SummaryValueEnd),

                TokenizerState::PostBlank => IntermediatePortState::Finished(TokenizerState::SummaryNewline),
                TokenizerState::End => IntermediatePortState::Finished(TokenizerState::SummaryCloser),
            }
        };

        let is_summary = match &stack_state.mode {
            PortStackMode::Normal => false,
            PortStackMode::Summary => true,
            PortStackMode::Deleted { summary, .. } => *summary,
            PortStackMode::Destructuring { summary, .. } => *summary,
        };
        
        *self = Tokenizer {
            stack: stack_state.new_stack,
            state: TokenizerState::End, /* this is a placeholder. we finalize the details later... */
            depth: stack_state.depth,
            node: stack_state.node,
            node_addr: stack_state.node_addr,
        };
        
        /* If intermediate state is Finished, go ahead and finish up. Otherwise, get references to the (offset, index) that we need to adjust. */
        let mut dont_care_offset = None;
        let (offset, index) = match intermediate_state {
            IntermediatePortState::Finished(finalized_state) => {
                self.state = finalized_state;
                return;
            },
            
            IntermediatePortState::NormalContent(ref mut offset, ref mut index) => (offset, index),
            IntermediatePortState::SummaryLabel(ref mut index) => (&mut dont_care_offset, index),
            IntermediatePortState::SummarySeparator(ref mut index) => (&mut dont_care_offset, index),
        };

        /* Respect the additional offset that the options requested. */
        if let Some(offset) = offset.as_mut() {
            *offset = *offset + options.additional_offset;
        }
        
        /* Adjust the offset and index. */
        match &change.ty {
            change::ChangeType::AlterNode { .. } => {},
            
            change::ChangeType::InsertNode { parent: affected_path, index: affected_index, child: new_childhood } if affected_path == &stack_state.current_path => {
                /* A new child was added to the node we're on. */
                if *index == *affected_index && options.prefer_after_new_node {
                    /* options said we should place after the new node, so do so. */
                    *index+= 1;
                    *offset = Some(new_childhood.end());
                } else if let Some(offset) = offset.as_mut() {
                    if new_childhood.extent().includes(*offset) {
                        /* if new node contains our offset, we need to descend into it. The state here is, once again, a placeholder. */
                        self.descend(if is_summary { TokenizerDescent::ChildSummary(*affected_index) } else { TokenizerDescent::Child(*affected_index) }, TokenizerState::End);

                        *index = 0;
                        *offset-= new_childhood.offset.to_size();
                    } else if *index >= *affected_index {
                        *index+= 1;
                    }
                } else if *index >= *affected_index {
                    /* If the new node was inserted before the child we were on, need to bump our child index unless we already descended into the inserted child. */
                    *index+= 1;
                }
            },
            
            change::ChangeType::Nest { parent, first_child, last_child, extent, props: _ } if parent == &stack_state.current_path => {
                /* Children were nested on this node */
                let new_nest = &self.node.children[*first_child];
                
                if (*first_child..=*last_child).contains(index) || offset.map_or(false, |o| extent.includes(o)) {
                    if options.prefer_after_new_node {
                        /* options said we should place after the new node, so do so. */
                        *index = first_child + 1;
                        *offset = Some(new_nest.end());
                    } else {
                        /* descend into the new node. */
                        let new_nest_offset = new_nest.offset;
                        
                        self.descend(if is_summary { TokenizerDescent::ChildSummary(*first_child) } else { TokenizerDescent::Child(*first_child) }, TokenizerState::End);

                        // TODO: is there something more helpful we could do here?
                        *index = 0;
                        
                        if let Some(offset) = offset.as_mut() {
                            *offset-= new_nest_offset.to_size();
                        }
                    }
                } else if *index > *last_child {
                    /* If the new node was nested before the child we were on, need to adjust our child index */
                    *index-= last_child-first_child;
                }
            },

            change::ChangeType::Destructure { parent, .. } if parent == &stack_state.current_path => {
                /* Handled by PortStackMode::Destructuring, so we don't have to deal with it here. */
            },

            /* If the node we were on (or an ancestor of it) were deleted, that was already handled by port_recurse. Here we're only worried about our direct children (that we're not positioned on) being deleted. */
            change::ChangeType::DeleteRange { parent, first_child, last_child } if parent == &stack_state.current_path => {
                if (*first_child..=*last_child).contains(index) {
                    *index = *first_child;
                } else if *index > *last_child {
                    *index-= last_child-first_child+1;
                }
            },
            
            /* Other cases where the node we were on wasn't affected and our hints don't need adjustment. */
            change::ChangeType::Nest { .. } => {},
            change::ChangeType::Destructure { .. } => {},
            change::ChangeType::InsertNode { .. } => {},
            change::ChangeType::DeleteRange { .. } => {},
        };

        /* Now that we've adjusted offset and size, we can convert the intermediate state to actual state. */
        let children = &self.node.children;
        self.state = match intermediate_state {
            /* This should've been handled earlier, but whatever. */
            IntermediatePortState::Finished(finalized_state) => finalized_state,

            IntermediatePortState::NormalContent(offset, index) => TokenizerState::MetaContent(self.get_line_begin(offset.unwrap_or_else(|| children[index].offset), index), index),

            /* Real TokenizerState doesn't support one-past-the-end for SummaryLabel and SummarySeparator, so need to fix if that would be the case. */
            IntermediatePortState::SummaryLabel(index) if index < children.len() => TokenizerState::SummaryLabel(index),
            IntermediatePortState::SummarySeparator(index) if index < children.len() => TokenizerState::SummarySeparator(index),
            IntermediatePortState::SummaryLabel(_) => TokenizerState::SummaryCloser,
            IntermediatePortState::SummarySeparator(_) => TokenizerState::SummaryCloser,
        };

        /* Adjust our stream position to actually be on a token. */
        while match self.gen_token() {
            TokenGenerationResult::Skip => true,
            _ => false
        } {
            self.move_next();
        }
    }

    /// Used to recurse to the base of the tokenizer stack so we can start porting from the top down. Returns whether or not to keep going.
    #[instrument]
    fn port_recurse(tok: &TokenizerStackEntry, new_root: &sync::Arc<structure::Node>, change: &change::Change) -> PortStackState {
        match &tok.stack {
            Some(parent) => {
                let mut state = Self::port_recurse(parent, new_root, change);
                Self::port_stack_entry(&mut state, tok, change);
                state
            },
            None => {
                /* reached root */
                let mut state = PortStackState::new(new_root.clone());
                Self::port_stack_entry(&mut state, tok, change);
                state
            }
        }
    }

    /// Applies a change to a single item in the tokenizer stack. If we need to stop descending in the middle of the stack, return the 
    #[instrument]
    fn port_stack_entry(state: &mut PortStackState, old_tok: &TokenizerStackEntry, change: &change::Change) {
        /* This logic more-or-less mirrors change::update_path */
        let child_index = match old_tok.descent {
            TokenizerDescent::Child(child_index) | TokenizerDescent::ChildSummary(child_index) => child_index,
            
            /* This is handled implicitly by PortStackState::push behavior. */
            TokenizerDescent::MySummary => return,
        };
        
        match &change.ty {
            change::ChangeType::AlterNode { .. } => state.push(child_index),
            change::ChangeType::InsertNode { parent: path, index: after_child, child: _ } => {
                if path == &state.current_path && child_index >= *after_child {
                    state.push(child_index + 1);
                } else {
                    state.push(child_index);
                }
            },
            change::ChangeType::Nest { parent, first_child, last_child, extent: _, props: _ } => {
                if parent == &state.current_path {
                    if (*first_child..=*last_child).contains(&child_index) {
                        state.push(*first_child);
                        state.push(child_index - first_child);
                    } else if child_index > *last_child {
                        state.push(child_index - (last_child-first_child));
                    }
                } else {
                    state.push(child_index);
                }
            },
            change::ChangeType::Destructure { parent, child_index: destructured_child, num_grandchildren, offset } => {
                if parent == &state.current_path {
                    state.destructured(*destructured_child, child_index, *num_grandchildren, *offset, &old_tok.node.children[*destructured_child]);
                } else {
                    state.push(child_index);
                }
            },
            change::ChangeType::DeleteRange { parent, first_child, last_child } => {
                if parent == &state.current_path {
                    if (*first_child..=*last_child).contains(&child_index) {
                        state.deleted(*first_child, child_index, &old_tok.node);
                    } else if child_index > *last_child {
                        state.push(child_index - (last_child-first_child+1));
                    }
                } else {
                    state.push(child_index);
                }
            },
        }
    }

    fn seek_in_node_to_offset(&mut self, offset: addr::Address) {
        let index = self.node.children.partition_point(|ch| ch.offset < offset);
        
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

    /// Returns the token at the current position, or Skip if the current position in the stream doesn't generate a token.
    pub fn gen_token(&self) -> TokenGenerationResult {
        match self.state {
            TokenizerState::PreBlank => if self.node.props.title_display.has_blanks() {
                TokenGenerationResult::Ok(token::Token {
                    class: token::TokenClass::Punctuation(token::PunctuationClass::Empty),
                    node: self.node.clone(),
                    node_path: self.structure_path(),
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
                node_path: self.structure_path(),
                node_addr: self.node_addr,
                depth: self.depth,
                newline: !self.node.props.title_display.is_inline(),
            }),
            
            TokenizerState::MetaContent(_, _) => TokenGenerationResult::Skip,
            TokenizerState::Hexdump(extent, _) => TokenGenerationResult::Ok(token::Token {
                class: token::TokenClass::Hexdump(extent),
                node: self.node.clone(),
                node_path: self.structure_path(),
                node_addr: self.node_addr,
                depth: self.depth + 1,
                newline: true,
            }),
            TokenizerState::Hexstring(extent, _) => TokenGenerationResult::Ok(token::Token {
                class: token::TokenClass::Hexstring(extent),
                node: self.node.clone(),
                node_path: self.structure_path(),
                node_addr: self.node_addr,
                depth: self.depth + 1,
                newline: true,
            }),
            
            TokenizerState::SummaryOpener => TokenGenerationResult::Ok(token::Token {
                class: token::TokenClass::Punctuation(token::PunctuationClass::OpenBracket),
                node: self.node.clone(),
                node_path: self.structure_path(),
                node_addr: self.node_addr,
                depth: self.depth,
                newline: false,
            }),
            TokenizerState::SummaryLabel(i) => {
                let ch = &self.node.children[i];
                TokenGenerationResult::Ok(token::Token {
                    class: token::TokenClass::SummaryLabel,
                    node: ch.node.clone(),
                    node_path: self.structure_path(),
                    node_addr: self.node_addr + ch.offset.to_size(),
                    depth: self.depth,
                    newline: false,
                })
            },
            TokenizerState::SummarySeparator(i) => if i+1 < self.node.children.len() {
                TokenGenerationResult::Ok(token::Token {  
                    class: token::TokenClass::Punctuation(token::PunctuationClass::Comma),
                    node: self.node.clone(),
                    node_path: self.structure_path(),
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
                node_path: self.structure_path(),
                node_addr: self.node_addr,
                depth: self.depth,
                newline: false,
            }),
            TokenizerState::SummaryNewline => TokenGenerationResult::Ok(token::Token {
                class: token::TokenClass::Punctuation(token::PunctuationClass::Empty),
                node: self.node.clone(),
                node_path: self.structure_path(),
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
                        structure::ContentDisplay::Hexdump { .. } => token::TokenClass::Hexdump(extent),
                        structure::ContentDisplay::Hexstring => token::TokenClass::Hexstring(extent),
                    },
                    node: self.node.clone(),
                    node_path: self.structure_path(),
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
                    node_path: self.structure_path(),
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
            (pitch * (offset.to_size() / pitch)).to_addr()
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
    
    /// Moves one position backwards in the stream.
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
                    let extent = addr::Extent::between(self.get_line_begin(offset - addr::unit::BIT, index), offset);
                        
                    self.state = match self.node.props.content_display {
                        structure::ContentDisplay::None => TokenizerState::MetaContent(extent.begin, index),
                        structure::ContentDisplay::Hexdump { .. } => TokenizerState::Hexdump(extent, index),
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

    /// Moves one position forwards in the stream.
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
                        structure::ContentDisplay::Hexdump { .. } => TokenizerState::Hexdump(extent, index),
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
            mode: PortStackMode::Normal,
            current_path: structure::Path::new(),
            new_stack: None,
            depth: 0,
            node_addr: addr::unit::NULL,
            node: root,
        }
    }

    fn deleted(&mut self, first_deleted_child_index: usize, child_of_interest: usize, old_node: &sync::Arc<structure::Node>) {
        match self.mode {
            PortStackMode::Normal | PortStackMode::Summary => {
                /* From here on out, we will leave the following alone:
                    - new_stack
                    - depth
                    - node_addr
                    - node
                 */
                
                let childhood = &old_node.children[child_of_interest];
                
                self.mode = PortStackMode::Deleted {
                    node: childhood.node.clone(),
                    first_deleted_child_index,
                    offset_within_parent: childhood.offset.to_size(),
                    summary: match self.mode {
                        PortStackMode::Normal => false,
                        PortStackMode::Summary => true,
                        _ => panic!("unreachable"),
                    },
                };
            },
            PortStackMode::Deleted { .. } => {
                /* Something got deleted, but we were already processing stack entries for deleted nodes anyway. */
            },
            PortStackMode::Destructuring { .. } => {
                panic!("deletion and destructure shouldn't happen in the same change ");
            },
        }
    }

    fn destructured(&mut self, destructured_child: usize, current_child: usize, num_grandchildren: usize, offset: addr::Address, childhood: &structure::Childhood) {
        match self.mode {
            /* Only enter destructuring mode if we're trying to descend into a child that got destructured. */
            PortStackMode::Normal | PortStackMode::Summary if current_child == destructured_child => {
                assert_eq!(offset, childhood.offset);
                
                self.mode = PortStackMode::Destructuring {
                    summary: match self.mode {
                        PortStackMode::Normal => false,
                        PortStackMode::Summary => true,
                        _ => false
                    },
                    destructured_child_index: destructured_child,
                    destructured_childhood: childhood.clone(),
                };
            },

            /* If we're not in destructuring mode and we're descending into a different child node, just fix the index and continue normally. */
            PortStackMode::Normal | PortStackMode::Summary => {
                if current_child > destructured_child {
                    self.push(current_child + num_grandchildren - 1);
                } else {
                    self.push(current_child);
                }
            },

            /* If this is the second time around we're entering the function,
             * it's because we're descending into one of the destructured node's
             * children and we can push the new index and continue normally. */
            PortStackMode::Destructuring { summary: true, .. } => {
                self.mode = PortStackMode::Summary;
                self.push(destructured_child + current_child);
            },
            PortStackMode::Destructuring { summary: false, .. } => {
                self.mode = PortStackMode::Normal;
                self.push(destructured_child + current_child);
            },
            
            PortStackMode::Deleted { .. } => panic!("it shouldn't be possible to both delete and destructure a node in the same change"),
        }
    }
    
    fn summarized(&mut self) {
        match self.mode {
            PortStackMode::Normal => {
                /* Need to insert MySummary */
                self.push_descent(TokenizerDescent::MySummary);
                self.mode = PortStackMode::Summary;
            },
            PortStackMode::Summary => {
                /* We're already in Summary. Don't care. */
            },
            PortStackMode::Deleted { .. } => {
                /* We're processing stack entries in nodes that have been deleted. Don't care. */
            },
            PortStackMode::Destructuring { .. } => panic!("should be unreachable"),
        }
    }

    fn push_descent(&mut self, descent: TokenizerDescent) {
        descent.build_path(&mut self.current_path);
        
        match &mut self.mode {
            PortStackMode::Normal | PortStackMode::Summary => {
                let childhood = descent.childhood(&self.node);
                let parent_node = std::mem::replace(&mut self.node, childhood.node);

                let tse = TokenizerStackEntry {
                    stack: self.new_stack.take(),
                    descent,
                    depth: self.depth,
                    node: parent_node,
                    node_addr: self.node_addr,
                };

                self.depth+= tse.descent.depth_change();
                self.node_addr+= childhood.offset.to_size();
                self.new_stack = Some(sync::Arc::new(tse));
            },
            PortStackMode::Deleted { ref mut node, ref mut offset_within_parent, .. } => {
                let childhood = descent.childhood(&node);

                *node = childhood.node;
                *offset_within_parent = *offset_within_parent + childhood.offset.to_size();
            },
            PortStackMode::Destructuring { .. } => panic!("should be unreachable"),
        }
    }
    
    fn push(&mut self, child: usize) {
        let descent = match self.node.props.children_display {
            structure::ChildrenDisplay::None => todo!(),
            structure::ChildrenDisplay::Summary => { self.summarized(); TokenizerDescent::ChildSummary(child) }
            structure::ChildrenDisplay::Full => TokenizerDescent::Child(child),
        };
        
        self.push_descent(descent);
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
                            None => Some(inflate_structure(child, addr::unit::NULL, structure::Path::default(), &mut lookup))
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
        
    fn inflate_childhood(xml: roxmltree::Node, parent_addr: addr::Address, child_path: structure::Path, map: &mut collections::HashMap<String, (addr::Address, structure::Path, sync::Arc<structure::Node>)>) -> structure::Childhood {
        let offset = match xml.attribute("offset") {
            Some(addr) => addr::Address::parse(addr).unwrap(),
            None => addr::unit::NULL
        };
        structure::Childhood {
            node: inflate_structure(xml, parent_addr + offset.to_size(), child_path, map),
            offset,
        }
    }
        
    pub fn inflate_structure(xml: roxmltree::Node, node_addr: addr::Address, node_path: structure::Path, map: &mut collections::HashMap<String, (addr::Address, structure::Path, sync::Arc<structure::Node>)>) -> sync::Arc<structure::Node> {
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
                    None => structure::ContentDisplay::default(),
                    Some("hexstring") => structure::ContentDisplay::Hexstring,
                    Some("hexdump") => structure::ContentDisplay::Hexdump {
                        line_pitch: xml.attribute("line_pitch")
                            .or_else(|| xml.attribute("pitch"))
                            .map_or(
                                16.into(),
                                |p| addr::Address::parse(p).map_or_else(                                
                                    |e| panic!("expected valid pitch, got '{}' ({:?})", p, e),
                                    |a| a.to_size())),
                        gutter_pitch: xml.attribute("gutter_pitch").map_or(
                            8.into(),
                            |p| addr::Address::parse(p).map_or_else(                                
                                |e| panic!("expected valid pitch, got '{}' ({:?})", p, e),
                                |a| a.to_size())),
                    },
                    Some("none") => structure::ContentDisplay::None,
                    Some(invalid) => panic!("invalid content attribute: {}", invalid)
                },
                locked: true,
            },
            children: xml.children().filter(|c| c.is_element()).enumerate().map(|(i, c)| {
                let mut path = node_path.clone();
                path.push(i);
                inflate_childhood(c, node_addr, path, map)
            }).collect()
        };
        let arc = sync::Arc::new(node);
        map.insert(arc.props.name.clone(), (node_addr, node_path, arc.clone()));
        arc
    }

    impl TokenDef {
        fn into_token(self, lookup: &collections::HashMap<String, (addr::Address, structure::Path, sync::Arc<structure::Node>)>) -> token::Token {
            let lookup_result = lookup.get(&self.node_name).unwrap_or_else(|| panic!("expected a node named '{}'", self.node_name));
            token::Token {
                class: self.class,
                node: lookup_result.2.clone(),
                node_path: lookup_result.1.clone(),
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

    use crate::model::document;
    use crate::model::versioned::Versioned;
    
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
                node: root.clone(), node_path: vec![], node_addr: 0.into(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Title,
                node: root.clone(), node_path: vec![], node_addr: 0.into(), depth: 0, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x0, 0x10)),
                node: root.clone(), node_path: vec![], node_addr: 0.into(), depth: 1, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x10, 0x20)),
                node: root.clone(), node_path: vec![], node_addr: 0.into(), depth: 1, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x20, 0x30)),
                node: root.clone(), node_path: vec![], node_addr: 0.into(), depth: 1, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x30, 0x32)),
                node: root.clone(), node_path: vec![], node_addr: 0.into(), depth: 1, newline: true
            },
            /* child */
            token::Token {
                class: token::TokenClass::Punctuation(token::PunctuationClass::Empty),
                node: child.clone(), node_path: vec![0], node_addr: 0x32.into(), depth: 1, newline: true
            },
            token::Token {
                class: token::TokenClass::Title,
                node: child.clone(), node_path: vec![0], node_addr: 0x32.into(), depth: 1, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x0, 0x10)),
                node: child.clone(), node_path: vec![0], node_addr: 0x32.into(), depth: 2, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x10, 0x18)),
                node: child.clone(), node_path: vec![0], node_addr: 0x32.into(), depth: 2, newline: true
            },
            token::Token {
                class: token::TokenClass::Punctuation(token::PunctuationClass::Empty),
                node: child.clone(), node_path: vec![0], node_addr: 0x32.into(), depth: 1, newline: true
            },
            /* root */
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x4a, 0x50)),
                node: root.clone(), node_path: vec![], node_addr: 0.into(), depth: 1, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x50, 0x60)),
                node: root.clone(), node_path: vec![], node_addr: 0.into(), depth: 1, newline: true
            },
            token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::between(0x60, 0x70)),
                node: root.clone(), node_path: vec![], node_addr: 0.into(), depth: 1, newline: true
            },
            token::Token {
                class: token::TokenClass::Punctuation(token::PunctuationClass::Empty),
                node: root.clone(), node_path: vec![], node_addr: 0.into(), depth: 0, newline: true
            },
        ];

        let testcase = xml::Testcase {
            structure: root,
            expected_tokens,
        };

        test_forward(&testcase);
        test_backward(&testcase);
    }

    fn seek_to_token(tokenizer: &mut Tokenizer, target: &token::Token) {
        while match tokenizer.gen_token() {
            TokenGenerationResult::Ok(token) => &token != target,
            TokenGenerationResult::Skip => true,
            TokenGenerationResult::Boundary => panic!("couldn't find token"),
        } {
            if !tokenizer.move_next() {
                panic!("hit end of token stream");
            }
        }        
    }

    fn peek(tokenizer: &mut Tokenizer) -> token::Token {
        loop {
            match tokenizer.gen_token() {
                TokenGenerationResult::Ok(token) => return token,
                TokenGenerationResult::Skip => assert!(tokenizer.move_next()),
                TokenGenerationResult::Boundary => panic!("couldn't find token"),
            }
        }        
    }

    fn assert_tokenizers_eq(a: &Tokenizer, b: &Tokenizer) {
        assert_eq!(a.state, b.state);
        assert_eq!(a.depth, b.depth);
        assert!(sync::Arc::ptr_eq(&a.node, &b.node));
        assert_eq!(a.node_addr, b.node_addr);

        let mut stack_walker_a = &a.stack;
        let mut stack_walker_b = &b.stack;

        loop {
            let (stack_item_a, stack_item_b) = match (stack_walker_a, stack_walker_b) {
                (Some(a), Some(b)) => (a, b),
                (None, None) => return,
                _ => panic!("mismatch"),
            };

            assert_eq!(stack_item_a.descent, stack_item_b.descent);
            assert_eq!(stack_item_a.depth, stack_item_b.depth);
            assert!(sync::Arc::ptr_eq(&stack_item_a.node, &stack_item_b.node));
            assert_eq!(stack_item_a.node_addr, stack_item_b.node_addr);
            
            stack_walker_a = &stack_item_a.stack;
            stack_walker_b = &stack_item_b.stack;
        };
    }

    fn assert_port_functionality(old_doc: &document::Document, new_doc: &document::Document, records: &[(token::Token, token::Token, PortOptions)]) {
        let mut tokenizers: Vec<(Tokenizer, &token::Token, &token::Token, &PortOptions)> = records.iter().map(|(before_token, after_token, options)| (Tokenizer::at_beginning(old_doc.root.clone()), before_token, after_token, options)).collect();

        for (tokenizer, before_token, _after_token, _) in tokenizers.iter_mut() {
            seek_to_token(tokenizer, before_token);
        }

        for (tokenizer, _before_token, after_token, options) in tokenizers.iter_mut() {
            println!("tokenizer before port: {:#?}", tokenizer);
            new_doc.changes_since_ref(old_doc, &mut |doc, change| tokenizer.port_change(&doc.root, change, options));
            println!("tokenizer after port: {:#?}", tokenizer);
            
            assert_eq!(&peek(tokenizer), *after_token);

            /* Check that the ported tokenizer is the same as if we had created a new tokenizer and seeked it (if only we knew where to seek it to...), i.e. its internal state isn't corrupted in a way that doesn't happen during normal tokenizer movement. */
            let mut clean_tokenizer = Tokenizer::at_beginning(new_doc.root.clone());
            seek_to_token(&mut clean_tokenizer, after_token);
            assert_tokenizers_eq(&tokenizer, &clean_tokenizer);
        }        
    }
    
    #[test]
    fn port_delete_node() {
        let root = structure::Node::builder()
            .name("root")
            .size(0x40)
            .child(0x10, |b| b
                   .name("child0")
                   .size(0x20))
            .child(0x14, |b| b
                   .name("child1")
                   .size(0x50)
                   .child(0x0, |b| b
                          .name("child1.0")
                          .size(0x18))
                   .child(0x20, |b| b
                          .name("child1.1")
                          .size(0x18))
                   .child(0x30, |b| b
                          .name("child1.2")
                          .size(0x18))
                   .child(0x48, |b| b
                          .name("child1.3")
                          .size(0x1c)))
            .child(0x60, |b| b
                   .name("child2")
                   .size(0x4))
            .build();
 
        let old_doc = document::Document::new_for_structure_test(root);
        let mut new_doc = old_doc.clone();
        new_doc.change_for_debug(old_doc.delete_range(vec![1], 1, 2)).unwrap();
        
        let (o_child_1_2, o_child_1_2_addr) = old_doc.lookup_node(&vec![1, 2]);
        let (o_child_1_3, o_child_1_3_addr) = old_doc.lookup_node(&vec![1, 3]);
        let (n_child_1,   n_child_1_addr)   = new_doc.lookup_node(&vec![1]);

        assert_port_functionality(&old_doc, &new_doc, &[
            (
                token::Token {
                    class: token::TokenClass::Hexdump(addr::Extent::sized_u64(0x10, 0x8)),
                    node: o_child_1_2.clone(),
                    node_path: vec![1, 2],
                    node_addr: o_child_1_2_addr,
                    depth: 3,
                    newline: true
                },
                token::Token {
                    class: token::TokenClass::Hexdump(addr::Extent::sized_u64(0x40, 0x8)),
                    node: n_child_1.clone(),
                    node_path: vec![1],
                    node_addr: n_child_1_addr,
                    depth: 2,
                    newline: true
                },
                PortOptionsBuilder::new().build()
            ),
            (
                token::Token {
                    class: token::TokenClass::Hexdump(addr::Extent::sized_u64(0x10, 0xc)),
                    node: o_child_1_3.clone(),
                    node_path: vec![1, 3],
                    node_addr: o_child_1_3_addr,
                    depth: 3,
                    newline: true
                },
                token::Token {
                    class: token::TokenClass::Hexdump(addr::Extent::sized_u64(0x10, 0xc)),
                    /* child1.3 shouldn't be affected, so use the old node and addr to assert that */
                    node: o_child_1_3.clone(),
                    node_path: vec![1, 3],
                    node_addr: o_child_1_3_addr,
                    depth: 3,
                    newline: true
                },
                PortOptionsBuilder::new().build()
            ),
        ]);
    }
}
