//! This module includes the logic that converts from a document structure
//! hierarchy into a seekable stream of tokens.

use std::sync;

use crate::model::addr;
use crate::model::document::change;
use crate::model::document::structure;
use crate::model::document;
use crate::model::listing::token;

use tracing::instrument;

#[derive(Clone, Debug, PartialEq, Eq)]
enum PositionState {
    PreBlank,
    Title,
    
    MetaContent(addr::Address, usize),
    Hexdump {
        extent: addr::Extent,
        line_extent: addr::Extent,
        index: usize
    },
    Hexstring(addr::Extent, usize),
    Ellipsis(addr::Extent, usize),

    SummaryPreamble,
    SummaryOpener,
    /// The argument here is an index for which child is being labelled. Does not tolerate one-past-the-end.
    SummaryLabel(usize),
    /// The argument here is an index for which child comes after the separator. Tolerates one-past-the-end, does not tolerate 0.
    /// We still go through this state for the last child, even though it doesn't have a separator after it,
    /// we just suppress that token when it comes time to generate it.
    SummarySeparator(usize),
    SummaryCloser,
    SummaryEpilogue,

    SummaryValueBegin,
    SummaryLeaf,
    SummaryValueEnd,

    PostBlank,
    End,
}

#[derive(Clone, PartialEq, Eq, Debug)]
enum Descent {
    Child(usize),
    ChildSummary(usize),
    MySummary,
}

#[derive(Clone)]
pub struct StackEntry {
    stack: Option<sync::Arc<StackEntry>>,
    descent: Descent,
    /// How many nodes deep in the hierarchy generated tokens should appear to be (plus or minus one depending on token type)
    apparent_depth: usize,
    /// How long the [stack] chain actually is. Used when comparing Positions.
    logical_depth: usize,
    node: sync::Arc<structure::Node>,
    node_addr: addr::Address,    
}

/* This lets us provide an alternate, simpler implementation to
 * certain unit tests to help isolate bugs to either Position logic
 * or Window/Line logic. */
pub trait AbstractPosition: Clone {
    fn at_beginning(root: sync::Arc<structure::Node>) -> Self;
    fn at_path(root: sync::Arc<structure::Node>, path: &structure::Path, offset: addr::Address) -> Self;
    fn port_change(&mut self, new_doc: &sync::Arc<document::Document>, change: &document::change::Change);
    fn hit_top(&self) -> bool;
    fn hit_bottom(&self) -> bool;
    fn gen_token(&self) -> TokenGenerationResult;
    fn move_prev(&mut self) -> bool;
    fn move_next(&mut self) -> bool;
    fn next_postincrement(&mut self) -> Option<token::Token>;
    fn prev(&mut self) -> Option<token::Token>;
    fn in_summary(&self) -> bool;
}

#[derive(Clone)]
pub struct Position {
    /* invariants:
       - stack should always contain a path all the way back to the root node
     */
    stack: Option<sync::Arc<StackEntry>>,
    state: PositionState,
    /// How many nodes deep in the hierarchy generated tokens should appear to be (plus or minus one depending on token type)
    apparent_depth: usize,
    /// How long the [stack] chain actually is. Used when comparing Positions.
    logical_depth: usize,
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

struct StackDebugHelper<'a>(&'a Option<sync::Arc<StackEntry>>);

impl std::fmt::Debug for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Position")
            .field("state", &self.state)
            .field("node", &self.node.props.name)
            .field("node_addr", &self.node_addr)
            .field("stack", &StackDebugHelper(&self.stack))
            .finish_non_exhaustive()
    }
}

impl<'a> std::fmt::Debug for StackDebugHelper<'a> {
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

impl std::fmt::Debug for StackEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Entry")
            .field("descent", &self.descent)
            .field("node", &self.node.props.name)
            .field("node_addr", &self.node_addr)
            .finish_non_exhaustive()
    }
}

#[non_exhaustive]
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PortOptions {
    pub additional_offset: Option<addr::Size>,
    pub prefer_after_new_node: bool,
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
    new_stack: Option<sync::Arc<StackEntry>>,
    apparent_depth: usize,
    logical_depth: usize,
    node_addr: addr::Address,
    node: sync::Arc<structure::Node>,
}

impl std::fmt::Debug for PortStackState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PortStackState")
            .field("mode", &self.mode)
            .field("current_path", &self.current_path)
            .field("new_stack", &StackDebugHelper(&self.new_stack))
            .field("apparent_depth", &self.apparent_depth)
            .field("logical_depth", &self.logical_depth)
            .field("node_addr", &self.node_addr)
            .field("node", &self.node.props.name)
            .finish_non_exhaustive()
    }
}

#[derive(Debug)]
enum IntermediatePortState {
    Finished(PositionState),

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

    pub fn additional_offset<T: Into<addr::Size>>(mut self, offset: T) -> PortOptionsBuilder {
        self.options.additional_offset = Some(offset.into());
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

impl Position {
    /// Creates a new position seeked to the root of the structure hierarchy and the beginning of the token stream.
    pub fn at_beginning(root: sync::Arc<structure::Node>) -> Position {
        Position {
            stack: None,
            state: PositionState::PreBlank,
            apparent_depth: 0,
            logical_depth: 0,
            node: root,
            node_addr: addr::unit::NULL,
        }
    }

    /// Creates a new position positioned at a specific offset within the node at the given path.
    pub fn at_path(root: sync::Arc<structure::Node>, path: &structure::Path, offset: addr::Address) -> Position {
        let mut node = &root;
        let mut node_addr = addr::unit::NULL;
        let mut apparent_depth = 0;
        let mut logical_depth = 0;
        let mut stack = None;
        let mut summary_prev = false;
        let mut summary_next = match root.props.children_display {
            structure::ChildrenDisplay::Summary => true,
            _ => false
        };
                
        for child_index in path {
            if !summary_prev && summary_next {
                stack = Some(sync::Arc::new(StackEntry {
                    stack: stack.take(),
                    descent: Descent::MySummary,
                    apparent_depth,
                    logical_depth,
                    node: node.clone(),
                    node_addr,
                }));
                /* This is where the difference between logical and apparent depth comes in. Logical depth tracks actual length of stack linked list. */
                logical_depth+= 1;
            }

            summary_prev = summary_next;

            stack = Some(sync::Arc::new(StackEntry {
                stack: stack.take(),
                descent: if summary_prev { Descent::ChildSummary(*child_index) } else { Descent::Child(*child_index) },
                apparent_depth,
                logical_depth,
                node: node.clone(),
                node_addr,
            }));

            let childhood = &node.children[*child_index];
            node = &childhood.node;
            node_addr+= childhood.offset.to_size();
            apparent_depth+= 1;
            logical_depth+= 1;

            summary_next = summary_next || match node.props.children_display {
                structure::ChildrenDisplay::Summary => true,
                _ => false
            };
        }
        
        let mut position = Position {
            stack,
            state: if summary_prev { PositionState::SummaryValueBegin } else { PositionState::PreBlank },
            apparent_depth,
            logical_depth,
            node: node.clone(),
            node_addr
        };

        if offset > addr::unit::NULL {
            position.seek_in_node_to_offset(offset, summary_next);
        }
        
        position
    }
    
    /// Applies a single change to the position state.
    #[instrument]
    pub fn port_change(&mut self, new_root: &sync::Arc<structure::Node>, change: &change::Change, options: &mut PortOptions) {
        /* Recreate our stack, processing descents and such, leaving off with some information about the node we're actually on now. */
        let mut stack_state = match &self.stack {
            Some(parent) => Self::port_recurse(parent, new_root, change),
            None => PortStackState::new(new_root.clone())
        };

        /* Convert our old state into an intermediate state that allows us to represent that we might not know the offset yet, or might not care about figuring it out. */
        let mut intermediate_state = match (&stack_state.mode, &self.state) {
            /* We were in a child that got deleted. Our old state tells us about where we were in the child. */
            (PortStackMode::Deleted { node: _, first_deleted_child_index, offset_within_parent: _, .. }, _) if stack_state.children_summarized() => IntermediatePortState::SummaryLabel(*first_deleted_child_index),
            (PortStackMode::Deleted { node: _, first_deleted_child_index, offset_within_parent, .. }, state) => {
                let offset_within_child = match state {
                    PositionState::MetaContent(offset, _) => *offset,
                    PositionState::Hexdump { extent, .. } => extent.begin,
                    PositionState::Hexstring(extent, _) => extent.begin,
                    _ => addr::unit::NULL
                };
                
                IntermediatePortState::NormalContent(Some(offset_within_child + *offset_within_parent), *first_deleted_child_index)
            },

            /* We were in a child that got destructured. Our old state tells us about where we were in that child. */
            (PortStackMode::Destructuring { destructured_childhood, destructured_child_index, summary: false }, state) => match state {
                PositionState::PreBlank
                    | PositionState::Title
                    | PositionState::SummaryPreamble
                    | PositionState::SummaryOpener
                    | PositionState::SummaryValueBegin => IntermediatePortState::NormalContent(Some(destructured_childhood.offset), *destructured_child_index),
                
                PositionState::MetaContent(offset, index) => IntermediatePortState::NormalContent(Some(destructured_childhood.offset + offset.to_size()), destructured_child_index + *index),
                PositionState::Hexdump { extent, index, .. } => IntermediatePortState::NormalContent(Some(destructured_childhood.offset + extent.begin.to_size()), destructured_child_index + *index),
                PositionState::Hexstring(extent, index) => IntermediatePortState::NormalContent(Some(destructured_childhood.offset + extent.begin.to_size()), destructured_child_index + *index),
                PositionState::Ellipsis(extent, index) => IntermediatePortState::NormalContent(Some(destructured_childhood.offset + extent.begin.to_size()), destructured_child_index + *index),
                PositionState::SummaryLeaf => IntermediatePortState::NormalContent(Some(destructured_childhood.offset), *destructured_child_index),

                PositionState::SummaryLabel(i)
                    | PositionState::SummarySeparator(i) => IntermediatePortState::NormalContent(None, destructured_child_index + *i),

                PositionState::SummaryValueEnd
                    | PositionState::SummaryEpilogue
                    | PositionState::SummaryCloser
                    | PositionState::PostBlank
                    | PositionState::End => IntermediatePortState::NormalContent(Some(destructured_childhood.end()), destructured_child_index + 1),
            },
            // TODO: try harder here
            (PortStackMode::Destructuring { destructured_child_index, summary: true, .. }, _) => IntermediatePortState::SummaryLabel(*destructured_child_index),

            /* If a node was switched from ChildrenDisplay::Full to ChildrenDisplay::Summary, we want to stay on the
             * title or preblank if we were on them, but otherwise we pretend as if we're in PortStackMode::Summary and
             * fixup the Descent::MySummary later. */
            (PortStackMode::Normal, PositionState::PreBlank) => IntermediatePortState::Finished(PositionState::PreBlank),
            (PortStackMode::Normal, PositionState::Title) => IntermediatePortState::Finished(PositionState::Title),
            
            (PortStackMode::Normal, state) if !stack_state.children_summarized() => match state {
                PositionState::PreBlank => IntermediatePortState::Finished(PositionState::PreBlank),
                PositionState::Title => IntermediatePortState::Finished(PositionState::Title),
                
                PositionState::MetaContent(offset, index) => IntermediatePortState::NormalContent(Some(*offset), *index),
                PositionState::Hexdump { extent, index, .. } => IntermediatePortState::NormalContent(Some(extent.begin), *index),
                PositionState::Hexstring(extent, index) => IntermediatePortState::NormalContent(Some(extent.begin), *index),
                PositionState::Ellipsis(extent, index) => IntermediatePortState::NormalContent(Some(extent.begin), *index),

                PositionState::SummaryPreamble => IntermediatePortState::Finished(PositionState::Title),
                PositionState::SummaryOpener => IntermediatePortState::NormalContent(Some(addr::unit::NULL), 0),
                PositionState::SummaryLabel(i) => IntermediatePortState::NormalContent(None, *i),
                PositionState::SummarySeparator(i) => IntermediatePortState::NormalContent(None, *i),
                PositionState::SummaryCloser => IntermediatePortState::Finished(PositionState::End),
                PositionState::SummaryEpilogue => IntermediatePortState::Finished(PositionState::PostBlank),
                PositionState::SummaryValueBegin => IntermediatePortState::Finished(PositionState::Title),
                PositionState::SummaryLeaf => IntermediatePortState::NormalContent(Some(addr::unit::NULL), 0),
                PositionState::SummaryValueEnd => IntermediatePortState::Finished(PositionState::End),

                PositionState::PostBlank => IntermediatePortState::Finished(PositionState::PostBlank),
                PositionState::End => IntermediatePortState::Finished(PositionState::End),
            },

            (PortStackMode::Normal | PortStackMode::Summary, state) => match state {
                // TODO: these are maybe wrong and should be tested
                PositionState::PreBlank =>
                    IntermediatePortState::Finished(PositionState::SummaryValueBegin),
                PositionState::Title =>
                    IntermediatePortState::Finished(PositionState::SummaryValueBegin),
                
                PositionState::MetaContent(_, index) => IntermediatePortState::SummaryLabel(*index),
                PositionState::Hexdump { index, .. } => IntermediatePortState::SummaryLabel(*index),
                PositionState::Hexstring(_, index) => IntermediatePortState::SummaryLabel(*index),
                PositionState::Ellipsis(_, index) => IntermediatePortState::SummaryLabel(*index),

                PositionState::SummaryPreamble => IntermediatePortState::Finished(PositionState::SummaryPreamble),
                PositionState::SummaryOpener => IntermediatePortState::Finished(PositionState::SummaryOpener),
                PositionState::SummaryLabel(i) => IntermediatePortState::SummaryLabel(*i),
                PositionState::SummarySeparator(i) => IntermediatePortState::SummarySeparator(*i),
                PositionState::SummaryCloser => IntermediatePortState::Finished(PositionState::SummaryCloser),
                PositionState::SummaryEpilogue => IntermediatePortState::Finished(PositionState::SummaryEpilogue),
                PositionState::SummaryValueBegin => IntermediatePortState::Finished(PositionState::SummaryValueBegin),
                PositionState::SummaryLeaf => IntermediatePortState::Finished(PositionState::SummaryLeaf),
                PositionState::SummaryValueEnd => IntermediatePortState::Finished(PositionState::SummaryValueEnd),

                PositionState::PostBlank => IntermediatePortState::Finished(PositionState::SummaryEpilogue),
                PositionState::End => IntermediatePortState::Finished(PositionState::SummaryCloser),
            }
        };

        /* It's possible for port_stack to not handle the MySummary descent if the node we were on got switched to
         * summary rather than one of its ancestors. */
        if match &intermediate_state {
            IntermediatePortState::SummaryLabel(_) => true,
            IntermediatePortState::SummarySeparator(_) => true,
            IntermediatePortState::Finished(state) => state.is_summary(),
            _ => false
        } {
            stack_state.summarize();
        }
        
        let is_summary = stack_state.summarized();

        *self = Position {
            stack: stack_state.new_stack,
            state: PositionState::End, /* this is a placeholder. we finalize the details later... */
            apparent_depth: stack_state.apparent_depth,
            logical_depth: stack_state.logical_depth,
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
        if let (Some(offset), Some(additional_offset)) = (offset.as_mut(), options.additional_offset) {
            *offset = *offset + additional_offset;
        }
        
        /* Adjust the offset and index. */
        match &change.ty {
            change::ChangeType::AlterNode { .. } => {},
            change::ChangeType::AlterNodesBulk { .. } => {},
            change::ChangeType::StackFilter(_) => {},
            
            change::ChangeType::InsertNode { parent: affected_path, index: affected_index, child: new_childhood } if affected_path == &stack_state.current_path => {
                /* A new child was added to the node we're on. */
                if *index == *affected_index && options.prefer_after_new_node {
                    /* options said we should place after the new node, so do so. */
                    *index+= 1;
                    *offset = Some(new_childhood.end());
                } else if let Some(offset) = offset.as_mut() {
                    if new_childhood.extent().includes(*offset) {
                        /* if new node contains our offset, we need to descend into it. The state here is, once again, a placeholder. */
                        self.descend(if is_summary { Descent::ChildSummary(*affected_index) } else { Descent::Child(*affected_index) }, PositionState::End);

                        *index = 0;
                        *offset-= new_childhood.offset.to_size();
                    } else if *index == *affected_index && *offset > new_childhood.offset {
                        *index+= 1;
                    } else if *index > *affected_index {
                        *index+= 1;
                    }
                } else if *index >= *affected_index {
                    /* If the new node was inserted before the child we were on, need to bump our child index unless we already descended into the inserted child. */
                    *index+= 1;
                }
            },
            
            change::ChangeType::Nest { range, extent, props: _ } if range.parent == stack_state.current_path => {
                /* Children were nested on this node */
                let new_nest = &self.node.children[range.first];
                
                if range.contains_index(*index) && offset.map_or(false, |o| extent.includes(o)) {
                    if options.prefer_after_new_node {
                        /* options said we should place after the new node, so do so. */
                        *index = range.first + 1;
                        *offset = Some(new_nest.end());
                    } else {
                        /* descend into the new node. */
                        let new_nest_offset = new_nest.offset;
                        
                        self.descend(if is_summary { Descent::ChildSummary(range.first) } else { Descent::Child(range.first) }, PositionState::End);

                        // TODO: is there something more helpful we could do here?
                        *index = 0;
                        
                        if let Some(offset) = offset.as_mut() {
                            *offset-= new_nest_offset.to_size();
                        }
                    }
                } else if *index > range.last {
                    /* If the new node was nested before the child we were on, need to adjust our child index */
                    *index-= range.count() - 1;
                }
            },

            change::ChangeType::Destructure { parent, .. } if parent == &stack_state.current_path => {
                /* Handled by PortStackMode::Destructuring, so we don't have to deal with it here. */
            },

            /* If the node we were on (or an ancestor of it) were deleted, that was already handled by port_recurse. Here we're only worried about our direct children (that we're not positioned on) being deleted. */
            change::ChangeType::DeleteRange { range } if range.parent == stack_state.current_path => {
                if range.contains_index(*index) {
                    *index = range.first;
                } else if *index > range.last {
                    *index-= range.count();
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

            IntermediatePortState::NormalContent(offset, index) => {
                let line_begin = self.estimate_line_begin(offset, index);
                /* Output the difference between the requested "additional offset" and where the token actually will begin. */
                options.additional_offset = offset.and_then(|offset| if offset >= line_begin {
                    Some(offset - line_begin)
                } else {
                    None
                });
                PositionState::MetaContent(line_begin, index)
            },

            /* Real PositionState doesn't support one-past-the-end for SummaryLabel and SummarySeparator, so need to fix if that would be the case. */
            IntermediatePortState::SummaryLabel(index) if index < children.len() => PositionState::SummaryLabel(index),
            IntermediatePortState::SummarySeparator(index) if index < children.len() => PositionState::SummarySeparator(index),
            IntermediatePortState::SummaryLabel(_) => PositionState::SummaryCloser,
            IntermediatePortState::SummarySeparator(_) => PositionState::SummaryCloser,
        };

        /* Adjust our stream position to actually be on a token. */
        while match self.gen_token() {
            TokenGenerationResult::Skip => true,
            _ => false
        } {
            self.move_next();
        }
    }

    /// Used to recurse to the base of the stack so we can start porting from the top down. Returns whether or not to keep going.
    #[instrument]
    fn port_recurse(tok: &StackEntry, new_root: &sync::Arc<structure::Node>, change: &change::Change) -> PortStackState {
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

    /// Applies a change to a single item in the stack. If we need to stop descending in the middle of the stack, return the 
    #[instrument]
    fn port_stack_entry(state: &mut PortStackState, old_tok: &StackEntry, change: &change::Change) {
        /* This logic more-or-less mirrors change::update_path */
        let child_index = match old_tok.descent {
            Descent::Child(child_index) | Descent::ChildSummary(child_index) => child_index,
            
            /* This is handled implicitly by PortStackState::push behavior. */
            Descent::MySummary => return,
        };
        
        match &change.ty {
            change::ChangeType::AlterNode { .. } => state.push(child_index),
            change::ChangeType::AlterNodesBulk { .. } => state.push(child_index),
            change::ChangeType::StackFilter(_) => state.push(child_index),
            change::ChangeType::InsertNode { parent: path, index: after_child, child: _ } => {
                if path == &state.current_path && child_index >= *after_child {
                    state.push(child_index + 1);
                } else {
                    state.push(child_index);
                }
            },
            change::ChangeType::Nest { range, extent: _, props: _ } => {
                if range.parent == state.current_path {
                    if range.contains_index(child_index) {
                        state.push(range.first);
                        state.push(child_index - range.first);
                    } else if child_index > range.last {
                        state.push(child_index - (range.count() - 1));
                    }
                } else {
                    state.push(child_index);
                }
            },
            change::ChangeType::Destructure { parent, child_index: destructured_child, num_grandchildren, offset } => {
                if parent == &state.current_path {
                    state.destructure(*destructured_child, child_index, *num_grandchildren, *offset, &old_tok.node.children[*destructured_child]);
                } else {
                    state.push(child_index);
                }
            },
            change::ChangeType::DeleteRange { range } => {
                if range.parent == state.current_path {
                    if range.contains_index(child_index) {
                        state.deleted(range.first, child_index, &old_tok.node);
                    } else if child_index > range.last {
                        state.push(child_index - range.count());
                    }
                } else {
                    state.push(child_index);
                }
            },
        }
    }

    fn seek_in_node_to_offset(&mut self, offset: addr::Address, summary: bool) {
        let index = self.node.children.partition_point(|ch| ch.offset < offset);
        
        if summary {
            if self.node.children.len() > 0 {
                self.state = PositionState::SummaryLabel(index);
            } else {
                self.state = PositionState::SummaryLeaf;
            }
        } else {
            self.state = PositionState::MetaContent(self.estimate_line_begin(Some(offset), index), index);
        }

        while match self.gen_token() {
            TokenGenerationResult::Skip => true,
            _ => false
        } {
            self.move_next();
        }
    }
    
    /// Creates a new position seeked to the end of the token stream.
    pub fn at_end(root: &sync::Arc<structure::Node>) -> Position {
        Position {
            stack: None,
            state: PositionState::End,
            apparent_depth: 0,
            logical_depth: 0,
            node: root.clone(),
            node_addr: addr::unit::NULL,
        }
    }

    /// Returns the token at the current position, or Skip if the current position in the stream doesn't generate a token.
    pub fn gen_token(&self) -> TokenGenerationResult {
        let common = token::TokenCommon {
            node: self.node.clone(),
            node_path: self.structure_path(),
            node_addr: self.node_addr,
            node_child_index: self.structure_position_child(),
            depth: self.apparent_depth,
        };
        
        match self.state {
            PositionState::PreBlank => if self.node.props.title_display.has_blanks() {
                TokenGenerationResult::Ok(token::BlankLine {
                    common,
                    accepts_cursor: false,
                }.into())
            } else {
                TokenGenerationResult::Skip
            },
            PositionState::Title => TokenGenerationResult::Ok(token::Title {
                common,
            }.into()),
            
            PositionState::MetaContent(_, _) => TokenGenerationResult::Skip,
            PositionState::Hexdump { extent, line_extent, .. } => TokenGenerationResult::Ok(token::Hexdump {
                common: common.adjust_depth(1),
                extent,
                line: line_extent,
            }.into()),
            PositionState::Hexstring(extent, _) => TokenGenerationResult::Ok(token::Hexstring {
                common: common.adjust_depth(1),
                extent,
            }.into()),
            PositionState::Ellipsis(extent, _) => TokenGenerationResult::Ok(token::Ellipsis {
                common: common.adjust_depth(1),
                extent,
            }.into()),

            PositionState::SummaryPreamble => TokenGenerationResult::Ok(token::SummaryPreamble {
                common,
            }.into()),
            PositionState::SummaryOpener => TokenGenerationResult::Ok(token::SummaryPunctuation {
                common,
                kind: token::PunctuationKind::OpenBracket,
            }.into()),
            PositionState::SummaryLabel(i) => {
                let ch = &self.node.children[i];
                let mut path = self.structure_path();
                path.push(i);
                TokenGenerationResult::Ok(token::SummaryLabel {
                    common: token::TokenCommon {
                        node: ch.node.clone(),
                        node_path: path,
                        node_addr: self.node_addr + ch.offset.to_size(),
                        node_child_index: 0,
                        depth: self.apparent_depth,
                    },
                }.into())
            },
            PositionState::SummarySeparator(i) => if i < self.node.children.len() {
                TokenGenerationResult::Ok(token::SummaryPunctuation {
                    common,
                    kind: token::PunctuationKind::Comma,
                }.into())
            } else {
                TokenGenerationResult::Skip
            },
            PositionState::SummaryCloser => TokenGenerationResult::Ok(token::SummaryPunctuation {
                common,
                kind: token::PunctuationKind::CloseBracket,
            }.into()),
            PositionState::SummaryEpilogue => TokenGenerationResult::Ok(token::SummaryEpilogue {
                common,
            }.into()),
            
            PositionState::SummaryValueBegin => TokenGenerationResult::Skip,
            PositionState::SummaryLeaf => {
                let limit = std::cmp::min(16.into(), self.node.size);
                let extent = addr::Extent::between(addr::unit::NULL, limit.to_addr());
                
                TokenGenerationResult::Ok(match self.node.props.content_display {
                    structure::ContentDisplay::None => token::SummaryPunctuation {
                        common,
                        kind: token::PunctuationKind::Space,
                    }.into(),
                    // Disallow hexdumps in summaries. This is a little nasty. Review later.
                    structure::ContentDisplay::Hexdump { .. } => token::Hexstring {
                        common,
                        extent,
                    }.into(),
                    structure::ContentDisplay::Hexstring => token::Hexstring {
                        common,
                        extent,
                    }.into(),
                })
            },
            PositionState::SummaryValueEnd => TokenGenerationResult::Skip,

            PositionState::PostBlank => if self.node.props.title_display.has_blanks() {
                TokenGenerationResult::Ok(token::BlankLine {
                    common: common.adjust_depth(1),
                    accepts_cursor: true,
                }.into())
            } else {
                TokenGenerationResult::Skip
            },
            PositionState::End => TokenGenerationResult::Skip,
        }
    }

    /// Return the extent of the line containing the given address, biased downwards.
    fn get_line_extent(&self, offset: addr::Address, pitch: addr::Size) -> addr::Extent {
        addr::Extent::sized((pitch * (offset.to_size() / pitch)).to_addr(), pitch)
    }

    fn try_get_natural_line_extent(&self, offset: addr::Address) -> Option<addr::Extent> {
        self.node.props.content_display.preferred_pitch().map(|pitch| self.get_line_extent(offset, pitch))
    }

    /// Returns the boundaries of the children at index-1 and index. This is not an [addr::Extent] because the latter
    /// child may begin at a lower address than the former child ends at.
    fn get_interstitial(&self, index: usize) -> (addr::Address, addr::Address) {
        /* Find the children. */
        let prev_child_option = match index {
            0 => None,
            /* Something is seriously wrong if index was farther than one-past-the-end. */
            i => Some((i-1, &self.node.children[i-1]))
        };
        let next_child_option = self.node.children.get(index).map(|child| (index, child));

        /* Find the limits */
        let lower_limit = match prev_child_option {
            /* Can't include data from the child, so need to stop after its end. */
            Some((_, prev_child)) => prev_child.end(),
            /* Can't include data that belongs to the parent, so need to stop before our begin. */
            None => addr::unit::NULL,
        };

        /* Where can we not end beyond? */
        let upper_limit = match next_child_option {
            /* Can't include data from the child, so need to stop before it begins. */
            Some((_, next_child)) => next_child.offset,
            /* Can't include data that belongs to the parent, so need to stop before we end. */
            None => self.node.size.to_addr(),
        };

        (lower_limit, upper_limit)
    }

    /// Despite the name, this must return exactly the same boundary that move_next and move_prev would so that we don't
    /// seek to different tokens than what scrolling would've produced.
    fn estimate_line_begin(&self, offset: Option<addr::Address>, index: usize) -> addr::Address {
        let interstitial = self.get_interstitial(index);

        if let Some(offset) = offset {
            match self.try_get_natural_line_extent(offset) {
                Some(le) => std::cmp::max(le.begin, interstitial.0),
                None => interstitial.0
            }
                
        } else {
            interstitial.0
        }
    }
    
    /// Moves one position backwards in the stream.
    /// Returns true when successful, or false if hit the beginning of the token stream.
    pub fn move_prev(&mut self) -> bool {
        match self.state {
            PositionState::PreBlank => {
                self.try_ascend(AscendDirection::Prev)
            },
            PositionState::Title => {
                self.state = PositionState::PreBlank;
                true
            },
            PositionState::SummaryPreamble => {
                self.state = PositionState::Title;
                true
            },
            
            PositionState::MetaContent(offset, index) => {
                let prev_child_option = match index {
                    0 => None,
                    /* Something is seriously wrong if index was farther than one-past-the-end. */
                    i => Some((i-1, &self.node.children[i-1]))
                };

                /* Descend, if we can. */
                if let Some((prev_child_index, prev_child)) = prev_child_option {
                    if prev_child.end() >= offset {
                        self.descend(
                            Descent::Child(prev_child_index),
                            /* Descend to thse end of the child. */
                            PositionState::End);

                        return true;
                    }
                }

                /* Emit content, if we can. */
                if offset > addr::unit::NULL {
                    let interstitial = self.get_interstitial(index);
                    assert!(interstitial.1 > interstitial.0);
                    let interstitial = addr::Extent::between(interstitial.0, interstitial.1);
                    
                    self.state = match self.node.props.content_display {
                        structure::ContentDisplay::None => PositionState::Ellipsis(interstitial, index),
                        structure::ContentDisplay::Hexdump { line_pitch, gutter_pitch: _ } => {
                            let line_extent = self.get_line_extent(offset - addr::unit::BIT, line_pitch);

                            PositionState::Hexdump {
                                extent: addr::Extent::between(std::cmp::max(interstitial.begin, line_extent.begin), offset),
                                line_extent,
                                index
                            }
                        }
                        structure::ContentDisplay::Hexstring => PositionState::Hexstring(interstitial, index),
                    };
                    
                    return true;
                }
                
                /* We're pointed at the beginning. Emit the title block. */
                self.state = PositionState::Title;
                true
            },
            PositionState::Hexstring(extent, index) => {
                self.state = PositionState::MetaContent(extent.begin, index);
                true
            },
            PositionState::Hexdump { extent, index, .. } => {
                self.state = PositionState::MetaContent(extent.begin, index);
                true
            },
            PositionState::Ellipsis(extent, index) => {
                self.state = PositionState::MetaContent(extent.begin, index);
                true
            },

            PositionState::SummaryOpener => {
                self.try_ascend(AscendDirection::Prev)
            },
            PositionState::SummaryLabel(i) => {
                if i == 0 {
                    self.state = PositionState::SummaryOpener;
                } else {
                    self.state = PositionState::SummarySeparator(i);
                }
                true
            },
            PositionState::SummarySeparator(i) => {
                self.descend(
                    Descent::ChildSummary(i-1),
                    PositionState::SummaryValueEnd);
                true
            },
            PositionState::SummaryCloser => {
                if self.node.children.is_empty() {
                    self.state = PositionState::SummaryOpener;
                } else {
                    self.state = PositionState::SummarySeparator(self.node.children.len());
                }
                true
            },
            PositionState::SummaryEpilogue => {
                self.descend(
                    Descent::MySummary,
                    PositionState::SummaryCloser);
                true
            },
            
            PositionState::SummaryValueBegin => {
                // should take us to SummaryLabel(i)
                self.try_ascend(AscendDirection::Prev)
            },
            PositionState::SummaryLeaf => {
                self.state = PositionState::SummaryValueBegin;
                true
            },
            PositionState::SummaryValueEnd => {
                if self.node.children.is_empty() {
                    self.state = PositionState::SummaryLeaf;
                } else {
                    self.state = PositionState::SummaryCloser;
                }
                true
            },

            PositionState::PostBlank => {
                match self.node.props.children_display {
                    structure::ChildrenDisplay::None => {
                        self.state = PositionState::MetaContent(self.node.size.to_addr(), self.node.children.len());
                    },
                    structure::ChildrenDisplay::Summary => {
                        self.state = PositionState::SummaryEpilogue;
                    },
                    structure::ChildrenDisplay::Full => {
                        self.state = PositionState::MetaContent(self.node.size.to_addr(), self.node.children.len());
                    },
                }
                true
            },
            PositionState::End => {
                self.state = PositionState::PostBlank;
                true
            },
        }
    }

    /// Moves one position forwards in the stream.
    /// Returns true when successful, or false if hit the end of the token stream.
    pub fn move_next(&mut self) -> bool {
        match self.state {
            PositionState::PreBlank => {
                self.state = PositionState::Title;
                true
            },
            PositionState::Title => {
                match self.node.props.children_display {
                    structure::ChildrenDisplay::None => {
                        self.state = PositionState::MetaContent(addr::unit::NULL, 0);
                    },
                    structure::ChildrenDisplay::Summary => {
                        self.state = PositionState::SummaryPreamble;
                    },
                    structure::ChildrenDisplay::Full => {
                        self.state = PositionState::MetaContent(addr::unit::NULL, 0);
                    },
                }
                true
            },
            PositionState::SummaryPreamble => {
                self.descend(
                    Descent::MySummary,
                    PositionState::SummaryOpener);
                true

            },
            PositionState::MetaContent(offset, index) => {
                let next_child_option = self.node.children.get(index).map(|child| (index, child));
                
                /* Descend, if we can. */
                if let Some((next_child_index, next_child)) = next_child_option {
                    if next_child.offset <= offset {
                        self.descend(
                            Descent::Child(next_child_index),
                            /* Descend to the beginning of the child. */
                            PositionState::PreBlank);

                        return true;
                    }
                }

                /* Emit content, if we can. */
                if offset < self.node.size.to_addr() {
                    let interstitial = self.get_interstitial(index);
                    assert!(interstitial.1 > interstitial.0);
                    let interstitial = addr::Extent::between(interstitial.0, interstitial.1);

                    self.state = match self.node.props.content_display {
                        structure::ContentDisplay::None => PositionState::Ellipsis(interstitial, index),
                        structure::ContentDisplay::Hexdump { line_pitch, gutter_pitch: _ } => {
                            let line_extent = self.get_line_extent(offset, line_pitch);
                            
                            PositionState::Hexdump {
                                extent: addr::Extent::between(offset, std::cmp::min(line_extent.end, interstitial.end)),
                                line_extent,
                                index
                            }
                        },
                        structure::ContentDisplay::Hexstring => PositionState::Hexstring(interstitial, index),
                    };

                    return true;
                }

                /* We were pointed at (or past!) the end. */
                self.state = PositionState::PostBlank;
                true
            },
            PositionState::Hexstring(extent, index) => {
                self.state = PositionState::MetaContent(extent.end, index);
                true
            },
            PositionState::Hexdump { extent, index, .. } => {
                self.state = PositionState::MetaContent(extent.end, index);
                true
            },
            PositionState::Ellipsis(extent, index) => {
                self.state = PositionState::MetaContent(extent.end, index);
                true
            },

            PositionState::SummaryOpener => {
                if self.node.children.is_empty() {
                    self.state = PositionState::SummaryCloser;
                } else {
                    self.state = PositionState::SummaryLabel(0);
                }
                true
            },
            PositionState::SummaryLabel(i) => {
                self.descend(
                    Descent::ChildSummary(i),
                    PositionState::SummaryValueBegin);
                true
            },
            PositionState::SummarySeparator(i) => {
                if self.node.children.len() == i {
                    self.state = PositionState::SummaryCloser;
                } else {
                    self.state = PositionState::SummaryLabel(i);
                }
                true
            },
            PositionState::SummaryCloser => {
                self.try_ascend(AscendDirection::Next)
            },
            PositionState::SummaryEpilogue => {
                self.state = PositionState::PostBlank;
                true
            },

            PositionState::SummaryValueBegin => {
                if self.node.children.is_empty() {
                    self.state = PositionState::SummaryLeaf;
                } else {
                    self.state = PositionState::SummaryOpener;
                }
                true
            },
            PositionState::SummaryLeaf => {
                self.state = PositionState::SummaryValueEnd;
                true
            },
            PositionState::SummaryValueEnd => {
                self.try_ascend(AscendDirection::Next)
            },

            PositionState::PostBlank => {
                self.state = PositionState::End;
                true
            },
            PositionState::End => {
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
    
    /// Use this when you're trying to have the position represent an element.
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
    
    /// Use this when you're trying to have the position represent a boundary between tokens.
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

    /// If the current position in the stream does not generate a token, moves to the next one that does.
    pub fn canonicalize_next(&mut self) {
        while match self.gen_token() {
            TokenGenerationResult::Ok(_) => false,
            TokenGenerationResult::Skip => true,
            TokenGenerationResult::Boundary => false,
        } {
            if !self.move_next() {
                break;
            }
        }
    }

    /// Pushes an entry onto the stack and sets up for traversing
    /// a child node.
    ///
    /// # Arguments
    ///
    /// * `descent` - The type of descent being performed.
    /// * `state_within` - Where within the child to descend to.
    ///
    fn descend(
        &mut self,
        descent: Descent,
        state_within: PositionState) {
        let childhood = descent.childhood(&self.node);
        let parent_node = std::mem::replace(&mut self.node, childhood.node);
        let depth_change = descent.depth_change();
        
        let parent_entry = StackEntry {
            stack: self.stack.take(),
            descent,
            apparent_depth: self.apparent_depth,
            logical_depth: self.logical_depth,
            node: parent_node,
            node_addr: self.node_addr,
        };

        self.apparent_depth+= depth_change;
        self.logical_depth+= 1;
        self.stack = Some(sync::Arc::new(parent_entry));
        self.state = state_within;
        self.node_addr+= childhood.offset.to_size();
    }
    
    /// Replaces our context with the parent's context, returning false if there
    /// was no parent.
    fn try_ascend(&mut self, dir: AscendDirection) -> bool {
        match std::mem::replace(&mut self.stack, None) {
            Some(stack_entry) => {
                let stack_entry = sync::Arc::unwrap_or_clone(stack_entry);
                *self = Position {
                    state: match dir {
                        AscendDirection::Prev => stack_entry.descent.before_state(&stack_entry),
                        AscendDirection::Next => stack_entry.descent.after_state(&stack_entry)
                    },
                    stack: stack_entry.stack,
                    apparent_depth: stack_entry.apparent_depth,
                    logical_depth: stack_entry.logical_depth,
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
            PositionState::End => self.stack.is_none(),
            _ => false
        }
    }

    pub fn hit_top(&self) -> bool {
        match self.state {
            PositionState::PreBlank => self.stack.is_none(),
            _ => false
        }
    }

    pub fn node_addr(&self) -> addr::Address {
        self.node_addr
    }
    
    pub fn structure_path(&self) -> structure::Path {
        let mut path = Vec::new();

        StackEntry::build_path(&self.stack, &mut path);
        
        path
    }

    pub fn structure_position_child(&self) -> usize {
        match self.state {
            PositionState::MetaContent(_, ch) => ch,
            PositionState::Hexdump { index: ch, .. } => ch,
            PositionState::Hexstring(_, ch) => ch,
            PositionState::Ellipsis(_, ch) => ch,
            PositionState::SummaryLabel(ch) => ch,
            PositionState::SummarySeparator(ch) => ch,
            PositionState::SummaryCloser => self.node.children.len(),
            PositionState::SummaryEpilogue => self.node.children.len(),
            PositionState::PostBlank => self.node.children.len(),
            PositionState::End => self.node.children.len(),
            _ => 0,
        }
    }

    pub fn structure_position_offset(&self) -> addr::Address {
        match self.state {
            PositionState::PreBlank => addr::unit::NULL,
            PositionState::Title => addr::unit::NULL,
            PositionState::MetaContent(offset, _) => offset,
            PositionState::Hexdump { extent, .. } => extent.begin,
            PositionState::Hexstring(extent, _) => extent.begin,
            PositionState::Ellipsis(extent, _) => extent.begin,
            PositionState::SummaryPreamble => addr::unit::NULL,
            PositionState::SummaryOpener => addr::unit::NULL,
            PositionState::SummaryLabel(i) => self.node.children[i].offset,
            PositionState::SummarySeparator(i) => self.node.children[i-1].end(),
            PositionState::SummaryCloser => self.node.size.to_addr(),
            PositionState::SummaryEpilogue => self.node.size.to_addr(),
            PositionState::SummaryValueBegin => addr::unit::NULL,
            PositionState::SummaryLeaf => addr::unit::NULL,
            PositionState::SummaryValueEnd => self.node.size.to_addr(),
            PositionState::PostBlank => self.node.size.to_addr(),
            PositionState::End => self.node.size.to_addr(),
        }
    }

    /// Returns true if the token that would be returned by gen_token() is part of a summary.
    pub fn in_summary(&self) -> bool {
        match self.state {
            PositionState::PreBlank => false,
            PositionState::Title => false,
            PositionState::MetaContent(_, _) => false,
            PositionState::Hexdump { .. } => false,
            PositionState::Hexstring(_, _) => false,
            PositionState::Ellipsis(_, _) => false,

            PositionState::SummaryPreamble => true,
            PositionState::SummaryOpener => true,
            PositionState::SummaryLabel(_) => true,
            PositionState::SummarySeparator(_) => true,
            PositionState::SummaryCloser => true,
            PositionState::SummaryEpilogue => true,
            PositionState::SummaryValueBegin => true,
            PositionState::SummaryLeaf => true,
            PositionState::SummaryValueEnd => true,

            PositionState::PostBlank => false,
            PositionState::End => false,
        }
    }
}

impl PartialEq for Position {
    fn eq(&self, other: &Self) -> bool {
        (match (&self.stack, &other.stack) {
            (Some(x), Some(y)) => sync::Arc::ptr_eq(x, y),
            _ => false,
        } || self.stack == other.stack) &&
            self.state == other.state &&
            self.logical_depth == other.logical_depth &&
            sync::Arc::ptr_eq(&self.node, &other.node) &&
            self.node_addr == other.node_addr
    }
}

impl Eq for Position {
}

impl Descent {
    fn childhood(&self, node: &sync::Arc<structure::Node>) -> structure::Childhood {
        match self {
            Descent::Child(i) => node.children[*i].clone(),
            Descent::ChildSummary(i) => node.children[*i].clone(),
            Descent::MySummary => structure::Childhood {
                node: node.clone(),
                offset: addr::unit::NULL,
            },
        }
    }

    fn depth_change(&self) -> usize {
        match self {
            Descent::Child(_) | Descent::ChildSummary(_) => 1,
            Descent::MySummary => 0,
        }
    }

    fn before_state(&self, stack_entry: &StackEntry) -> PositionState {
        match self {
            Descent::Child(i) => PositionState::MetaContent(stack_entry.node.children[*i].offset, *i),
            Descent::ChildSummary(i) => PositionState::SummaryLabel(*i),
            Descent::MySummary => PositionState::SummaryPreamble,
        }
    }

    fn after_state(&self, stack_entry: &StackEntry) -> PositionState {
        match self {
            Descent::Child(i) => PositionState::MetaContent(stack_entry.node.children[*i].end(), *i+1),
            Descent::ChildSummary(i) => PositionState::SummarySeparator(*i+1),
            Descent::MySummary => PositionState::SummaryEpilogue,
        }
    }

    fn build_path(&self, path: &mut structure::Path) {
        match self {
            Descent::Child(i) | Descent::ChildSummary(i) => path.push(*i),
            Descent::MySummary => {},
        }        
    }
}

impl StackEntry {
    fn build_path(entry: &Option<sync::Arc<StackEntry>>, path: &mut structure::Path) {
        if let Some(tse) = entry {
            Self::build_path(&tse.stack, path);
            tse.descent.build_path(path);
        }
    }
}

impl PositionState {
    /// Returns whether or not this state represents a state that should only exist within a Descent::MySummary
    /// stack entry.
    fn is_summary(&self) -> bool {
        match self {
            Self::SummaryPreamble => false,
            Self::SummaryEpilogue => false,
            
            Self::SummaryOpener => true,
            Self::SummaryLabel(_) => true,
            Self::SummarySeparator(_) => true,
            Self::SummaryCloser => true,
            Self::SummaryValueBegin => true,
            Self::SummaryLeaf => true,
            Self::SummaryValueEnd => true,
            _ => false,
        }
    }
}

impl PartialEq for StackEntry {
    fn eq(&self, other: &Self) -> bool {
        (match (&self.stack, &other.stack) {
            (Some(x), Some(y)) => sync::Arc::ptr_eq(x, y),
            _ => false,
        } || self.stack == other.stack) &&
            self.descent == other.descent &&
            self.logical_depth == other.logical_depth &&
            sync::Arc::ptr_eq(&self.node, &other.node) &&
            self.node_addr == other.node_addr
    }
}

impl Eq for StackEntry {
}

impl PortStackState {
    fn new(root: sync::Arc<structure::Node>) -> PortStackState {
        PortStackState {
            mode: PortStackMode::Normal,
            current_path: structure::Path::new(),
            new_stack: None,
            apparent_depth: 0,
            logical_depth: 0,
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

    fn destructure(&mut self, destructured_child: usize, current_child: usize, num_grandchildren: usize, offset: addr::Address, childhood: &structure::Childhood) {
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
    
    fn summarize(&mut self) {
        match self.mode {
            PortStackMode::Normal => {
                /* Need to insert MySummary */
                self.push_descent(Descent::MySummary);
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

    fn push_descent(&mut self, descent: Descent) {
        descent.build_path(&mut self.current_path);
        
        match &mut self.mode {
            PortStackMode::Normal | PortStackMode::Summary => {
                let childhood = descent.childhood(&self.node);
                let parent_node = std::mem::replace(&mut self.node, childhood.node);

                let tse = StackEntry {
                    stack: self.new_stack.take(),
                    descent,
                    apparent_depth: self.apparent_depth,
                    logical_depth: self.logical_depth,
                    node: parent_node,
                    node_addr: self.node_addr,
                };

                self.apparent_depth+= tse.descent.depth_change();
                self.logical_depth+= 1;
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

    fn summarized(&self) -> bool {
        match &self.mode {
            PortStackMode::Normal => false,
            PortStackMode::Summary => true,
            PortStackMode::Deleted { summary, .. } => *summary,
            PortStackMode::Destructuring { summary, .. } => *summary,
        }
    }
    
    fn children_summarized(&self) -> bool {
        self.summarized() || match self.node.props.children_display {
            structure::ChildrenDisplay::None => false,
            structure::ChildrenDisplay::Summary => true,
            structure::ChildrenDisplay::Full => false,
        }
    }
    
    fn push(&mut self, child: usize) {
        let descent = match self.node.props.children_display {
            structure::ChildrenDisplay::None => todo!(),
            structure::ChildrenDisplay::Summary => { self.summarize(); Descent::ChildSummary(child) }
            structure::ChildrenDisplay::Full => Descent::Child(child),
        };
        
        self.push_descent(descent);
    }
}

mod cmp {
    use std::sync;
    use crate::model::addr;
    use crate::model::document::structure;

    #[derive(PartialEq)]
    enum StateOrDescent {
        State(super::PositionState),
        Descent(super::Descent),
    }

    impl std::cmp::PartialOrd for StateOrDescent {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            match (self, other) {
                (Self::State(x), Self::State(y)) => x.partial_cmp(y),
                (Self::Descent(x), Self::Descent(y)) => x.partial_cmp(y),
                (Self::Descent(_), Self::State(_)) => Self::partial_cmp(other, self).map(std::cmp::Ordering::reverse),
                (Self::State(x), Self::Descent(y)) => {
                    let child_index = match y {
                        super::Descent::Child(i) => i,
                        super::Descent::ChildSummary(i) => i,
                        super::Descent::MySummary => return Some(match x {
                            super::PositionState::PreBlank => std::cmp::Ordering::Less,
                            super::PositionState::Title => std::cmp::Ordering::Less,
                            super::PositionState::SummaryPreamble => std::cmp::Ordering::Less,
                            super::PositionState::SummaryEpilogue => std::cmp::Ordering::Greater,
                            super::PositionState::PostBlank => std::cmp::Ordering::Greater,
                            super::PositionState::End => std::cmp::Ordering::Greater,
                            _ => return None,
                        }),
                    };

                    Some(match x {
                        super::PositionState::PreBlank => std::cmp::Ordering::Less,
                        super::PositionState::Title => std::cmp::Ordering::Less,
                        super::PositionState::MetaContent(_, i) if i <= child_index => std::cmp::Ordering::Less,
                        super::PositionState::MetaContent(_, _) => std::cmp::Ordering::Greater,
                        super::PositionState::Hexdump { index, .. } if index == child_index => std::cmp::Ordering::Less,
                        super::PositionState::Hexdump { index, .. } => index.cmp(child_index),
                        super::PositionState::Hexstring(_, i) if i == child_index => std::cmp::Ordering::Less,
                        super::PositionState::Hexstring(_, i) => i.cmp(child_index),
                        super::PositionState::Ellipsis(_, i) if i == child_index => std::cmp::Ordering::Less,
                        super::PositionState::Ellipsis(_, i) => i.cmp(child_index),
                        super::PositionState::SummaryPreamble => std::cmp::Ordering::Less,
                        super::PositionState::SummaryOpener => std::cmp::Ordering::Less,
                        super::PositionState::SummaryLabel(i) if i == child_index => std::cmp::Ordering::Less,
                        super::PositionState::SummaryLabel(i) => i.cmp(child_index),
                        super::PositionState::SummarySeparator(i) if i == child_index => std::cmp::Ordering::Less,
                        super::PositionState::SummarySeparator(i) => i.cmp(child_index),
                        super::PositionState::SummaryCloser => std::cmp::Ordering::Greater,
                        super::PositionState::SummaryEpilogue => std::cmp::Ordering::Greater,
                        super::PositionState::SummaryValueBegin => std::cmp::Ordering::Less,
                        super::PositionState::SummaryValueEnd => std::cmp::Ordering::Greater,
                        super::PositionState::SummaryLeaf => return None,
                        super::PositionState::PostBlank => std::cmp::Ordering::Greater,
                        super::PositionState::End => std::cmp::Ordering::Greater,
                    })
                }
            }
        }
    }

    pub struct Item {
        stack: Option<sync::Arc<super::StackEntry>>,
        sod: StateOrDescent,
        node: sync::Arc<structure::Node>,
        logical_depth: usize,
    }

    impl Item {
        fn parent(&self) -> Option<Item> {
            self.stack.as_ref().map(|tse| Self::from(&**tse))
        }
    }
    
    impl From<&super::Position> for Item {
        fn from(t: &super::Position) -> Self {
            Self {
                stack: t.stack.clone(),
                sod: StateOrDescent::State(t.state.clone()),
                node: t.node.clone(),
                logical_depth: t.logical_depth
            }
        }
    }

    impl From<&super::StackEntry> for Item {
        fn from(t: &super::StackEntry) -> Self {
            Self {
                stack: t.stack.clone(),
                sod: StateOrDescent::Descent(t.descent.clone()),
                node: t.node.clone(),
                logical_depth: t.logical_depth
            }
        }
    }

    fn cmp_same_depth(a: &Item, b: &Item) -> Option<std::cmp::Ordering> {
        assert_eq!(a.logical_depth, b.logical_depth);
        
        if a.logical_depth == 0 {
            if sync::Arc::ptr_eq(&a.node, &b.node) {
                return a.sod.partial_cmp(&b.sod);
            } else {
                /* We were comparing items from different documents */
                return None;
            }
        }

        match cmp_same_depth(&a.parent().unwrap(), &b.parent().unwrap()) {
            Some(std::cmp::Ordering::Equal) => a.sod.partial_cmp(&b.sod),
            x => x
        }
    }
    
    pub fn partial_cmp(mut a: Item, mut b: Item) -> Option<std::cmp::Ordering> {
        let critical_depth = std::cmp::min(a.logical_depth, b.logical_depth);
        while a.logical_depth > critical_depth {
            a = a.parent().unwrap();
        }
        while b.logical_depth > critical_depth {
            b = b.parent().unwrap();
        }
        cmp_same_depth(&a, &b)
    }

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub enum StateGroup {
        /* comparable with all */
        Preamble,

        /* comparable with self, but not comparable with others */
        NormalContent,
        SummaryContent,
        SummaryLeaf,

        /* comparable with all */
        Postamble,
    }
    
    pub fn state_tuple(state: &super::PositionState) -> (StateGroup, usize, usize, addr::Address, usize) {
        match state {
            super::PositionState::PreBlank => (StateGroup::Preamble, 0, 0, addr::unit::NULL, 0),
            super::PositionState::Title => (StateGroup::Preamble, 1, 0, addr::unit::NULL, 0),
            super::PositionState::SummaryValueBegin => (StateGroup::Preamble, 2, 0, addr::unit::NULL, 0),
            
            super::PositionState::MetaContent(addr, index) => (StateGroup::NormalContent, 0, *index, *addr, 0),
            super::PositionState::Hexdump { extent, line_extent: _, index } => (StateGroup::NormalContent, 0, *index, extent.begin, 1),
            super::PositionState::Hexstring(extent, index) => (StateGroup::NormalContent, 0, *index, extent.begin, 1),
            super::PositionState::Ellipsis(extent, index) => (StateGroup::NormalContent, 0, *index, extent.begin, 1),
            super::PositionState::SummaryPreamble => (StateGroup::SummaryContent, 0, 0, addr::unit::NULL, 0),
            super::PositionState::SummaryOpener => (StateGroup::SummaryContent, 1, 0, addr::unit::NULL, 0),
            super::PositionState::SummaryLabel(x) => (StateGroup::SummaryContent, 2, 2*x+1, addr::unit::NULL, 0),
            super::PositionState::SummarySeparator(x) => (StateGroup::SummaryContent, 2, 2*x, addr::unit::NULL, 0),
            super::PositionState::SummaryCloser => (StateGroup::SummaryContent, 3, 0, addr::unit::NULL, 0),
            super::PositionState::SummaryEpilogue => (StateGroup::SummaryContent, 4, 0, addr::unit::NULL, 0),
            
            super::PositionState::SummaryLeaf => (StateGroup::SummaryLeaf, 0, 0, addr::unit::NULL, 0),
            
            super::PositionState::SummaryValueEnd => (StateGroup::Postamble, 0, 0, addr::unit::NULL, 0),
            super::PositionState::PostBlank => (StateGroup::Postamble, 1, 0, addr::unit::NULL, 0),
            super::PositionState::End => (StateGroup::Postamble, 2, 0, addr::unit::NULL, 0),
        }
    }
}

impl std::cmp::PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        cmp::partial_cmp(cmp::Item::from(self), cmp::Item::from(other))
    }
}

impl std::cmp::PartialOrd for PositionState {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let st1 = cmp::state_tuple(self);
        let st2 = cmp::state_tuple(other);

        match (st1.0, st2.0) {
            (cmp::StateGroup::Preamble, _) => {},
            (cmp::StateGroup::Postamble, _) => {},
            (_, cmp::StateGroup::Preamble) => {},
            (_, cmp::StateGroup::Postamble) => {},
            (x, y) if x == y => {},

            /* certain state groups are incomparable */
            _ => return None,
        };

        return Some(st1.cmp(&st2));
    }
}

impl std::cmp::PartialOrd for Descent {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Child(x), Self::Child(y)) => Some(x.cmp(y)),
            (Self::ChildSummary(x), Self::ChildSummary(y)) => Some(x.cmp(y)),
            (Self::MySummary, Self::MySummary) => Some(std::cmp::Ordering::Equal),
            _ => None,
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

    struct TokenDef<'a, 'input> {
        node: roxmltree::Node<'a, 'input>,
        node_name: String,
        depth: usize,
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

    fn inflate_token_tree<'a, 'input>(xml: roxmltree::Node<'a, 'input>, collection: &mut vec::Vec<TokenDef<'a, 'input>>, depth: usize) {
        for c in xml.children().filter(|c| c.is_element()) {
            if c.has_tag_name("indent") {
                inflate_token_tree(c, collection, depth + 1)
            } else {
                collection.push(TokenDef {
                    node_name: c.attribute("node").unwrap().to_string(),
                    node: c,
                    depth,
                })
            }
        }
    }

    fn inflate_extent(xml: &roxmltree::Node) -> addr::Extent {
        addr::Extent::parse(xml.attribute("extent").unwrap()).unwrap()
    }

    fn inflate_line_extent(xml: &roxmltree::Node) -> addr::Extent {
        addr::Extent::parse(xml.attribute("line").unwrap()).unwrap()
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

    impl<'a, 'input> TokenDef<'a, 'input> {
        fn into_token(self, lookup: &collections::HashMap<String, (addr::Address, structure::Path, sync::Arc<structure::Node>)>) -> token::Token {
            let lookup_result = lookup.get(&self.node_name).unwrap_or_else(|| panic!("expected a node named '{}'", self.node_name));

            let common = token::TokenCommon {
                node: lookup_result.2.clone(),
                node_path: lookup_result.1.clone(),
                node_addr: lookup_result.0,
                node_child_index: self.node.attribute("index").unwrap().parse().unwrap(),
                depth: self.depth,
            };

            match self.node.tag_name().name() {
                "null" => token::BlankLine { common, accepts_cursor: self.node.attribute("cursor").map_or(false, |b| b.eq("true")) }.into(),
                "open" => token::SummaryPunctuation { common, kind: token::PunctuationKind::OpenBracket }.into(),
                "comma" => token::SummaryPunctuation { common, kind: token::PunctuationKind::Comma }.into(),
                "close" => token::SummaryPunctuation { common, kind: token::PunctuationKind::CloseBracket }.into(),
                "title" => token::Title { common }.into(),
                "summlabel" => token::SummaryLabel { common }.into(),
                "preamble" => token::SummaryPreamble { common }.into(),
                "epilogue" => token::SummaryEpilogue { common }.into(),
                "hexdump" => token::Hexdump {
                    common,
                    extent: inflate_extent(&self.node),
                    line: inflate_line_extent(&self.node)
                }.into(),
                "hexstring" => token::Hexstring {
                    common,
                    extent: inflate_extent(&self.node)
                }.into(),
                "ellipsis" => token::Ellipsis {
                    common,
                    extent: inflate_extent(&self.node)
                }.into(),
                tn => panic!("invalid token def: '{}'", tn)
            }
        }
    }
}

impl AbstractPosition for Position {
    fn at_beginning(root: sync::Arc<structure::Node>) -> Self {
        Position::at_beginning(root)
    }

    fn at_path(root: sync::Arc<structure::Node>, path: &structure::Path, offset: addr::Address) -> Self {
        Position::at_path(root, path, offset)
    }

    fn port_change(&mut self, new_doc: &sync::Arc<document::Document>, change: &document::change::Change) {
        Position::port_change(self, &new_doc.root, change, &mut PortOptions::default());
    }
    
    fn hit_top(&self) -> bool {
        Position::hit_top(self)
    }
    
    fn hit_bottom(&self) -> bool {
        Position::hit_bottom(self)
    }
    
    fn gen_token(&self) -> TokenGenerationResult {
        Position::gen_token(self)
    }
    
    fn move_prev(&mut self) -> bool {
        Position::move_prev(self)
    }

    fn move_next(&mut self) -> bool {
        Position::move_next(self)
    }

    fn next_postincrement(&mut self) -> Option<token::Token> {
        Position::next_postincrement(self)
    }

    fn prev(&mut self) -> Option<token::Token> {
        Position::prev(self)
    }

    fn in_summary(&self) -> bool {
        Position::in_summary(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::iter;
    use std::vec;

    use crate::model::document;
    use crate::model::versioned::Versioned;
    
    struct DownwardPositionIterator(Position);
    struct UpwardPositionIterator(Position);

    impl iter::Iterator for DownwardPositionIterator {
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

    impl iter::Iterator for UpwardPositionIterator {
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
            &mut DownwardPositionIterator(Position::at_beginning(tc.structure.clone())));
    }

    fn test_backward(tc: &xml::Testcase) {
        itertools::assert_equal(
            tc.expected_tokens.iter().rev().map(|x| x.clone()),
            &mut UpwardPositionIterator(Position::at_end(&tc.structure)));
    }

    fn test_cmp(mut position: Position) {
        let mut prev = vec![position.clone()];
        while position.move_next() {
            for p in &prev {
                let ordering = p.partial_cmp(&position);
                if ordering != Some(std::cmp::Ordering::Less) {
                    panic!("comparing {:?} to {:?} resulted in incorrect ordering {:?}", p, position, ordering);
                }
            }
            prev.push(position.clone());
        }
    }

    #[test]
    fn simple() {
        let tc = parse_testcase(include_bytes!("stream_tests/simple.xml"));
        test_forward(&tc);
        test_backward(&tc);
    }

    #[test]
    fn simple_cmp() {
        let tc = parse_testcase(include_bytes!("stream_tests/simple.xml"));
        test_cmp(Position::at_beginning(tc.structure.clone()));
    }
    
    #[test]
    fn nesting() {
        let tc = parse_testcase(include_bytes!("stream_tests/nesting.xml"));
        test_forward(&tc);
        test_backward(&tc);
    }

    #[test]
    fn nesting_cmp() {
        let tc = parse_testcase(include_bytes!("stream_tests/nesting.xml"));
        test_cmp(Position::at_beginning(tc.structure.clone()));
    }
    
    #[test]
    fn formatting() {
        let tc = parse_testcase(include_bytes!("stream_tests/formatting.xml"));
        test_forward(&tc);
        test_backward(&tc);
    }

    #[test]
    fn content_display() {
        let tc = parse_testcase(include_bytes!("stream_tests/content_display.xml"));
        test_forward(&tc);
        test_backward(&tc);
    }

    #[test]
    fn summary() {
        let tc = parse_testcase(include_bytes!("stream_tests/summary.xml"));
        test_forward(&tc);
        test_backward(&tc);
    }

    #[test]
    fn summary_cmp() {
        let tc = parse_testcase(include_bytes!("stream_tests/summary.xml"));
        test_cmp(Position::at_beginning(tc.structure.clone()));
    }
    
    fn seek_to_token(position: &mut Position, target: &token::Token) {
        while match position.gen_token() {
            TokenGenerationResult::Ok(token) => &token != target,
            TokenGenerationResult::Skip => true,
            TokenGenerationResult::Boundary => panic!("couldn't find token"),
        } {
            if !position.move_next() {
                panic!("hit end of token stream");
            }
        }        
    }

    fn peek(position: &mut Position) -> token::Token {
        loop {
            match position.gen_token() {
                TokenGenerationResult::Ok(token) => return token,
                TokenGenerationResult::Skip => assert!(position.move_next()),
                TokenGenerationResult::Boundary => panic!("couldn't find token"),
            }
        }        
    }

    fn assert_positions_eq(a: &Position, b: &Position) {
        assert_eq!(a.state, b.state);
        assert_eq!(a.apparent_depth, b.apparent_depth);
        assert_eq!(a.logical_depth, b.logical_depth);
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
            assert_eq!(stack_item_a.apparent_depth, stack_item_b.apparent_depth);
            assert_eq!(stack_item_a.logical_depth, stack_item_b.logical_depth);
            assert!(sync::Arc::ptr_eq(&stack_item_a.node, &stack_item_b.node));
            assert_eq!(stack_item_a.node_addr, stack_item_b.node_addr);
            
            stack_walker_a = &stack_item_a.stack;
            stack_walker_b = &stack_item_b.stack;
        };
    }

    #[test]
    fn at_path_on_summary() {
        let root = structure::Node::builder()
            .name("root")
            .size(0x40)
            .child(0x10, |b| b
                   .name("child0")
                   .size(0x20))
            .child(0x14, |b| b
                   .name("child1")
                   .size(0x50)
                   .children_display(structure::ChildrenDisplay::Summary)
                   .child(0x0, |b| b
                          .name("child1.0")
                          .size(0x18))
                   .child(0x20, |b| b
                          .name("child1.1")
                          .size(0x18))
                   .child(0x34, |b| b
                          .name("child1.2")
                          .size(0x18))
                   .child(0x48, |b| b
                          .name("child1.3")
                          .size(0x1c)))
            .child(0x60, |b| b
                   .name("child2")
                   .size(0x4))
            .build();

        let tok = Position::at_path(root.clone(), &vec![1, 1], 0x10.into());

        assert_eq!(tok, Position {
            stack: Some(sync::Arc::new(StackEntry {
                stack: Some(sync::Arc::new(StackEntry {
                    stack: Some(sync::Arc::new(StackEntry {
                        stack: None,
                        descent: Descent::Child(1),
                        apparent_depth: 0,
                        logical_depth: 0,
                        node: root.clone(),
                        node_addr: 0x0.into(),
                    })),
                    descent: Descent::MySummary,
                    apparent_depth: 1,
                    logical_depth: 1,
                    node: root.children[1].node.clone(),
                    node_addr: 0x14.into(),
                })),
                descent: Descent::ChildSummary(1),
                apparent_depth: 1,
                logical_depth: 2, /* ! */
                node: root.children[1].node.clone(),
                node_addr: 0x14.into(),
            })),
            state: PositionState::SummaryLeaf,
            apparent_depth: 2,
            logical_depth: 3,
            node: root.children[1].node.children[1].node.clone(),
            node_addr: 0x34.into(),
        });
    }
    
    fn assert_port_functionality(old_doc: &document::Document, new_doc: &document::Document, records: &[(token::Token, token::Token, PortOptions, PortOptions)]) {
        let mut positions: Vec<(Position, &token::Token, &token::Token, &PortOptions, &PortOptions)> = records.iter().map(
            |(before_token, after_token, before_options, after_options)| (
                Position::at_beginning(old_doc.root.clone()),
                before_token,
                after_token,
                before_options,
                after_options)
        ).collect();

        for (position, before_token, _after_token, _, _) in positions.iter_mut() {
            seek_to_token(position, before_token);
        }

        for (position, _before_token, after_token, options_before, options_after) in positions.iter_mut() {
            println!("position before port: {:#?}", position);
            let mut options = options_before.clone();
            new_doc.changes_since_ref(old_doc, &mut |doc, change| position.port_change(&doc.root, change, &mut options));
            println!("position after port: {:#?}", position);
            
            assert_eq!(&peek(position), *after_token);
            assert_eq!(&options, *options_after);

            /* Check that the ported position is the same as if we had created a new position and seeked it (if only we knew where to seek it to...), i.e. its internal state isn't corrupted in a way that doesn't happen during normal position movement. */
            let mut clean_position = Position::at_beginning(new_doc.root.clone());
            seek_to_token(&mut clean_position, after_token);
            assert_positions_eq(&position, &clean_position);
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
                   .child(0x34, |b| b
                          .name("child1.2")
                          .size(0x18))
                   .child(0x48, |b| b
                          .name("child1.3")
                          .size(0x1c)))
            .child(0x60, |b| b
                   .name("child2")
                   .size(0x4))
            .build();
 
        let old_doc = document::Builder::new(root).build();
        let mut new_doc = old_doc.clone();

        /* Delete child1.1 and child1.2. */
        new_doc.change_for_debug(old_doc.delete_range(structure::SiblingRange::new(vec![1], 1, 2))).unwrap();
        
        let (o_child_1_2, o_child_1_2_addr) = old_doc.lookup_node(&vec![1, 2]);
        let (o_child_1_3, o_child_1_3_addr) = old_doc.lookup_node(&vec![1, 3]);
        let (n_child_1,   n_child_1_addr)   = new_doc.lookup_node(&vec![1]);

        assert_port_functionality(&old_doc, &new_doc, &[
            (
                token::Token::Hexdump(token::Hexdump {
                    common: token::TokenCommon {
                        node: o_child_1_2.clone(),
                        node_path: vec![1, 2],
                        node_addr: o_child_1_2_addr,
                        node_child_index: 0,
                        depth: 3,
                    },
                    extent: addr::Extent::sized_u64(0x10, 0x8),
                    line: addr::Extent::sized_u64(0x10, 0x10),
                }),
                token::Token::Hexdump(token::Hexdump {
                    common: token::TokenCommon {
                        node: n_child_1.clone(),
                        node_path: vec![1],
                        node_addr: n_child_1_addr,
                        node_child_index: 1,
                        depth: 2,
                    },
                    extent: addr::Extent::sized_u64(0x40, 0x8),
                    line: addr::Extent::sized_u64(0x40, 0x10)
                }),
                PortOptionsBuilder::new().additional_offset(0x4).build(), /* Asking about offset 0x14 into old child1.2 */
                PortOptionsBuilder::new().additional_offset(0x8).build(), /* Becomes offset 0x48 in new child1 */
            ),
            (
                token::Token::Hexdump(token::Hexdump {
                    common: token::TokenCommon {
                        node: o_child_1_3.clone(),
                        node_path: vec![1, 3],
                        node_addr: o_child_1_3_addr,
                        node_child_index: 0,
                        depth: 3,
                    },
                    extent: addr::Extent::sized_u64(0x10, 0xc),
                    line: addr::Extent::sized_u64(0x10, 0x10)
                }),
                token::Token::Hexdump(token::Hexdump {
                    common: token::TokenCommon {
                        /* child1.3 shouldn't be affected, so use the old node and addr to assert that */
                        node: o_child_1_3.clone(),
                        node_path: vec![1, 3],
                        node_addr: o_child_1_3_addr,
                        node_child_index: 0,
                        depth: 3,
                    },
                    extent: addr::Extent::sized_u64(0x10, 0xc),
                    line: addr::Extent::sized_u64(0x10, 0x10),
                }),
                PortOptionsBuilder::new().additional_offset(0x4).build(),
                PortOptionsBuilder::new().additional_offset(0x4).build(),
            ),
        ]);
    }

    #[test]
    fn port_insert_node_simple() {
        let root = structure::Node::builder()
            .name("root")
            .size(0x400)
            .build();
 
        let old_doc = document::Builder::new(root.clone()).build();
        let mut new_doc = old_doc.clone();

        let node = sync::Arc::new(structure::Node {
            props: structure::Properties {
                name: "child".to_string(),
                title_display: structure::TitleDisplay::Minor,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::Hexdump {
                    line_pitch: addr::Size::from(16),
                    gutter_pitch: addr::Size::from(8),
                },
                locked: false,
            },
            children: vec::Vec::new(),
            size: addr::Size::from(0x30),
        });
        
        new_doc.change_for_debug(old_doc.insert_node(vec![], 0, structure::Childhood::new(node.clone(), 0x12.into()))).unwrap();

        let new_root = new_doc.root.clone();
        
        assert_port_functionality(&old_doc, &new_doc, &[
            /* offset 0x4 */
            (
                token::Token::Hexdump(token::Hexdump {
                    common: token::TokenCommon {
                        node: root.clone(),
                        node_path: vec![],
                        node_addr: addr::unit::NULL,
                        node_child_index: 0,
                        depth: 1,
                    },
                    extent: addr::Extent::sized_u64(0x0, 0x10),
                    line: addr::Extent::sized_u64(0x0, 0x10),
                }),
                token::Token::Hexdump(token::Hexdump {
                    common: token::TokenCommon {
                        node: new_root.clone(),
                        node_path: vec![],
                        node_addr: addr::unit::NULL,
                        node_child_index: 0,
                        depth: 1,
                    },
                    extent: addr::Extent::sized_u64(0x0, 0x10),
                    line: addr::Extent::sized_u64(0x0, 0x10),
                }),
                PortOptionsBuilder::new().additional_offset(0x4).build(),
                PortOptionsBuilder::new().additional_offset(0x4).build(),
            ),
            /* offset 0x11 */
            (
                token::Token::Hexdump(token::Hexdump {
                    common: token::TokenCommon {
                        node: root.clone(),
                        node_path: vec![],
                        node_addr: addr::unit::NULL,
                        node_child_index: 0,
                        depth: 1,
                    },
                    extent: addr::Extent::sized_u64(0x10, 0x10),
                    line: addr::Extent::sized_u64(0x10, 0x10),
                }),
                token::Token::Hexdump(token::Hexdump {
                    common: token::TokenCommon {
                        node: new_root.clone(),
                        node_path: vec![],
                        node_addr: addr::unit::NULL,
                        node_child_index: 0,
                        depth: 1,
                    },
                    extent: addr::Extent::sized_u64(0x10, 0x2),
                    line: addr::Extent::sized_u64(0x10, 0x10),
                }),
                PortOptionsBuilder::new().additional_offset(0x1).build(),
                PortOptionsBuilder::new().additional_offset(0x1).build(),
            ),
        ]);
    }

    #[test]
    fn port_nest_nodes_simple() {
        let root = structure::Node::builder()
            .name("root")
            .size(0x400)
            .child(0x38, |b| b
                   .name("child0")
                   .size(0x20))
            .build();
 
        let old_doc = document::Builder::new(root.clone()).build();
        let mut new_doc = old_doc.clone();

        let props = structure::Properties {
            name: "child".to_string(),
            title_display: structure::TitleDisplay::Minor,
            children_display: structure::ChildrenDisplay::Full,
            content_display: structure::ContentDisplay::Hexdump {
                line_pitch: addr::Size::from(16),
                gutter_pitch: addr::Size::from(8),
            },
            locked: false,
        };
        
        new_doc.change_for_debug(old_doc.nest(structure::SiblingRange {
            parent: vec![],
            first: 0,
            last: 0,
        }, addr::Extent::sized_u64(0x24, 0x58), props)).unwrap();

        let new_root = new_doc.root.clone();
        let new_child = new_doc.root.children[0].node.clone();
        
        assert_port_functionality(&old_doc, &new_doc, &[
            /* offset 0x4 */
            (
                token::Token::Hexdump(token::Hexdump {
                    common: token::TokenCommon {
                        node: root.clone(),
                        node_path: vec![],
                        node_addr: addr::unit::NULL,
                        node_child_index: 0,
                        depth: 1,
                    },
                    extent: addr::Extent::sized_u64(0x0, 0x10),
                    line: addr::Extent::sized_u64(0x0, 0x10),
                }),
                token::Token::Hexdump(token::Hexdump {
                    common: token::TokenCommon {
                        node: new_root.clone(),
                        node_path: vec![],
                        node_addr: addr::unit::NULL,
                        node_child_index: 0,
                        depth: 1,
                    },
                    extent: addr::Extent::sized_u64(0x0, 0x10),
                    line: addr::Extent::sized_u64(0x0, 0x10),
                }),
                PortOptionsBuilder::new().additional_offset(0x4).build(),
                PortOptionsBuilder::new().additional_offset(0x4).build(),
            ),
            /* offset 0x28 */
            (
                token::Token::Hexdump(token::Hexdump {
                    common: token::TokenCommon {
                        node: root.clone(),
                        node_path: vec![],
                        node_addr: addr::unit::NULL,
                        node_child_index: 0,
                        depth: 1,
                    },
                    extent: addr::Extent::sized_u64(0x20, 0x10),
                    line: addr::Extent::sized_u64(0x20, 0x10),
                }),
                token::Token::Hexdump(token::Hexdump {
                    common: token::TokenCommon {
                        node: new_child.clone(),
                        node_path: vec![0],
                        node_addr: 0x24.into(),
                        node_child_index: 0,
                        depth: 2,
                    },
                    extent: addr::Extent::sized_u64(0x0, 0x10),
                    line: addr::Extent::sized_u64(0x0, 0x10),
                }),
                PortOptionsBuilder::new().additional_offset(0x8).build(),
                PortOptionsBuilder::new().additional_offset(0x4).build(),
            ),
        ]);
    }

    #[test]
    fn port_root_only_summary() {
        let root = structure::Node::builder()
            .name("root")
            .size(0x400)
            .build();
 
        let old_doc = document::Builder::new(root.clone()).build();
        let mut new_doc = old_doc.clone();

        let props = structure::Properties {
            name: "root".to_string(),
            children_display: structure::ChildrenDisplay::Summary,
            ..Default::default()
        };
        
        new_doc.change_for_debug(old_doc.alter_node(vec![], props)).unwrap();

        let new_root = new_doc.root.clone();
        
        assert_port_functionality(&old_doc, &new_doc, &[
            /* title */
            (
                token::Token::Title(token::Title {
                    common: token::TokenCommon {
                        node: root.clone(),
                        node_path: vec![],
                        node_addr: addr::unit::NULL,
                        node_child_index: 0,
                        depth: 0,
                    },
                }),
                token::Token::Title(token::Title {
                    common: token::TokenCommon {
                        node: new_root.clone(),
                        node_path: vec![],
                        node_addr: addr::unit::NULL,
                        node_child_index: 0,
                        depth: 0,
                    },
                }),
                PortOptionsBuilder::new().build(),
                PortOptionsBuilder::new().build(),
            ),
            /* offset 0x0 */
            (
                token::Token::Hexdump(token::Hexdump {
                    common: token::TokenCommon {
                        node: root.clone(),
                        node_path: vec![],
                        node_addr: addr::unit::NULL,
                        node_child_index: 0,
                        depth: 1,
                    },
                    extent: addr::Extent::sized_u64(0x0, 0x10),
                    line: addr::Extent::sized_u64(0x0, 0x10),
                }),
                token::Token::SummaryPunctuation(token::SummaryPunctuation {
                    common: token::TokenCommon {
                        node: new_root.clone(),
                        node_path: vec![],
                        node_addr: addr::unit::NULL,
                        node_child_index: 0,
                        depth: 0,
                    },
                    kind: token::PunctuationKind::CloseBracket,
                }),
                PortOptionsBuilder::new().additional_offset(0x0).build(),
                PortOptionsBuilder::new().additional_offset(0x0).build(),
            ),
        ]);
    }
}
