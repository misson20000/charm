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
                TokenizerState::PreBlank | TokenizerState::Title | TokenizerState::SummaryOpener | TokenizerState::SummaryEntryBegin | TokenizerState::SummaryLabel => IntermediatePortState::NormalContent(Some(destructured_childhood.offset), *destructured_child_index),
                
                TokenizerState::MetaContent(offset, index) => IntermediatePortState::NormalContent(Some(destructured_childhood.offset + offset.to_size()), destructured_child_index + *index),
                TokenizerState::Hexdump(extent, index) => IntermediatePortState::NormalContent(Some(destructured_childhood.offset + extent.begin.to_size()), destructured_child_index + *index),
                TokenizerState::Hexstring(extent, index) => IntermediatePortState::NormalContent(Some(destructured_childhood.offset + extent.begin.to_size()), destructured_child_index + *index),
                TokenizerState::SummaryLeaf => IntermediatePortState::NormalContent(Some(destructured_childhood.offset), *destructured_child_index),

                TokenizerState::SummarySeparator(i) => IntermediatePortState::NormalContent(None, destructured_child_index + *i),

                TokenizerState::SummaryEntryEnd | TokenizerState::SummaryCloser | TokenizerState::PostBlank | TokenizerState::End => IntermediatePortState::NormalContent(Some(destructured_childhood.end()), destructured_child_index + 1),
            },
            // TODO: try harder here
            PortStackMode::Destructuring { destructured_child_index, summary: true, .. } => IntermediatePortState::SummarySeparator(*destructured_child_index),
            
            PortStackMode::Normal => match &self.state {
                TokenizerState::PreBlank =>
                    IntermediatePortState::Finished(TokenizerState::PreBlank),
                TokenizerState::Title =>
                    IntermediatePortState::Finished(TokenizerState::Title),
                
                TokenizerState::MetaContent(offset, index) => IntermediatePortState::NormalContent(Some(*offset), *index),
                TokenizerState::Hexdump(extent, index) => IntermediatePortState::NormalContent(Some(extent.begin), *index),
                TokenizerState::Hexstring(extent, index) => IntermediatePortState::NormalContent(Some(extent.begin), *index),

                TokenizerState::SummaryOpener => IntermediatePortState::NormalContent(Some(addr::unit::NULL), 0),
                TokenizerState::SummarySeparator(i) => IntermediatePortState::NormalContent(None, *i),
                TokenizerState::SummaryCloser => IntermediatePortState::Finished(TokenizerState::End),
                
                TokenizerState::SummaryEntryBegin => IntermediatePortState::Finished(TokenizerState::PreBlank),
                TokenizerState::SummaryLabel => IntermediatePortState::Finished(TokenizerState::Title),
                TokenizerState::SummaryLeaf => IntermediatePortState::NormalContent(Some(addr::unit::NULL), 0),
                TokenizerState::SummaryEntryEnd => IntermediatePortState::Finished(TokenizerState::End),

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
                TokenizerState::SummarySeparator(i) => IntermediatePortState::SummarySeparator(*i),
                TokenizerState::SummaryCloser => IntermediatePortState::Finished(TokenizerState::SummaryCloser),
                
                TokenizerState::SummaryValueBegin => IntermediatePortState::Finished(TokenizerState::SummaryValueBegin),
                TokenizerState::SummaryLabel(i) => IntermediatePortState::SummaryLabel(*i),
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
