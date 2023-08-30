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

mod normal;
mod summary;

#[derive(Clone, Debug, PartialEq, Eq)]
enum Mode {
    Normal(normal::NormalState),
    SummaryBranch(summary::BranchState),
    SummaryLeaf(summary::LeafState),
    Invalid,
}

#[derive(Clone, Debug)]
struct CommonFields {
    node: sync::Arc<structure::Node>,
    node_absolute_addr: addr::Address,
    actual_depth: usize,
    visual_depth: usize,
}

#[derive(Debug)]
pub enum TokenGenerationResult {
    Ok(token::Token),
    Skip,
    Boundary,
}

#[derive(Clone, PartialEq, Eq, Debug)]
enum StackEntryClass {
    Normal(normal::StackEntry),
    Branch(summary::BranchStackEntry),
}

#[derive(Clone)]
pub struct StackEntry {
    parent: Option<sync::Arc<StackEntry>>,
    class: StackEntryClass,
    common: CommonFields,
}

#[derive(Clone)]
pub struct Tokenizer {
    stack: Option<sync::Arc<StackEntry>>,
    mode: Mode,
    common: CommonFields
}

enum PopDirection {
    Prev, Next
}

#[derive(PartialEq, Eq)]
enum MovementResult {
    Ok,
    HitBoundary
}

impl Tokenizer {
    /// Creates a new tokenizer seeked to the root of the structure hierarchy and the beginning of the token stream.
    pub fn at_beginning(root: sync::Arc<structure::Node>) -> Tokenizer {
        Tokenizer {
            stack: None,
            mode: Mode::Normal(normal::State::PreBlank),
            common: CommonFields {
                node: root,
                node_absolute_addr: addr::unit::NULL,
                actual_depth: 0,
                visual_depth: 0,
            }
        }
    }

    /// Creates a new tokenizer positioned at a specific offset within the node at the given path.
    pub fn at_path(root: sync::Arc<structure::Node>, path: &structure::Path, offset: addr::Address) -> Tokenizer {
        todo!();
    }
    
    fn seek_in_node_to_offset(&mut self, offset: addr::Address) {
        todo!();

        /*
        let index = self.node.children.partition_point(|ch| ch.offset < offset);
        
        self.state = TokenizerState::MetaContent(self.get_line_begin(offset, index), index);

        while match self.gen_token() {
            TokenGenerationResult::Skip => true,
            _ => false
        } {
            self.move_next();
    }
        */
    }
    
    /// Creates a new tokenizer seeked to the end of the token stream.
    pub fn at_end(root: &sync::Arc<structure::Node>) -> Tokenizer {
        Tokenizer {
            stack: None,
            mode: Mode::Normal(normal::State::End),
            common: CommonFields {
                node: root.clone(),
                node_addr: addr::unit::NULL,
                actual_depth: 0,
                visual_depth: 0,
            },
        }
    }

    /// Returns the token at the current position, or Skip if the current position in the stream doesn't generate a token.
    pub fn gen_token(&self) -> TokenGenerationResult {
        match self.mode {
            Mode::Normal(ns) => ns.gen_token(&self.common),
            Mode::SummaryBranch(sbs) => sbs.gen_token(&self.common),
            Mode::SummaryLeaf(sls) => sls.gen_token(&self.common),
        }
    }
    
    /// Moves one position backwards in the stream.
    fn move_prev(&mut self) -> MovementResult {
        match mem::replace(&mut self.mode, Mode::Invalid) {
            Mode::Normal(ns) => ns.move_prev(self),
            Mode::SummaryBranch(sbs) => sbs.move_prev(self),
            Mode::SummaryLeaf(sls) => sls.move_prev(self)
        }
    }

    /// Moves one position forwards in the stream.
    fn move_next(&mut self) -> MovementResult {
        match mem::replace(&mut self.mode, Mode::Invalid) {
            Mode::Normal(ns) => ns.move_next(self),
            Mode::SummaryBranch(sbs) => sbs.move_next(self),
            Mode::SummaryLeaf(sls) => sls.move_next(self)
        }
    }
    
    pub fn prev(&mut self) -> Option<token::Token> {
        while self.move_prev() == MovementResult::Ok {
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
        while self.move_next() == MovementResult::Ok {
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
            self.move_next() == MovementResult::Ok
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
            TokenizerState::PostBlank => self.node.size.to_addr(),
            TokenizerState::End => self.node.size.to_addr(),
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
                let accepts_cursor = c.attribute("cursor").map_or(false, |b| b.eq("true"));
                
                collection.push(TokenDef {
                    class: match c.tag_name().name() {
                        "null" => token::TokenClass::Punctuation { class: token::PunctuationClass::Empty, accepts_cursor },
                        "open" => token::TokenClass::Punctuation { class: token::PunctuationClass::OpenBracket, accepts_cursor },
                        "comma" => token::TokenClass::Punctuation { class: token::PunctuationClass::Comma, accepts_cursor },
                        "close" => token::TokenClass::Punctuation { class: token::PunctuationClass::CloseBracket, accepts_cursor },
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
                content_display: structure::ContentDisplay::Hexdump {
                    line_pitch: 16.into(),
                    gutter_pitch: 8.into(),
                },
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
                content_display: structure::ContentDisplay::Hexdump {
                    line_pitch: 16.into(),
                    gutter_pitch: 8.into(),
                },
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
                class: token::TokenClass::Punctuation { class: token::PunctuationClass::Empty, accepts_cursor: false },
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
                class: token::TokenClass::Punctuation { class: token::PunctuationClass::Empty, accepts_cursor: false },
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
                class: token::TokenClass::Punctuation { class: token::PunctuationClass::Empty, accepts_cursor: true },
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
                class: token::TokenClass::Punctuation { class: token::PunctuationClass::Empty, accepts_cursor: true },
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
