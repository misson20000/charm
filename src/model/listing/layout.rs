//! This module includes the logic that converts from a token stream
//! to lines that would be displayed in a window.

use std::collections;
use std::fmt;
use std::iter;
use std::sync;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::token;
use crate::model::versioned::Versioned;
use crate::logic::tokenizer;
use crate::util;

#[derive(Clone)]
pub enum LineType {
    Empty,
    Blank(token::Token),
    Title(token::Token),
    Hexdump {
        title: Option<token::Token>,
        node: sync::Arc<structure::Node>,
        line_extent: addr::Extent,
        tokens: collections::VecDeque<token::Token>
    },
    Hexstring {
        title: Option<token::Token>,
        token: token::Token,
    },
    Summary {
        title: Option<token::Token>,
        tokens: collections::VecDeque<token::Token>
    },
}

enum LinePushResult {
    Accepted,
    Completed,
    Rejected,
    BadPosition
}

#[derive(Clone)]
pub struct Line {
    ty: LineType,
}

pub trait LineView {
    type TokenIterator: iter::DoubleEndedIterator<Item = token::Token>;
    type BorrowingTokenIterator<'a>: iter::Iterator<Item = &'a token::Token> where Self: 'a;
    
    fn from_line(line: Line) -> Self;
    fn iter_tokens(&self) -> Self::BorrowingTokenIterator<'_>;
    fn to_tokens(self) -> Self::TokenIterator;
}

/* This lets us swap out a simpler implementation for testing to help narrow down
 * whether bugs are in Window logic or Tokenizer logic. */
pub trait WindowTokenizer: Clone {
    fn at_beginning(root: sync::Arc<structure::Node>) -> Self;
    fn at_path(root: sync::Arc<structure::Node>, path: &structure::Path, offset: addr::Address) -> Self;
    fn port_change(&mut self, new_doc: &sync::Arc<document::Document>, change: &document::change::Change);
    fn hit_top(&self) -> bool;
    fn hit_bottom(&self) -> bool;
    fn gen_token(&self) -> tokenizer::TokenGenerationResult;
    fn move_prev(&mut self) -> bool;
    fn move_next(&mut self) -> bool;
    fn next_postincrement(&mut self) -> Option<token::Token>;
    fn prev(&mut self) -> Option<token::Token>;
}

/// A listing window with a fixed height. Useful for scrolling by lines.
/// It is up to the user to make sure that this gets properly notified with structure invalidation events.
#[derive(Clone)]
pub struct Window<LV: LineView, Tokenizer: WindowTokenizer = tokenizer::Tokenizer> {
    current_document: sync::Arc<document::Document>,
    top: Tokenizer,
    bottom: Tokenizer,
    
    pub line_views: collections::VecDeque<LV>,
    pub window_height: usize,
    
    pub wants_update: bool,
}

impl<LV: LineView, Tokenizer: WindowTokenizer> Window<LV, Tokenizer> {
    pub fn new(doc: sync::Arc<document::Document>) -> Window<LV, Tokenizer> {
        Window {
            top: Tokenizer::at_beginning(doc.root.clone()),
            bottom: Tokenizer::at_beginning(doc.root.clone()),
            
            current_document: doc,
            
            line_views: std::collections::VecDeque::<LV>::new(),
            window_height: 0,
            
            wants_update: false,
        }
    }

    /// Moves the top of the window to the specified address. Returns amount
    /// window was adjusted upwards by due to hitting the bottom of the address space.
    pub fn seek(&mut self, document: sync::Arc<document::Document>, path: &structure::Path, offset: addr::Address) -> usize {
        self.current_document = document;
        let root = self.current_document.root.clone();
        self.repopulate_window(move |tok, _| *tok = Tokenizer::at_path(root, path, offset))
    }

    fn repopulate_window<F>(&mut self, tokenizer_provider: F) -> usize where
        F: FnOnce(&mut Tokenizer, &mut sync::Arc<document::Document>) {
        tokenizer_provider(&mut self.top, &mut self.current_document);
        self.bottom = self.top.clone();
        self.line_views.clear();
        
        let mut offset = 0;
        
        while self.line_views.len() < self.window_height {
            if self.bottom.hit_bottom() {
                if self.top.hit_top() {
                    /* entire document is too small to fit in window. */
                    break;
                }
                self.grow_top();
                offset+= 1;
            } else {
                self.grow_bottom();
            }
        }

        self.wants_update = true;

        offset
    }

    fn grow_top(&mut self) {
        if self.top.hit_top() {
            return;
        }

        let mut line = Line::new();

        loop {
            if !self.top.move_prev() {
                break;
            }

            if match line.push_front(match self.top.gen_token() {
                tokenizer::TokenGenerationResult::Ok(token) => token,
                tokenizer::TokenGenerationResult::Skip => continue,
                tokenizer::TokenGenerationResult::Boundary => break,
            }) {
                LinePushResult::Accepted => continue,
                LinePushResult::Completed => break,
                LinePushResult::Rejected => true,
                LinePushResult::BadPosition => false,
            } {
                assert!(self.top.move_next());
                break;
            }
        }

        if line.is_empty() {
            assert!(self.top.hit_top());
            return;
        }
        
        self.line_views.push_front(LV::from_line(line));
    }

    fn grow_bottom(&mut self) {
        if self.bottom.hit_bottom() {
            return;
        }

        let mut line = Line::new();

        loop {
            match line.push_back(match self.bottom.gen_token() {
                tokenizer::TokenGenerationResult::Ok(token) => token,
                tokenizer::TokenGenerationResult::Skip => if self.bottom.move_next() { continue } else { break },
                tokenizer::TokenGenerationResult::Boundary => break,
            }) {
                LinePushResult::Accepted => self.bottom.move_next(),
                LinePushResult::Completed => { self.bottom.move_next(); break },
                LinePushResult::Rejected => break,
                LinePushResult::BadPosition => self.bottom.move_next(),
            };
        }
        
        if line.is_empty() {
            assert!(self.bottom.hit_bottom());
            return;
        }
        
        self.line_views.push_back(LV::from_line(line));
    }

    fn shrink_top(&mut self) {
        let line = self.line_views.pop_front().unwrap();

        for token in line.to_tokens() {
            assert_eq!(token, self.top.next_postincrement().unwrap());
        }
    }

    fn shrink_bottom(&mut self) {
        let line = self.line_views.pop_back().unwrap();

        for token in line.to_tokens().rev() {
            assert_eq!(token, self.bottom.prev().unwrap());
        }
    }
    
    /// Scrolls the window upwards by one line. Returns false if the
    /// beginning of the token stream is hit.
    pub fn scroll_up(&mut self) -> bool {
        if self.top.hit_top() {
            return false;
        }
        
        self.grow_top();
        self.shrink_bottom();

        self.wants_update = true;

        true
    }

    /// Scrolls the window downwards by one line. Returns false if the
    /// end of the token stream is hit.
    pub fn scroll_down(&mut self) -> bool {
        if self.bottom.hit_bottom() {
            return false;
        }
        
        self.grow_bottom();
        self.shrink_top();

        self.wants_update = true;

        true
    }

    /// Changes the size of the window.
    pub fn resize(&mut self, size: usize) {
        self.window_height = size;

        while self.line_views.len() > self.window_height {
            self.shrink_bottom();
        }
        while self.line_views.len() < self.window_height {
            if self.bottom.hit_bottom() {
                if self.top.hit_top() {
                    break;
                } else {
                    self.grow_top();
                }
            } else {
                self.grow_bottom();
            }
        }

        self.wants_update = true;
    }

    pub fn get_window_height(&self) -> usize {
        self.window_height
    }

    pub fn get_bottom_hit_end(&self) -> bool {
        self.bottom.hit_bottom()
    }

    /* state bookkeeping */

    pub fn update(&mut self, document: &sync::Arc<document::Document>) {
        if self.current_document.is_outdated(document) {
            self.repopulate_window(|tok, current_doc| {
                document.changes_since(&current_doc.clone(), &mut |new_doc, change| {
                    tok.port_change(new_doc, change);
                    *current_doc = new_doc.clone()
                });
            });
        }
    }
}

impl Line {
    fn new() -> Self {
        Line {
            ty: LineType::Empty
        }
    }

    /// Returns true on success
    fn push_front(&mut self, token: token::Token) -> LinePushResult {
        let (new_ty, result) = match (std::mem::replace(&mut self.ty, LineType::Empty), &token.class) {
            /* A line with an Empty punctuation token on it can't have anything else on it. */
            (LineType::Blank(tok), _) => (LineType::Blank(tok), LinePushResult::Rejected),

            /* BlankLine punctuation tokens can go onto Empty lines to convert them to Blank lines, but nothing else. */
            (LineType::Empty, token::TokenClass::BlankLine { .. }) => (LineType::Blank(token), LinePushResult::Completed),
            (lt, token::TokenClass::BlankLine { .. }) => (lt, LinePushResult::Rejected),

            /* A title can end a line. */
            (LineType::Empty, token::TokenClass::Title) => (LineType::Title(token), LinePushResult::Accepted),

            /* A hexdump token can end a line. */
            (LineType::Empty, token::TokenClass::Hexdump { line, .. }) => (LineType::Hexdump {
                title: None,
                node: token.node.clone(),
                line_extent: *line,
                tokens: collections::VecDeque::from([token])
            }, LinePushResult::Accepted),

            /* A title token can occur on the same line as a hexdump if the title is inline and there isn't already a title. */
            (LineType::Hexdump {
                title: None,
                node,
                line_extent,
                tokens
            }, token::TokenClass::Title)
                if sync::Arc::ptr_eq(&token.node, &node)
                && token.node.props.title_display.is_inline()
                => (LineType::Hexdump {
                    title: Some(token),
                    node,
                    line_extent,
                    tokens
                }, LinePushResult::Accepted),

            /* Multiple hexdump tokens can coexist on a line under certain conditions. */
            (LineType::Hexdump { title, node: line_node, line_extent, mut tokens },
             token::TokenClass::Hexdump { extent: token_extent, line: token_line_extent })
                if sync::Arc::ptr_eq(&line_node, &token.node)
                && *token_line_extent == line_extent
                => {
                    /* Must be monotonic and non-overlapping. */
                    let result = if match tokens.front() {
                        Some(token::Token { class: token::TokenClass::Hexdump { extent, .. }, .. }) => extent,
                        Some(_) => panic!("all tokens in LineType::Hexdump.tokens should have TokenClass::Hexdump"),
                        None => panic!("a LineType::Hexdump shouldn't be able to exist without at least one token"),
                    }.begin < token_extent.end {
                        // TODO: log properly
                        println!("Attempted to add a token to a LineType::Hexdump that would've broken monotonicity. This shouldn't really happen.");
                        LinePushResult::Rejected
                    } else {
                        tokens.push_front(token);
                        LinePushResult::Rejected
                    };

                    (LineType::Hexdump {
                        title,
                        node: line_node,
                        line_extent,
                        tokens
                    }, result)
                },

            /* A hexstring token can end a line */
            (LineType::Empty, token::TokenClass::Hexstring(_)) => (LineType::Hexstring {
                title: None,
                token
            }, LinePushResult::Completed),

            /* A title token can occur on the same line as a hexstring if the title is inline and there isn't already a title. */
            (LineType::Hexstring { title: None, token: hexstring_token }, token::TokenClass::Title)
                if sync::Arc::ptr_eq(&token.node, &hexstring_token.node)
                && token.node.props.title_display.is_inline()
                => (LineType::Hexstring {
                    title: Some(token),
                    token: hexstring_token,
                }, LinePushResult::Accepted),

            /* Summaries... */
            (LineType::Empty, token::TokenClass::SummaryEpilogue) => (LineType::Summary {
                title: None,
                tokens: collections::VecDeque::from([token]),
            }, LinePushResult::Accepted),

            (LineType::Summary { title: None, tokens }, token::TokenClass::Title)
                if token.node.props.title_display.is_inline()
                => match tokens.front() {
                    Some(token::Token { class: token::TokenClass::SummaryPreamble, node, .. }) if sync::Arc::ptr_eq(&node, &token.node) => (LineType::Summary { title: Some(token), tokens }, LinePushResult::Accepted),
                    Some(_) => (LineType::Summary { title: None, tokens }, LinePushResult::Rejected),
                    None => panic!("LineType::Summary should have at least one token")
                },
                
            (LineType::Summary { title, mut tokens }, _) => {
                let result = match (tokens.front(), &token.class) {
                    (Some(token::Token { class: token::TokenClass::SummaryPreamble, .. }), _) => LinePushResult::Rejected,
                    (Some(_), token::TokenClass::SummaryPreamble) => { tokens.push_front(token); LinePushResult::Completed },
                    (Some(_), _) => { tokens.push_front(token); LinePushResult::Accepted },
                    (None, _) => panic!("LineType::Summary should have at least one token")
                };

                (LineType::Summary { title, tokens }, result)
            },

            (LineType::Empty, _class) => {
                println!("Attempted to end a line on a bad token");
                (LineType::Empty, LinePushResult::BadPosition)
            },
            
            (ty, _) => (ty, LinePushResult::Rejected)            
        };

        self.ty = new_ty;

        result
    }

    /// Returns true on success
    fn push_back(&mut self, token: token::Token) -> LinePushResult {
        let (new_ty, result) = match (std::mem::replace(&mut self.ty, LineType::Empty), &token.class) {
            /* A line with an Empty punctuation token on it can't have anything else on it. */
            (LineType::Blank(tok), _) => (LineType::Blank(tok), LinePushResult::Rejected),

            /* BlankLine punctuation tokens can go onto Empty lines to convert them to Blank lines, but nothing else. */
            (LineType::Empty, token::TokenClass::BlankLine { .. }) => (LineType::Blank(token), LinePushResult::Completed),
            (lt, token::TokenClass::BlankLine { .. }) => (lt, LinePushResult::Rejected),
                
            /* A title can begin a line. */
            (LineType::Empty, token::TokenClass::Title) => (LineType::Title(token), LinePushResult::Accepted),

            /* A hexdump token can begin a line. */
            (LineType::Empty, token::TokenClass::Hexdump { line, .. }) => (LineType::Hexdump {
                title: None,
                node: token.node.clone(),
                line_extent: *line,
                tokens: collections::VecDeque::from([token])
            }, LinePushResult::Accepted),

            /* A hexdump token can occur on the same line as a title if the title is inline. */
            (LineType::Title(title_token), token::TokenClass::Hexdump { extent: _, line })
                if sync::Arc::ptr_eq(&title_token.node, &token.node)
                && title_token.node.props.title_display.is_inline()
                => (LineType::Hexdump {
                    node: title_token.node.clone(),
                    title: Some(title_token),
                    line_extent: *line,
                    tokens: collections::VecDeque::from([token]),
                }, LinePushResult::Accepted),
                
            /* Multiple hexdump tokens can coexist on a line under certain conditions. */
            (LineType::Hexdump { title, node: line_node, line_extent, mut tokens },
             token::TokenClass::Hexdump { extent: token_extent, line: token_line_extent })
                if sync::Arc::ptr_eq(&line_node, &token.node)
                && *token_line_extent == line_extent
                => {
                    /* Must be monotonic and non-overlapping. */
                    let result = if match tokens.back() {
                        Some(token::Token { class: token::TokenClass::Hexdump { extent, .. }, .. }) => extent,
                        Some(_) => panic!("all tokens in LineType::Hexdump.tokens should have TokenClass::Hexdump"),
                        None => panic!("a LineType::Hexdump shouldn't be able to exist without at least one token"),
                    }.end > token_extent.begin {
                        // TODO: log properly
                        println!("Attempted to add a token to a LineType::Hexdump that would've broken monotonicity. This shouldn't really happen.");
                        LinePushResult::Rejected
                    } else {
                        tokens.push_back(token);
                        LinePushResult::Rejected
                    };

                    (LineType::Hexdump {
                        title,
                        node: line_node,
                        line_extent,
                        tokens
                    }, result)
                },

            /* A hexstring token can begin a line */
            (LineType::Empty, token::TokenClass::Hexstring(_)) => (LineType::Hexstring {
                title: None,
                token
            }, LinePushResult::Completed),

            /* A hexstring token can occur on the same line as a title if the title is inline. */
            (LineType::Title(title_token), token::TokenClass::Hexstring(_))
                if sync::Arc::ptr_eq(&title_token.node, &token.node)
                && title_token.node.props.title_display.is_inline()
                => (LineType::Hexstring {
                    title: Some(title_token),
                    token,
                }, LinePushResult::Accepted),
            
            /* Summaries... */
            (LineType::Empty, token::TokenClass::SummaryPreamble) => (LineType::Summary {
                title: None,
                tokens: collections::VecDeque::from([token]),
            }, LinePushResult::Accepted),
            
            (LineType::Title(title_token), token::TokenClass::SummaryPreamble)
                if sync::Arc::ptr_eq(&title_token.node, &token.node)
                && title_token.node.props.title_display.is_inline()
                => (LineType::Summary {
                    title: Some(title_token),
                    tokens: collections::VecDeque::from([token]),
                }, LinePushResult::Accepted),

            (LineType::Summary { title, mut tokens }, _) => {
                let result = match (tokens.back(), &token.class) {
                    (Some(token::Token { class: token::TokenClass::SummaryEpilogue, .. }), _) => LinePushResult::Rejected,
                    (Some(_), token::TokenClass::SummaryEpilogue) => { tokens.push_back(token); LinePushResult::Completed },
                    (Some(_), _) => { tokens.push_back(token); LinePushResult::Accepted },
                    (None, _) => panic!("LineType::Summary should have at least one token")
                };

                (LineType::Summary { title, tokens }, result)
            },

            (LineType::Empty, _class) => {
                println!("Attempted to start a line on a bad token");
                (LineType::Empty, LinePushResult::BadPosition)
            },
            
            (ty, _) => (ty, LinePushResult::Rejected)
        };

        self.ty = new_ty;

        result
    }

    fn is_empty(&self) -> bool {
        match &self.ty {
            LineType::Empty => true,
            _ => false,
        }
    }
    
    fn iter_tokens(&self) -> LineBorrowingTokenIterator<'_> {
        match &self.ty {
            LineType::Empty => util::PhiIterator::I1(iter::empty()),
            LineType::Blank(t) => util::PhiIterator::I2(iter::once(t)),
            LineType::Title(t) => util::PhiIterator::I2(iter::once(t)),
            LineType::Hexdump { title, tokens, .. } => util::PhiIterator::I3(title.iter().chain(tokens.iter())),
            LineType::Hexstring { title, token, .. } => util::PhiIterator::I4(title.iter().chain(iter::once(token))),
            LineType::Summary { title, tokens, .. } => util::PhiIterator::I3(title.iter().chain(tokens.iter())),
        }
    }

    fn into_iter(self) -> LineTokenIterator {
        match self.ty {
            LineType::Empty => util::PhiIterator::I1(iter::empty()),
            LineType::Blank(t) => util::PhiIterator::I2(iter::once(t)),
            LineType::Title(t) => util::PhiIterator::I2(iter::once(t)),
            LineType::Hexdump { title, tokens, .. } => util::PhiIterator::I3(title.into_iter().chain(tokens.into_iter())),
            LineType::Hexstring { title, token, .. } => util::PhiIterator::I4(title.into_iter().chain(iter::once(token))),
            LineType::Summary { title, tokens, .. } => util::PhiIterator::I3(title.into_iter().chain(tokens.into_iter())),
        }
    }
}

type LineBorrowingTokenIterator<'a> = util::PhiIterator
    <&'a token::Token,
     iter::Empty<&'a token::Token>,
     iter::Once<&'a token::Token>,
     iter::Chain<std::option::Iter<'a, token::Token>, collections::vec_deque::Iter<'a, token::Token>>,
     iter::Chain<std::option::Iter<'a, token::Token>, iter::Once<&'a token::Token>>>;

type LineTokenIterator = util::PhiIterator
    <token::Token,
     iter::Empty<token::Token>,
     iter::Once<token::Token>,
     iter::Chain<std::option::IntoIter<token::Token>, collections::vec_deque::IntoIter<token::Token>>,
     iter::Chain<std::option::IntoIter<token::Token>, iter::Once<token::Token>>>;

impl LineView for Line {
    // TODO: clean me up when we get impl_trait_in_assoc_type
    type BorrowingTokenIterator<'a> = LineBorrowingTokenIterator<'a>;
    type TokenIterator = LineTokenIterator;
    
    fn from_line(line: Line) -> Self {
        line
    }

    fn iter_tokens(&self) -> Self::BorrowingTokenIterator<'_> {
        self.iter_tokens()
    }
    
    fn to_tokens(self) -> Self::TokenIterator {
        self.into_iter()
    }
}

impl PartialEq for Line {
    fn eq(&self, other: &Line) -> bool {
        match (&self.ty, &other.ty) {
            (LineType::Empty, LineType::Empty) => true,
            (LineType::Blank(tok1), LineType::Blank(tok2)) => tok1.eq(tok2),
            (LineType::Title(tok1), LineType::Title(tok2)) => tok1.eq(tok2),
            
            (LineType::Hexdump {
                title: title1, node: node1, line_extent: line_extent1, tokens: tokens1
            }, LineType::Hexdump {
                title: title2, node: node2, line_extent: line_extent2, tokens: tokens2
            }) => title1.eq(title2) && sync::Arc::ptr_eq(node1, node2) && line_extent1.eq(line_extent2) && tokens1.iter().eq(tokens2.iter()),

            (LineType::Hexstring {
                title: title1, token: token1
            }, LineType::Hexstring {
                title: title2, token: token2
            }) => title1.eq(title2) && token1.eq(token2),

            (LineType::Summary {
                title: title1, tokens: tokens1
            }, LineType::Summary {
                title: title2, tokens: tokens2
            }) => title1.eq(title2) && tokens1.iter().eq(tokens2.iter()),
            
            _ => false
        }
    }
}

impl Eq for Line {
}

impl fmt::Debug for Line {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Line")
            .field("type", match &self.ty {
                LineType::Empty => &"empty",
                LineType::Blank(_) => &"blank",
                LineType::Title(_) => &"title",
                LineType::Hexdump { .. } => &"hexdump",
                LineType::Hexstring { .. } => &"hexstring",
                LineType::Summary { .. } => &"summary",
            })
            .field("tokens", &self.iter_tokens().map(|tok| token::TokenTestFormat(tok)).collect::<Vec<_>>())
            .finish()
    }
}

impl WindowTokenizer for tokenizer::Tokenizer {
    fn at_beginning(root: sync::Arc<structure::Node>) -> Self {
        tokenizer::Tokenizer::at_beginning(root)
    }

    fn at_path(root: sync::Arc<structure::Node>, path: &structure::Path, offset: addr::Address) -> Self {
        tokenizer::Tokenizer::at_path(root, path, offset)
    }

    fn port_change(&mut self, new_doc: &sync::Arc<document::Document>, change: &document::change::Change) {
        tokenizer::Tokenizer::port_change(self, &new_doc.root, change, &tokenizer::PortOptions::default());
    }
    
    fn hit_top(&self) -> bool {
        tokenizer::Tokenizer::hit_top(self)
    }
    
    fn hit_bottom(&self) -> bool {
        tokenizer::Tokenizer::hit_bottom(self)
    }
    
    fn gen_token(&self) -> tokenizer::TokenGenerationResult {
        tokenizer::Tokenizer::gen_token(self)
    }
    
    fn move_prev(&mut self) -> bool {
        tokenizer::Tokenizer::move_prev(self)
    }

    fn move_next(&mut self) -> bool {
        tokenizer::Tokenizer::move_next(self)
    }

    fn next_postincrement(&mut self) -> Option<token::Token> {
        tokenizer::Tokenizer::next_postincrement(self)
    }

    fn prev(&mut self) -> Option<token::Token> {
        tokenizer::Tokenizer::prev(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    trait TestTokenGenerator {
        fn generate_tokens(root: sync::Arc<structure::Node>) -> Vec<token::Token>;
    }
    
    /* This helps us isolate faults for testing */
    struct TestTokenizer<G: TestTokenGenerator> {
        tokens: Vec<token::Token>,
        position: usize,
        skip: bool,
        marker: core::marker::PhantomData<G>,
    }

    /* Derive thinks the PhantomData is uncloneable */
    impl<G: TestTokenGenerator> Clone for TestTokenizer<G> {
        fn clone(&self) -> Self {
            TestTokenizer {
                tokens: self.tokens.clone(),
                position: self.position,
                skip: self.skip,
                marker: self.marker.clone()
            }
        }
    }
    
    impl<G: TestTokenGenerator> WindowTokenizer for TestTokenizer<G> {
        fn at_beginning(root: sync::Arc<structure::Node>) -> TestTokenizer<G> {
            TestTokenizer {
                tokens: G::generate_tokens(root),
                position: 0,
                skip: false,
                marker: Default::default(),
            }
        }

        fn at_path(_root: sync::Arc<structure::Node>, _path: &structure::Path, _offset: addr::Address) -> TestTokenizer<G> {
            panic!("unsupported");
        }

        fn port_change(&mut self, _new_doc: &sync::Arc<document::Document>, _change: &document::change::Change) {
            panic!("unsupported");
        }

        fn hit_top(&self) -> bool {
            self.position == 0
        }

        fn hit_bottom(&self) -> bool {
            false
        }

        fn gen_token(&self) -> tokenizer::TokenGenerationResult {
            if self.skip {
                tokenizer::TokenGenerationResult::Skip
            } else if self.position < self.tokens.len() {
                tokenizer::TokenGenerationResult::Ok(self.tokens[self.position].clone())
            } else {
                tokenizer::TokenGenerationResult::Boundary
            }
        }
        
        fn move_prev(&mut self) -> bool {
            if self.skip {
                self.skip = false;
                true
            } else if self.position > 0 {
                self.position-= 1;
                true
            } else {
                false
            }
        }

        fn move_next(&mut self) -> bool {
            if !self.skip {
                self.skip = true;
                true
            } else if self.position < self.tokens.len() {
                self.skip = false;
                self.position+= 1;
                true
            } else {
                false
            }
        }

        fn next_postincrement(&mut self) -> Option<token::Token> {
            let mut token;
            while {
                token = self.gen_token();
                self.move_next()
            } {
                match token {
                    tokenizer::TokenGenerationResult::Ok(token) => return Some(token),
                    tokenizer::TokenGenerationResult::Skip => continue,
                    tokenizer::TokenGenerationResult::Boundary => return None,
                }
            }
            None
        }

        fn prev(&mut self) -> Option<token::Token> {
            while self.move_prev() {
                match self.gen_token() {
                    tokenizer::TokenGenerationResult::Ok(token) => return Some(token),
                    tokenizer::TokenGenerationResult::Skip => continue,
                    tokenizer::TokenGenerationResult::Boundary => return None,
                }
            }
            None
        }
    }
    
    #[test]
    fn scroll_around() {
        let document = sync::Arc::new(document::Document::invalid());
        let mut window = Window::<Line>::new(document);
        
        window.resize(5);

        /* scroll back and forth a bit */
        assert!(window.scroll_down());
        assert!(window.scroll_down());
        assert!(window.scroll_up());
        assert!(window.scroll_down());
        assert!(window.scroll_down());
        assert!(window.scroll_up());
        assert!(window.scroll_up());
        assert!(window.scroll_up());
    }

    fn print_lines<Tokenizer: WindowTokenizer>(window: &Window<Line, Tokenizer>) {
        for l in &window.line_views {
            print!("  ");
            for t in l.iter_tokens() {
                print!("{}", token::TokenTestFormat(t));
            }
            println!();
        }        
    }
    
    #[test]
    fn bonk_top() {
        let document = sync::Arc::new(document::Document::invalid());
        let mut window = Window::<Line>::new(document);
        
        window.resize(5);

        let window_before = window.clone();

        println!("before scrolling down: ");
        print_lines(&window);
        
        /* scroll down and back up again */
        assert!(window.scroll_down());
        println!("after scrolling down: ");
        print_lines(&window);

        assert!(window.scroll_up());
        println!("after scrolling back up: ");
        print_lines(&window);
        
        /* should leave the window in the same state */
        itertools::assert_equal(
            window_before.line_views.iter(),
            window.line_views.iter());
        
        /* should hit the top */
        assert!(!window.scroll_up());

        /* after having hit the top, scroll down and back up again */
        assert!(window.scroll_down());
        assert!(window.scroll_up());
    }

    #[test]
    fn graze_top() {
        let document = sync::Arc::new(document::Document::invalid());
        let mut window = Window::<Line>::new(document);

        window.resize(2);

        let window_before = window.clone();

        print_lines(&window);
        assert!(window.scroll_down());
        print_lines(&window);
        assert!(window.scroll_down());
        print_lines(&window);
        assert!(window.scroll_up());
        print_lines(&window);
        assert!(window.scroll_up());
        print_lines(&window);

        itertools::assert_equal(
            window_before.line_views.iter(),
            window.line_views.iter());
        
        assert!(window.scroll_down());
    }
}
