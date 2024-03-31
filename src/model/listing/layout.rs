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
use crate::model::listing::token::TokenKind;
use crate::model::versioned::Versioned;
use crate::logic::tokenizer;
use crate::util;

#[derive(Clone)]
pub enum LineType {
    Empty,
    Blank(token::BlankLineToken),
    Title(token::TitleToken),
    Hexdump {
        title: Option<token::TitleToken>,
        node: sync::Arc<structure::Node>,
        node_path: structure::Path,
        node_addr: addr::Address,
        line_extent: addr::Extent,
        tokens: collections::VecDeque<token::HexdumpToken>
    },
    Hexstring {
        title: Option<token::TitleToken>,
        token: token::HexstringToken,
    },
    Summary {
        title: Option<token::TitleToken>,
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
    pub ty: LineType,
}

pub trait LineView {
    fn from_line(line: Line) -> Self;
    fn iter_tokens(&self) -> impl iter::Iterator<Item = token::TokenRef<'_>>;
    fn to_tokens(self) -> impl iter::DoubleEndedIterator<Item = token::Token>;
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

        let line = Line::prev_from_tokenizer(&mut self.top);

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

        let line = Line::next_from_tokenizer(&mut self.bottom);
        
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
    pub fn empty() -> Self {
        Line {
            ty: LineType::Empty
        }
    }

    /// Returns the line ending at the tokenizer's current position, and moves the tokenizer to the beginning of that line.
    pub fn prev_from_tokenizer(tokenizer: &mut impl WindowTokenizer) -> Self {
        let mut line = Self::empty();

        loop {
            if !tokenizer.move_prev() {
                break;
            }

            if match line.push_front(match tokenizer.gen_token() {
                tokenizer::TokenGenerationResult::Ok(token) => token,
                tokenizer::TokenGenerationResult::Skip => continue,
                tokenizer::TokenGenerationResult::Boundary => break,
            }) {
                LinePushResult::Accepted => continue,
                LinePushResult::Completed => break,
                LinePushResult::Rejected => true,
                LinePushResult::BadPosition => false,
            } {
                /* roll the state back */
                assert!(tokenizer.move_next());
                break;
            }
        }

        line
    }

    /// Returns the line beginning at the tokenizer's current position, and moves the tokenizer to the end of that line.
    pub fn next_from_tokenizer(tokenizer: &mut impl WindowTokenizer) -> Self {
        let mut line = Line::empty();

        loop {
            match line.push_back(match tokenizer.gen_token() {
                tokenizer::TokenGenerationResult::Ok(token) => token,
                tokenizer::TokenGenerationResult::Skip => if tokenizer.move_next() { continue } else { break },
                tokenizer::TokenGenerationResult::Boundary => break,
            }) {
                LinePushResult::Accepted => tokenizer.move_next(),
                LinePushResult::Completed => { tokenizer.move_next(); break },
                LinePushResult::Rejected => break,
                LinePushResult::BadPosition => tokenizer.move_next(),
            };
        }

        line
    }
    
    /// Returns true on success
    fn push_front(&mut self, token: token::Token) -> LinePushResult {
        let (new_ty, result) = match (std::mem::replace(&mut self.ty, LineType::Empty), token) {
            /* A line with an Empty punctuation token on it can't have anything else on it. */
            (LineType::Blank(token), _) => (LineType::Blank(token), LinePushResult::Rejected),

            /* BlankLine punctuation tokens can go onto Empty lines to convert them to Blank lines, but nothing else. */
            (LineType::Empty, token::Token::BlankLine(token)) => (LineType::Blank(token), LinePushResult::Completed),
            (lt, token::Token::BlankLine(_)) => (lt, LinePushResult::Rejected),

            /* A title can end a line. */
            (LineType::Empty, token::Token::Title(token)) => (LineType::Title(token), LinePushResult::Accepted),

            /* A hexdump token can end a line. */
            (LineType::Empty, token::Token::Hexdump(token)) => (LineType::Hexdump {
                title: None,
                node: token.common.node.clone(),
                node_path: token.common.node_path.clone(),
                node_addr: token.common.node_addr,
                line_extent: token.line.clone(),
                tokens: collections::VecDeque::from([token])
            }, LinePushResult::Accepted),

            /* A title token can occur on the same line as a hexdump if the title is inline and there isn't already a title. */
            (LineType::Hexdump {
                title: None,
                node,
                node_path,
                node_addr,
                line_extent,
                tokens
            }, token::Token::Title(token))
                if sync::Arc::ptr_eq(&token.common.node, &node)
                && node_path == token.common.node_path
                && node_addr == token.common.node_addr
                && token.common.node.props.title_display.is_inline()
                => (LineType::Hexdump {
                    title: Some(token),
                    node,
                    node_path,
                    node_addr,
                    line_extent,
                    tokens
                }, LinePushResult::Accepted),

            /* Multiple hexdump tokens can coexist on a line under certain conditions. */
            (LineType::Hexdump { title, node: line_node, node_path, node_addr, line_extent, mut tokens },
             token::Token::Hexdump(token))
                if sync::Arc::ptr_eq(&line_node, &token.common.node)
                && node_path == token.common.node_path
                && node_addr == token.common.node_addr
                && token.line == line_extent
                => {
                    /* Must be monotonic and non-overlapping. */
                    let result = if tokens.front().expect("should have at least one token").extent.begin < token.extent.end {
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
                        node_path,
                        node_addr,
                        line_extent,
                        tokens
                    }, result)
                },

            /* A hexstring token can end a line */
            (LineType::Empty, token::Token::Hexstring(token)) => (LineType::Hexstring {
                title: None,
                token
            }, LinePushResult::Completed),

            /* A title token can occur on the same line as a hexstring if the title is inline and there isn't already a title. */
            (LineType::Hexstring { title: None, token: hexstring_token }, token::Token::Title(token))
                if sync::Arc::ptr_eq(&token.common.node, &hexstring_token.common.node)
                && token.common.node.props.title_display.is_inline()
                => (LineType::Hexstring {
                    title: Some(token),
                    token: hexstring_token,
                }, LinePushResult::Accepted),

            /* Summaries... */
            (LineType::Empty, token::Token::SummaryEpilogue(token)) => (LineType::Summary {
                title: None,
                tokens: collections::VecDeque::from([token.into_token()]),
            }, LinePushResult::Accepted),

            (LineType::Summary { title: None, tokens }, token::Token::Title(token))
                if token.node().props.title_display.is_inline()
                => match tokens.front() {
                    Some(token::Token::SummaryPreamble(preamble)) if sync::Arc::ptr_eq(preamble.node(), token.node()) => (LineType::Summary { title: Some(token), tokens }, LinePushResult::Accepted),
                    Some(_) => (LineType::Summary { title: None, tokens }, LinePushResult::Rejected),
                    None => panic!("LineType::Summary should have at least one token")
                },
                
            (LineType::Summary { title, mut tokens }, token) => {
                let result = match (tokens.front(), token) {
                    (Some(token::Token::SummaryPreamble(_)), _) => LinePushResult::Rejected,
                    (Some(_), token::Token::SummaryPreamble(preamble)) => { tokens.push_front(preamble.into_token()); LinePushResult::Completed },
                    (Some(_), token) => { tokens.push_front(token); LinePushResult::Accepted },
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
        let (new_ty, result) = match (std::mem::replace(&mut self.ty, LineType::Empty), token) {
            /* A line with an Empty punctuation token on it can't have anything else on it. */
            (LineType::Blank(tok), _) => (LineType::Blank(tok), LinePushResult::Rejected),

            /* BlankLine punctuation tokens can go onto Empty lines to convert them to Blank lines, but nothing else. */
            (LineType::Empty, token::Token::BlankLine(token)) => (LineType::Blank(token), LinePushResult::Completed),
            (lt, token::Token::BlankLine(_)) => (lt, LinePushResult::Rejected),
                
            /* A title can begin a line. */
            (LineType::Empty, token::Token::Title(token)) => (LineType::Title(token), LinePushResult::Accepted),

            /* A hexdump token can begin a line. */
            (LineType::Empty, token::Token::Hexdump(token)) => (LineType::Hexdump {
                title: None,
                node: token.node().clone(),
                node_path: token.node_path().clone(),
                node_addr: token.node_addr(),
                line_extent: token.line,
                tokens: collections::VecDeque::from([token])
            }, LinePushResult::Accepted),

            /* A hexdump token can occur on the same line as a title if the title is inline. */
            (LineType::Title(title_token), token::Token::Hexdump(token))
                if sync::Arc::ptr_eq(title_token.node(), token.node())
                && title_token.node().props.title_display.is_inline()
                => (LineType::Hexdump {
                    node: title_token.node().clone(),
                    node_path: token.node_path().clone(),
                    node_addr: token.node_addr(),
                    title: Some(title_token),
                    line_extent: token.line,
                    tokens: collections::VecDeque::from([token]),
                }, LinePushResult::Accepted),
                
            /* Multiple hexdump tokens can coexist on a line under certain conditions. */
            (LineType::Hexdump { title, node: line_node, node_path, node_addr, line_extent, mut tokens },
             token::Token::Hexdump(token))
                if sync::Arc::ptr_eq(&line_node, token.node())
                && node_path == token.common.node_path
                && node_addr == token.common.node_addr
                && line_extent == token.line
                => {
                    /* Must be monotonic and non-overlapping. */
                    let result = if tokens.back().expect("should have at least one token").extent.end > token.extent.begin {
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
                        node_path,
                        node_addr,
                        line_extent,
                        tokens
                    }, result)
                },

            /* A hexstring token can begin a line */
            (LineType::Empty, token::Token::Hexstring(token)) => (LineType::Hexstring {
                title: None,
                token
            }, LinePushResult::Completed),

            /* A hexstring token can occur on the same line as a title if the title is inline. */
            (LineType::Title(title_token), token::Token::Hexstring(token))
                if sync::Arc::ptr_eq(title_token.node(), token.node())
                && title_token.node().props.title_display.is_inline()
                => (LineType::Hexstring {
                    title: Some(title_token),
                    token,
                }, LinePushResult::Accepted),
            
            /* Summaries... */
            (LineType::Empty, token::Token::SummaryPreamble(token)) => (LineType::Summary {
                title: None,
                tokens: collections::VecDeque::from([token.into_token()]),
            }, LinePushResult::Accepted),
            
            (LineType::Title(title_token), token::Token::SummaryPreamble(token))
                if sync::Arc::ptr_eq(title_token.node(), token.node())
                && title_token.node().props.title_display.is_inline()
                => (LineType::Summary {
                    title: Some(title_token),
                    tokens: collections::VecDeque::from([token.into_token()]),
                }, LinePushResult::Accepted),

            (LineType::Summary { title, mut tokens }, token) => {
                let result = match (tokens.back(), token) {
                    (Some(token::Token::SummaryEpilogue(_)), _) => LinePushResult::Rejected,
                    (Some(_), token::Token::SummaryEpilogue(token)) => { tokens.push_back(token.into_token()); LinePushResult::Completed },
                    (Some(_), token) => { tokens.push_back(token); LinePushResult::Accepted },
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

    pub fn is_empty(&self) -> bool {
        match &self.ty {
            LineType::Empty => true,
            _ => false,
        }
    }
    
    pub fn iter_tokens(&self) -> impl iter::Iterator<Item = token::TokenRef<'_>> {
        /* These need a little bit of help to coerce properly. This may be a compiler bug? */
        let hexdump_mapper: for<'b> fn(&'b token::HexdumpToken) -> token::TokenRef<'b> = TokenKind::as_ref;
        let token_mapper: for<'b> fn(&'b token::Token) -> token::TokenRef<'b> = TokenKind::as_ref;
        
        match &self.ty {
            LineType::Empty => util::PhiIteratorOf5::I1(iter::empty()),
            LineType::Blank(t) => util::PhiIteratorOf5::I2(iter::once(t.as_ref())),
            LineType::Title(t) => util::PhiIteratorOf5::I2(iter::once(t.as_ref())),
            LineType::Hexdump { title, tokens, .. } => util::PhiIteratorOf5::I3(title.as_ref().map(TokenKind::as_ref).into_iter().chain(tokens.iter().map(hexdump_mapper))),
            LineType::Hexstring { title, token, .. } => util::PhiIteratorOf5::I4(title.as_ref().map(TokenKind::as_ref).into_iter().chain(iter::once(token.as_ref()))),
            LineType::Summary { title, tokens, .. } => util::PhiIteratorOf5::I5(title.as_ref().map(TokenKind::as_ref).into_iter().chain(tokens.iter().map(token_mapper))),
        }
    }

    pub fn into_iter(self) -> impl iter::DoubleEndedIterator<Item = token::Token> {
        /* These need a little bit of help to coerce properly. This may be a compiler bug? */
        let hexdump_mapper: fn(token::HexdumpToken) -> token::Token = TokenKind::into_token;
        
        match self.ty {
            LineType::Empty => util::PhiIteratorOf5::I1(iter::empty()),
            LineType::Blank(t) => util::PhiIteratorOf5::I2(iter::once(t.into_token())),
            LineType::Title(t) => util::PhiIteratorOf5::I2(iter::once(t.into_token())),
            LineType::Hexdump { title, tokens, .. } => util::PhiIteratorOf5::I3(title.map(TokenKind::into_token).into_iter().chain(tokens.into_iter().map(hexdump_mapper))),
            LineType::Hexstring { title, token, .. } => util::PhiIteratorOf5::I4(title.map(TokenKind::into_token).into_iter().chain(iter::once(token.into_token()))),
            LineType::Summary { title, tokens, .. } => util::PhiIteratorOf5::I5(title.map(TokenKind::into_token).into_iter().chain(tokens.into_iter())),
        }
    }
}

impl LineView for Line {
    fn from_line(line: Line) -> Self {
        line
    }

    fn iter_tokens(&self) -> impl iter::Iterator<Item = token::TokenRef<'_>> {
        self.iter_tokens()
    }
    
    fn to_tokens(self) -> impl iter::DoubleEndedIterator<Item = token::Token> {
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
                title: title1, node: node1, node_path: node_path1, node_addr: node_addr1, line_extent: line_extent1, tokens: tokens1
            }, LineType::Hexdump {
                title: title2, node: node2, node_path: node_path2, node_addr: node_addr2, line_extent: line_extent2, tokens: tokens2
            }) => title1.eq(title2)
                && sync::Arc::ptr_eq(node1, node2)
                && node_path1 == node_path2
                && node_addr1 == node_addr2
                && line_extent1.eq(line_extent2)
                && tokens1.iter().eq(tokens2.iter()),

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
        let document = document::Builder::default().arc();
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
        let document = document::Builder::default().arc();
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
        let document = document::Builder::default().arc();
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
