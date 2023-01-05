//! This module includes the logic that converts from a token stream
//! to lines that would be displayed in a window.

use std::collections;
use std::iter;
use std::sync;
use std::vec;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::token;
use crate::logic::tokenizer;

pub trait Line {
    type TokenIterator: iter::DoubleEndedIterator<Item = token::Token>;
    
    fn from_tokens(tokens: vec::Vec<token::Token>) -> Self;
    fn to_tokens(self) -> Self::TokenIterator;
}

/* This lets us swap out a simpler implementation for testing to help narrow down
 * whether bugs are in Window logic or Tokenizer logic. */
pub trait WindowTokenizer: Clone {
    fn at_beginning(root: sync::Arc<structure::Node>) -> Self;
    fn at_address(root: sync::Arc<structure::Node>, addr: addr::Address) -> Self;
    fn port_doc(&mut self, old_doc: &document::Document, new_doc: &document::Document);
    fn hit_top(&self) -> bool;
    fn hit_bottom(&self) -> bool;
    fn prev(&mut self) -> Option<token::Token>;
    fn next_postincrement(&mut self) -> Option<token::Token>;
}

/// A listing window with a fixed height. Useful for scrolling by lines.
/// It is up to the user to make sure that this gets properly notified with structure invalidation events.
#[derive(Clone)]
pub struct Window<L: Line, Tokenizer: WindowTokenizer = tokenizer::Tokenizer> {
    current_document: sync::Arc<document::Document>,
    top: Tokenizer,
    bottom: Tokenizer,
    
    pub lines: collections::VecDeque<L>,
    pub window_height: usize,
    
    pub wants_update: bool,
}

impl<L: Line, Tokenizer: WindowTokenizer> Window<L, Tokenizer> {
    pub fn new(doc: sync::Arc<document::Document>) -> Window<L, Tokenizer> {
        Window {
            top: Tokenizer::at_beginning(doc.root.clone()),
            bottom: Tokenizer::at_beginning(doc.root.clone()),
            
            current_document: doc,
            
            lines: std::collections::VecDeque::<L>::new(),
            window_height: 0,
            
            wants_update: false,
        }
    }

    /// Moves the top of the window to the specified address. Returns amount
    /// window was adjusted upwards by due to hitting the bottom of the address space.
    pub fn seek(&mut self, target: addr::Address) -> usize {
        let root = self.current_document.root.clone();
        self.repopulate_window(move |tok, _| *tok = Tokenizer::at_address(root, target))
    }

    fn repopulate_window<F>(&mut self, tokenizer_provider: F) -> usize where
        F: FnOnce(&mut Tokenizer, &mut sync::Arc<document::Document>) {
        tokenizer_provider(&mut self.top, &mut self.current_document);
        self.bottom = self.top.clone();
        self.lines.clear();
        
        let mut offset = 0;
        
        while self.lines.len() < self.window_height {
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

        let mut tokens = collections::VecDeque::new();

        let mut first = true;
        
        while {
            match self.top.prev() {
                Some(token) if !first && token.newline => {
                    assert_eq!(self.top.next_postincrement(), Some(token));
                    false
                },
                Some(token) => {
                    tokens.push_front(token);
                    true
                },
                None => false /* hit top */
            }
        } {
            first = false;
        }
        
        self.lines.push_front(L::from_tokens(tokens.into()));
    }

    fn grow_bottom(&mut self) {
        if self.bottom.hit_bottom() {
            return;
        }

        let mut tokens = vec::Vec::new();

        while let Some(token) = self.bottom.next_postincrement() {
            let nl = token.newline;
            
            tokens.push(token);

            if nl {
                break;
            }
        }
        
        self.lines.push_back(L::from_tokens(tokens));
    }

    fn shrink_top(&mut self) {
        let line = self.lines.pop_front().unwrap();

        for token in line.to_tokens() {
            assert_eq!(token, self.top.next_postincrement().unwrap());
        }
    }

    fn shrink_bottom(&mut self) {
        let line = self.lines.pop_back().unwrap();

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

        while self.lines.len() > self.window_height {
            self.shrink_bottom();
        }
        while self.lines.len() < self.window_height {
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
                tok.port_doc(current_doc, document);
                *current_doc = document.clone()
            });
        }
    }
}

impl Line for vec::Vec<token::Token> {
    type TokenIterator = vec::IntoIter<token::Token>;
    
    fn from_tokens(tokens: vec::Vec<token::Token>) -> Self {
        tokens
    }

    fn to_tokens(self) -> Self::TokenIterator {
        self.into_iter()
    }
}

impl WindowTokenizer for tokenizer::Tokenizer {
    fn at_beginning(root: sync::Arc<structure::Node>) -> Self {
        tokenizer::Tokenizer::at_beginning(root)
    }

    fn at_address(root: sync::Arc<structure::Node>, addr: addr::Address) -> Self {
        tokenizer::Tokenizer::at_address(root, addr)
    }

    fn port_doc(&mut self, old_doc: &document::Document, new_doc: &document::Document) {
        self.port_doc(old_doc, new_doc, &tokenizer::PortOptions::default());
    }
    
    fn hit_top(&self) -> bool {
        self.hit_top()
    }
    
    fn hit_bottom(&self) -> bool {
        self.hit_bottom()
    }
    
    fn prev(&mut self) -> Option<token::Token> {
        self.prev()
    }
    
    fn next_postincrement(&mut self) -> Option<token::Token> {
        self.next_postincrement()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::{assert_eq};

    #[derive(Clone)]
    struct TestTokenizer {
        node: sync::Arc<structure::Node>,
        position: u64
    }

    impl WindowTokenizer for TestTokenizer {
        fn at_beginning(root: sync::Arc<structure::Node>) -> TestTokenizer {
            TestTokenizer {
                node: root,
                position: 0
            }
        }

        fn at_address(_root: sync::Arc<structure::Node>, _addr: addr::Address) -> TestTokenizer {
            panic!("unsupported");
        }

        fn port_doc(&mut self, _old_doc: &document::Document, _new_doc: &document::Document) {
            panic!("unsupported");
        }

        fn hit_top(&self) -> bool {
            self.position == 0
        }

        fn hit_bottom(&self) -> bool {
            false
        }

        fn prev(&mut self) -> Option<token::Token> {
            if self.position == 0 {
                None
            } else {
                self.position-= 1;
                Some(token::Token {
                    class: token::TokenClass::Hexdump(addr::Extent::sized(self.position.into(), 1.into())),
                    node: self.node.clone(),
                    node_addr: addr::unit::NULL,
                    depth: 0,
                    newline: self.position % 2 == 1,
                })
            }
        }

        fn next_postincrement(&mut self) -> Option<token::Token> {
            let position = self.position;
            self.position+= 1;
            Some(token::Token {
                class: token::TokenClass::Hexdump(addr::Extent::sized(position.into(), 1.into())),
                node: self.node.clone(),
                node_addr: addr::unit::NULL,
                depth: 0,
                newline: position % 2 == 1,
            })
        }
    }
    
    #[test]
    fn scroll_around() {
        let document = sync::Arc::new(document::Document::invalid());
        let mut window = Window::<vec::Vec<token::Token>>::new(document);
        
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

    fn print_lines<Tokenizer: WindowTokenizer>(window: &Window<vec::Vec<token::Token>, Tokenizer>) {
        for l in &window.lines {
            print!("  ");
            for t in l {
                print!("{}", token::TokenTestFormat(t));
            }
            println!("");
        }        
    }
    
    #[test]
    fn bonk_top() {
        let document = sync::Arc::new(document::Document::invalid());
        let mut window = Window::<vec::Vec<token::Token>>::new(document);
        
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
        
        /* shoudl leave the window in the same state */
        assert_eq!(window_before.lines, window.lines);
        
        /* should hit the top */
        assert!(!window.scroll_up());

        /* after having hit the top, scroll down and back up again */
        assert!(window.scroll_down());
        assert!(window.scroll_up());
    }

    #[test]
    fn graze_top() {
        let document = sync::Arc::new(document::Document::invalid());
        let mut window = Window::<vec::Vec<token::Token>, TestTokenizer>::new(document);

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

        assert_eq!(window_before.lines, window.lines);
        
        assert!(window.scroll_down());
    }
}
