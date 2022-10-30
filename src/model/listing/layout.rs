//! This module includes the logic that converts from a token stream
//! to lines that would be displayed in a window.

use std::collections;
use std::iter;
use std::sync;
use std::vec;

use crate::model::addr;
use crate::model::document;
use crate::model::listing::token;
use crate::logic::tokenizer;

pub trait Line {
    type TokenIterator: iter::DoubleEndedIterator<Item = token::Token>;
    
    fn from_tokens(tokens: vec::Vec<token::Token>) -> Self;
    fn to_tokens(self) -> Self::TokenIterator;
}

/// A listing window with a fixed height. Useful for scrolling by lines.
/// It is up to the user to make sure that this gets properly notified with structure invalidation events.
pub struct Window<L: Line> {
    current_document: sync::Arc<document::Document>,
    top: tokenizer::Tokenizer,
    bottom: tokenizer::Tokenizer,

    top_buffer: Option<token::Token>,
    
    pub lines: collections::VecDeque<L>,
    pub window_height: usize,
    
    pub wants_update: bool,
}

impl<L: Line> Window<L> {
    pub fn new(doc: sync::Arc<document::Document>) -> Window<L> {
        Window {
            top: tokenizer::Tokenizer::at_beginning(doc.root.clone()),
            bottom: tokenizer::Tokenizer::at_beginning(doc.root.clone()),
            top_buffer: None,
            
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
        self.repopulate_window(move |tok, _| *tok = tokenizer::Tokenizer::at_address(root, target))
    }

    fn repopulate_window<F>(&mut self, tokenizer_provider: F) -> usize where
        F: FnOnce(&mut tokenizer::Tokenizer, &mut sync::Arc<document::Document>) {
        tokenizer_provider(&mut self.top, &mut self.current_document);
        self.bottom = self.top.clone();
        self.top_buffer = None;
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
        let mut tokens = collections::VecDeque::new();
        
        while {
            if let Some(token) = std::mem::replace(&mut self.top_buffer, self.top.prev()) {
                tokens.push_front(token);
            }
            match &self.top_buffer {
                Some(token) => !token.newline,
                None => false /* hit top */
            }
        } { }

        if self.top.hit_top() {
            return;
        }
        
        self.lines.push_front(L::from_tokens(tokens.into()));
    }

    fn grow_bottom(&mut self) {
        let mut tokens = vec::Vec::new();

        while let Some(token) = self.bottom.next_postincrement() {
            let nl = token.newline;
            
            tokens.push(token);

            if nl {
                break;
            }
        }

        if self.bottom.hit_bottom() {
            return;
        }
        
        self.lines.push_back(L::from_tokens(tokens));
    }

    fn shrink_top(&mut self) {
        let line = self.lines.pop_front().unwrap();

        for token in line.to_tokens() {
            assert_eq!(token, self.top.next_postincrement().unwrap());
            self.top_buffer = Some(token);
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
