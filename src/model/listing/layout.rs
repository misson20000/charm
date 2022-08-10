//! This module includes the logic that converts from a token stream
//! to lines that would be displayed in a window.

use std::collections;
use std::sync;
use std::task;
use std::vec;

use crate::model::addr;
use crate::model::document::structure;
use crate::model::listing::token;
use crate::logic::tokenizer;

pub struct Line {
    pub indent: usize,
    pub tokens: vec::Vec<token::Token>
}

/// A listing window with a fixed height. Useful for scrolling by lines.
/// It is up to the user to make sure that this gets properly notified with structure invalidation events.
pub struct Window {
    current_root: sync::Arc<structure::Node>,
    top: tokenizer::Tokenizer,
    bottom: tokenizer::Tokenizer,

    top_buffer: Option<token::Token>,
    
    pub lines: collections::VecDeque<Line>,
    pub window_height: usize,
    
    pub wants_update: bool,
}

impl Window {
    pub fn new(root: &sync::Arc<structure::Node>) -> Window {
        Window {
            top: tokenizer::Tokenizer::at_beginning(root),
            bottom: tokenizer::Tokenizer::at_beginning(root),
            top_buffer: None,
            
            current_root: root.clone(),
            
            lines: std::collections::VecDeque::<Line>::new(),
            window_height: 0,
            
            wants_update: false,
        }
    }

    /// Moves the top of the window to the specified address. Returns amount
    /// window was adjusted upwards by due to hitting the bottom of the address space.
    pub fn seek(&mut self, target: addr::Address) -> usize {
        self.repopulate_window(tokenizer::Tokenizer::at_address(&self.current_root, target))
    }

    fn repopulate_window(&mut self, pos: tokenizer::Tokenizer) -> usize {
        self.top = pos.clone();
        self.bottom = pos;
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

        self.lines.push_front(Line {
            indent: tokens[0].depth,
            tokens: tokens.into()
        });
    }

    fn grow_bottom(&mut self) {
        let mut tokens = vec::Vec::new();

        while let Some(token) = self.bottom.next() {
            let nl = token.newline;
            
            tokens.push(token);

            if nl {
                break;
            }
        }

        self.lines.push_back(Line {
            indent: tokens.get(0).map_or(0, |t| t.depth),
            tokens
        });
    }

    fn shrink_top(&mut self) {
        let line = self.lines.pop_front().unwrap();

        for token in line.tokens.into_iter() {
            assert_eq!(token, self.top.next().unwrap());
            self.top_buffer = Some(token);
        }
    }

    fn shrink_bottom(&mut self) {
        let line = self.lines.pop_back().unwrap();

        for token in line.tokens.into_iter().rev() {
            assert_eq!(token, self.bottom.prev().unwrap());
        }
    }
    
    /// Scrolls the window upwards by one line. Returns false if the
    /// beginning of the token stream is hit.
    pub fn scroll_up(&mut self) -> bool {
        if self.top.hit_top() {
            return true;
        }
        
        self.grow_top();
        self.shrink_bottom();

        self.wants_update = true;

        false
    }

    /// Scrolls the window downwards by one line. Returns false if the
    /// end of the token stream is hit.
    pub fn scroll_down(&mut self) -> bool {
        if self.bottom.hit_bottom() {
            return true;
        }
        
        self.grow_bottom();
        self.shrink_top();

        self.wants_update = true;

        false
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

    pub fn update(&mut self, root: &sync::Arc<structure::Node>, _cx: &mut task::Context) -> bool {
        let mut updated = false;
        
        if !sync::Arc::ptr_eq(&self.current_root, root) {
            self.current_root = root.clone();
            self.repopulate_window(tokenizer::Tokenizer::port(&self.top, &self.current_root));
            updated = true;
        }

        //for line in self.lines.iter_mut() {
        //    updated = line.update(document, cx) || updated;
        //}

        updated
    }
}
