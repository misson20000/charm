use std::collections;
use std::iter;
use std::sync;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::line;
use crate::model::listing::stream;
use crate::model::listing::token;
use crate::model::versioned::Versioned;

pub trait LineView {
    fn from_line(line: line::Line) -> Self;
    fn iter_tokens(&self) -> impl iter::Iterator<Item = token::TokenRef<'_>>;
    fn to_tokens(self) -> impl iter::DoubleEndedIterator<Item = token::Token>;
}

/// A listing window with a fixed height. Useful for scrolling by lines.
/// It is up to the user to make sure that this gets properly notified with structure invalidation events.
#[derive(Clone)]
pub struct Window<LV: LineView, Position: stream::AbstractPosition = stream::Position> {
    pub current_document: sync::Arc<document::Document>,
    top: Position,
    bottom: Position,
    
    pub line_views: collections::VecDeque<LV>,
    pub window_height: usize,
    
    pub wants_update: bool,
}

impl<LV: LineView, Position: stream::AbstractPosition> Window<LV, Position> {
    pub fn new(doc: sync::Arc<document::Document>) -> Window<LV, Position> {
        Window {
            top: Position::at_beginning(doc.root.clone()),
            bottom: Position::at_beginning(doc.root.clone()),
            
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
        self.repopulate_window(move |tok, _| *tok = Position::at_path(root, path, offset))
    }

    fn repopulate_window<F>(&mut self, position_provider: F) -> usize where
        F: FnOnce(&mut Position, &mut sync::Arc<document::Document>) {
        self.bottom = self.top.clone();
        position_provider(&mut self.bottom, &mut self.current_document);
        let (first_line, top, _index) = line::Line::containing_position(&mut self.bottom);
        self.top = top;
        self.line_views.clear();

        if !first_line.is_empty() {
            self.line_views.push_back(LV::from_line(first_line));
        }
        
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

        let line = line::Line::prev_from_position(&mut self.top);

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

        let line = line::Line::next_from_position(&mut self.bottom);
        
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

impl LineView for line::Line {
    fn from_line(line: line::Line) -> Self {
        line
    }

    fn iter_tokens(&self) -> impl iter::Iterator<Item = token::TokenRef<'_>> {
        self.iter_tokens()
    }
    
    fn to_tokens(self) -> impl iter::DoubleEndedIterator<Item = token::Token> {
        self.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::listing::token::TokenKind;
    
    #[test]
    fn scroll_around() {
        let document = document::Builder::default().arc();
        let mut window = Window::<line::Line>::new(document);
        
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

    fn print_lines<Position: stream::AbstractPosition>(window: &Window<line::Line, Position>) {
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
        let mut window = Window::<line::Line>::new(document);
        
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
        let mut window = Window::<line::Line>::new(document);

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

    #[test]
    fn seek() {
        let root = structure::Node::builder()
            .name("root")
            .size(0x4a0)
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
                   .size(0x40))
            .child(0xa0, |b| b
                   .name("child3")
                   .size(0x400))
            .build();
        let document = document::Builder::new(root).arc();
        let mut window1 = Window::<line::Line>::new(document.clone());
        window1.resize(10);
        let mut window2 = window1.clone();

        /* Scroll window 1 down until its first line is the one we're going to seek the other window to. */
        while !match window1.line_views[0].iter_tokens().next().unwrap() {
            token::TokenRef::Hexdump(hdt) => hdt.common.node_path == &[2] && hdt.extent.begin == 0x20.into(),
            _ => false
        } {
            window1.scroll_down();
        }
        window2.seek(document.clone(), &vec![2], 0x24.into());

        if !window1.line_views.iter().eq(window2.line_views.iter()) {
            let mut i1 = window1.line_views.iter();
            let mut i2 = window2.line_views.iter();
            loop {
                let line1 = i1.next();
                let line2 = i2.next();

                let str1 = line1.map(|l| format!("{}", l)).unwrap_or("<end>".to_string());
                let str2 = line2.map(|l| format!("{}", l)).unwrap_or("<end>".to_string());

                println!("{:60} | {:60}", str1, str2);

                if line1.is_none() && line2.is_none() {
                    break;
                }
            }
            panic!("windows mismatched");
        }
    }
    
    #[test]
    fn containing_position() {
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

        /* Pregenerate all the lines from a simple forward walk through the whole document. */
        let mut position = stream::Position::at_beginning(root.clone());
        let mut lines = vec![];
        loop {
            let mut begin = position.clone();
            begin.canonicalize_next();
            
            let line = line::Line::next_from_position(&mut position);
            if line.is_empty() {
                break;
            }

            let mut end = position.clone();
            end.canonicalize_next();
            
            lines.push((begin, line, end));
        }

        let mut position = stream::Position::at_beginning(root.clone());
        let mut i = 0;
        loop {
            let token = match position.gen_token() {
                stream::TokenGenerationResult::Ok(token) => token,
                stream::TokenGenerationResult::Skip => if position.move_next() { continue } else { break },
                stream::TokenGenerationResult::Boundary => break,
            };

            let expected_index_in_line = loop {
                if let Some(index) = lines[i].1.iter_tokens().position(|t| t == token.as_ref()) {
                    break index;
                } else {
                    i+= 1;
                }
            };
            
            let mut line_end = position.clone();
            let (line, mut line_begin, index_in_line) = line::Line::containing_position(&mut line_end);
            line_begin.canonicalize_next();
            line_end.canonicalize_next();
            
            if line != lines[i].1 || index_in_line != expected_index_in_line || line_begin != lines[i].0 || line_end != lines[i].2 {
                println!("seeked to {:?}", token);
                println!("line from forward walk        : {}", lines[i].1);
                println!("line from containing_position: {}", line);
                println!("expected index {}, got index {}", expected_index_in_line, index_in_line);
                
                println!("begin position [actual]  : {:#?}", line_begin);
                println!("begin position [expected]: {:#?}", lines[i].0);
                    
                println!("end position [actual]  : {:#?}", line_end);
                println!("end position [expected]: {:#?}", lines[i].2);
                
                panic!("mismatched");
            }

            position.move_next();
        }
    }
}
