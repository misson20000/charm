//use std::collections;
use std::sync;
//use std::task;
use std::vec;

use crate::model::addr;
//use crate::model::document;
use crate::model::document::structure;
//use crate::model::listing;
use crate::model::listing::token;

//use enum_dispatch::enum_dispatch;

pub type Line = vec::Vec<token::Token>;

/*
#[enum_dispatch]
pub trait BreakView {
    fn produce(&mut self) -> Option<LineGroup>;

    /// We've scrolled in the opposite direction you want, and this line fell
    /// off the end of the window. Regress your state so you can generate it
    /// again. There is no guarantee that you specifically are the one who
    /// produced this line; the downward view may have produced it and it slid
    /// all the way across the window. However, you can expect to be the same
    /// type and have the same break as whatever view produced the line you're
    /// trimming.

    /// Return true on success.
    /// If you need to pass to the next BreakView, return false.
    fn trim(&mut self, lg: &LineGroup) -> bool;
    
    fn hit_boundary(&self) -> bool;
    
    fn get_break(&self) -> &sync::Arc<brk::Break>;

    fn get_addr(&self) -> addr::Address;
}
*/

enum TokenGeneratorState {
    Clean, //< if going downward, would emit Null token.
    Title, //< if going downward, would emit Title token.
    Content(addr::Offset, usize), //< if going downward, would emit either the indexed child, or content starting at the offset.
    End, //< if going downward, we've hit the end of this node's content.
}

struct TokenGenerator {
    /* invariants:
       - stack should always contain a path all the way back to the root node
     */
    stack: Option<Box<TokenGenerator>>,
    state: TokenGeneratorState,
    node: sync::Arc<structure::Node>,
}

/*
impl TokenGenerator {
    /* for the top end */
    fn advance_up(&mut self) -> Option<token::Token>;
    fn retreat_down(&mut self);

    /* for the bottom end */
    fn advance_down(&mut self) -> Option<token::Token>;
    fn retreat_up(&mut self);
    fn hit_bottom(&self);
}
*/

/*
/// A listing window with a fixed height. Useful for scrolling by lines.
/// It is up to the user to make sure that this gets properly notified with break list invalidation events.
pub struct FixedWindow {
    flex: FlexWindow,
    
    pub lines: collections::VecDeque<Line>,
    pub top_margin: usize,
    pub window_height: usize,
    
    num_lines: usize, /* number of lines contained in line_groups */

    pub wants_update: bool,
}

/// A listing window with variable, unspecified height.
pub struct FlexWindow {
    document: document::Document,
    top: TokenGenerator,
    bottom: TokenGenerator,
}

impl FixedWindow {
    pub fn new(document: &document::Document) -> FixedWindow {
        FixedWindow {
            flex: FlexWindow::new(document, addr::Address::default()),
            
            lines: std::collections::VecDeque::<LineGroup>::new(),
            top_margin: 0,
            window_height: 0,
            
            num_lines: 0,

            wants_update: false,
        }
    }

    /// Moves the top of the window to the specified address. Returns amount
    /// window was adjusted upwards by due to hitting the bottom of the address space.
    pub fn seek(&mut self, target: addr::Address) -> usize {
        self.flex.seek(target);
        self.repopulate_window()
    }

    fn repopulate_window(&mut self) -> usize {
        self.line_groups.clear();
        self.num_lines = 0;
        self.top_margin = 0;
        
        let mut offset = 0;
        let mut hit_bottom = false;
        
        while self.num_lines < self.top_margin + self.window_height {
            if hit_bottom {
                offset+= self.scroll_up(1);
            } else {
                hit_bottom = self.produce_lines_bottom();
            }
        }

        self.wants_update = true;

        offset
    }

    /// Scrolls the window upwards. Returns amount window was actually moved
    /// by. This can be less than what was requested if the beginning or end of
    /// the address space was hit.
    pub fn scroll_up(&mut self, count: usize) -> usize {
        /* produce lines in the top margin until we can just shrink it */
        while self.top_margin < count {
            if self.produce_lines_top() {
                break
            }
        }

        /* shrink the top margin then try to trim the bottom */
        let actual = std::cmp::min(self.top_margin, count);
        self.top_margin-= actual;
        self.try_trim_bottom();

        self.wants_update = true; 
        
        actual
    }

    /// Scrolls the window downwards. Returns amount window was actually moved
    /// by. This can be less than what was requested if the beginning or end of
    /// the address space was hit.
    pub fn scroll_down(&mut self, count: usize) -> usize {
        /* grow bottom margin */
        while self.num_lines < self.top_margin + self.window_height + count {
            if self.produce_lines_bottom() {
                break
            }
        }

        /* expand top margin (shifting window) then try to trim it */
        let actual = std::cmp::min(count, self.num_lines - (self.top_margin + self.window_height));
        self.top_margin+= actual;
        self.try_trim_top();

        self.wants_update = true;
        
        actual
    }

    /// Changes the size of the window.
    pub fn resize_window(&mut self, size: usize) {
        self.window_height = size;

        while self.num_lines < self.top_margin + self.window_height {
            self.produce_lines_bottom();
        }

        self.wants_update = true;
    }

    pub fn get_window_height(&self) -> usize {
        self.window_height
    }

    pub fn get_bottom_hit_end(&self) -> bool {
        self.flex.get_bottom_hit_end()
    }

    /// Iterates over lines in the window. Outputs are tuples of (line_no,
    /// line_group). The line_no represents an offset from the top of the window
    /// and can be negative for lines in the top margin.
    pub fn iter<'a>(&'a self) -> ListingIterator<'a> {
        ListingIterator::new(self)
    }

    pub fn find_group(&self, slg: &LineGroup) -> Option<isize> {
        self.iter().find(|(_, lg)| lg == &slg).map(|t| t.0)
    }

    pub fn get_line(&self, lineno: isize) -> Option<&LineGroup> {
        self.iter().find_map(|(i, lg)| if i >= lineno { Some(lg) } else { None })
    }
    
    /* internal actions */

    /// Tries to trim as many lines as possible off the top margin, since they
    /// are outside the window. Will not fragment line groups.
    fn try_trim_top(&mut self) {
        while match self.line_groups.front() {
            std::option::Option::Some(lg) => lg.num_lines() <= self.top_margin, /* don't trim too large line groups, or don't trim if top_margin is 0 */
            std::option::Option::None => false /* don't trim if line_groups is empty */
        } {
            let lg = self.line_groups.pop_front().unwrap();

            let num_trimmed = lg.num_lines();
            self.num_lines-= num_trimmed;
            self.top_margin-= num_trimmed;

            self.flex.trim_down(&lg);
        }

        self.wants_update = true;
    }

    /// Tries to trim as many lines as possible off the bottom margin, since
    /// they are outside the window. Will not fragment line groups.
    fn try_trim_bottom(&mut self) {
        while match self.line_groups.back() {
            Some(lg) => self.top_margin + self.window_height <= self.num_lines - lg.num_lines(),
            None => false
        } {
            let lg = self.line_groups.pop_back().unwrap();
            
            self.num_lines-= lg.num_lines();

            self.flex.trim_up(&lg);
        }

        self.wants_update = true;
    }

    /// Tries to add a line group to the top margin. Returns true if no lines
    /// could be produced because the top of the address space was reached.
    fn produce_lines_top(&mut self) -> bool {
        let lg = match self.flex.produce_up() {
            Some(lg) => lg,
            None => return true
        };
        
        let nl = lg.num_lines();
        self.top_margin+= nl;
        self.num_lines+= nl;
        self.line_groups.push_front(lg);

        self.wants_update = true;
        
        false
    }

    /// Tries to add a line group to the bottom margin. Returns true if no lines
    /// could be produced because the bottom of the address space was reached.
    fn produce_lines_bottom(&mut self) -> bool {
        let lg = match self.flex.produce_down() {
            Some(lg) => lg,
            None => return true
        };
        
        self.num_lines+= lg.num_lines();
        self.line_groups.push_back(lg);

        self.wants_update = true;
        
        false
    }
    
    /* state bookkeeping */

    pub fn update(&mut self, document: &document::Document, cx: &mut task::Context) -> bool {
        let mut updated = false;
        
        if self.flex.is_outdated(document) {
            self.flex = FlexWindow::new(document, self.flex.top_view.get_addr());
            self.repopulate_window();
            updated = true;
        }

        for lg in self.line_groups.iter_mut() {
            updated = lg.update(&document, cx) || updated;
        }

        updated
    }
}

impl FlexWindow {
    pub fn new(document: &document::Document, addr: addr::Address) -> FlexWindow {
        let (top_view, bottom_view) = match document.breaks.break_at(addr) {
            brk if brk.addr == addr => {
                (BreakViewUpward::new_from_top(&document.breaks, brk.clone()),
                 BreakViewDownward::new_from_top(&document.breaks, brk.clone()))
            },
            brk => {
                (BreakViewUpward::new_from_middle(&document.breaks, brk.clone(), addr),
                 BreakViewDownward::new_from_middle(&document.breaks, brk.clone(), addr))
            }
        };

        FlexWindow {
            document: document.clone(),
            top_view,
            bottom_view
        }
    }

    /// Useful for if you need to re-create the window at a new address
    pub fn get_document(&self) -> &document::Document {
        &self.document
    }
    
    pub fn is_outdated(&self, new_document: &document::Document) -> bool {
        self.document.is_layout_outdated(new_document)
    }
    
    /// NOTE: zero-sizes the window
    pub fn seek(&mut self, target: addr::Address) {
        *self = Self::new(&self.document, target); // a shame to have to clone Document only for the original to be dropped
    }

    /// Shrinks the window by one token on the bottom.
    pub fn trim_up(&mut self) {
        self.bottom_gen.retreat_up();
    }

    /// Shrinks the window by one token on the top.
    pub fn trim_down(&mut self) {
        self.top_gen.retreat_down();
    }

    /// Tries to grow the window by one token on the top.
    pub fn produce_up(&mut self) -> Option<token::Token> {
        self.top_gen.advance_up()
    }

    /// Tries to grow the window by one line group on the bottom.
    pub fn produce_down(&mut self) -> Option<LineGroup> {
        self.bottom_gen.advance_down()
    }

    pub fn get_bottom_hit_end(&self) -> bool {
        self.bottom_gen.hit_bottom()
    }
}
*/

impl TokenGenerator {
    pub fn new_from_root(root: &sync::Arc<structure::Node>) -> TokenGenerator {
        TokenGenerator {
            stack: None,
            state: TokenGeneratorState::Clean,
            node: root.clone()
        }
    }

    pub fn advance_down(&mut self) -> Option<token::Token> {
        loop {
            match self.state {
                TokenGeneratorState::Clean => {
                    self.state = TokenGeneratorState::Title;
                    break Some(token::Token {
                        class: token::TokenClass::Null,
                        node: self.node.clone(),
                        newline: true,
                    });
                },
                TokenGeneratorState::Title => {
                    self.state = TokenGeneratorState::Content(addr::unit::ZERO, 0);
                    break Some(token::Token {
                        class: token::TokenClass::Title,
                        node: self.node.clone(),
                        newline: true,
                    });
                },
                TokenGeneratorState::Content(offset, index) => {
                    let next_child_option = self.node.children.get(index);
                    
                    /* should we descend? */
                    if let Some(next_child) = next_child_option {
                        if next_child.offset <= offset {
                            /* push our state onto the traversal stack while simultaneously seting up our state for new node */
                            let nco = next_child.offset;
                            let ncn = next_child.node.clone();
                            let parent_node = std::mem::replace(&mut self.node, ncn);
                            self.stack = Some(Box::new(TokenGenerator {
                                stack: None,
                                state: TokenGeneratorState::Content(nco + self.node.size, index + 1),
                                node: parent_node,
                            }));
                            self.state = TokenGeneratorState::Clean;
                            /* re-enter loop */
                            continue;
                        }
                    }

                    /* is there any content left? */
                    if offset < self.node.size {
                        let pitch = addr::Size::from(16);
                        
                        let (amount, end) = match next_child_option {
                            Some(next_child) => (std::cmp::min(next_child.offset - offset, pitch), false),
                            _ if self.node.size - offset <= pitch => (self.node.size - offset, true),
                            _ => (pitch, false)
                        };

                        if end {
                            self.state = TokenGeneratorState::End;
                        } else {
                            self.state = TokenGeneratorState::Content(offset + amount, index);
                        }
                        
                        break Some(token::Token {
                            class: token::TokenClass::Hexdump(addr::Extent::sized(offset.to_addr(), amount)),
                            node: self.node.clone(),
                            newline: true,
                        });
                    }
                },
                TokenGeneratorState::End => {
                    /* try to ascend hierarchy by popping the stack */
                    match std::mem::replace(&mut self.stack, None) {
                        Some(replacement) => {
                            *self = *replacement;
                            continue;
                        },
                        None => break None
                    }
                }
            }
        }
    }

    pub fn hit_bottom(&self) -> bool {
        match self.state {
            TokenGeneratorState::End => self.stack.is_none(),
            _ => false
        }
    }
}

/*
pub struct ListingIterator<'a> {
    iter: std::collections::vec_deque::Iter<'a, Line>,
    line_no: isize,
}

impl<'a> ListingIterator<'a> {
    fn new(window: &FixedWindow) -> ListingIterator {
        ListingIterator {
            iter: window.lines.iter(),
            line_no: -(window.top_margin as isize),
        }
    }
}

impl<'a> std::iter::Iterator for ListingIterator<'a> {
    type Item = (isize, &'a Line);
                 
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|lg| {
            let r = (self.line_no, lg);
            self.line_no+= lg.num_lines() as isize;
            r
        })
    }
}

impl std::fmt::Debug for FlexWindow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FlexWindow")
            .field("top_gen", &self.top_gen)
            .field("bottom_gen", &self.bottom_gen)
            .finish()
    }
}
*/

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_generator() {
        let mut root = structure::Node {
            name: "root".to_string(),
            size: addr::Size::from(0x100),
            title_display: structure::TitleDisplay::Major,
            children_display: structure::ChildrenDisplay::Full,
            content_display: structure::ContentDisplay::Hexdump(16),
            locked: true,
            children: vec::Vec::new()
        };

        let mut child = structure::Node {
            name: "child".to_string(),
            size: addr::Size::from(0x18),
            title_display: structure::TitleDisplay::Major,
            children_display: structure::ChildrenDisplay::Full,
            content_display: structure::ContentDisplay::Hexdump(16),
            locked: true,
            children: vec::Vec::new()
        };

        root.children.push(structure::Childhood {
            node: sync::Arc::new(child),
            offset: addr::Size::from(0x32)
        });

        let mut gen = TokenGenerator::new_from_root(&sync::Arc::new(root));

        while let Some(token) = gen.advance_down() {
            println!("token: {:?}", token);
        }
    }
}
