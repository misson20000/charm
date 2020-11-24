use std::sync;
use std::task;

use crate::model::addr;
use crate::model::space;
use crate::model::document;
use crate::model::document::BreakMapExt;
use crate::model::document::brk;
use crate::model::listing::line_group::LineGroup;

use enum_dispatch::enum_dispatch;

/*

A listing looks like this:

start of buffer:
    top_address 0x00400050
 [  0x00400050  [intentionally empty line]
    0x00400050  label:
  ] 0x00400050  00 11 22 33 44 55 66 77  88 99 aa bb cc dd ee ff
 [] 0x00400070  10 21 32 43 54 65 76 87  98 a9 ba cb dc ed fe 0f
top_margin (measured in lines): (everything above this is outside the window)
 [] 0x00400080  00 11 22 33 44 55 66 77  88 99 aa bb cc dd ee ff
 [] 0x00400090  00 11 22 33 44 55 66 77  88 99 aa bb cc dd ee ff
 [] 0x004000a0  00 11 22 33 44 55 66 77  88 99 aa bb cc dd ee ff
 [  0x004000b0  [intentionally empty line]
    0x004000b0  label2:
top_margin + window_height: (measured in lines) (everything below this is outside the window)
  ] 0x004000b0  00 11 22 33 44 55 66 77  88 99 aa bb cc dd ee ff
 [] 0x004000c0  00 11 22 33 44 55 66 77  88 99 aa bb cc dd ee ff
num_lines: (this is the extent of our buffer)
    bottom_address 0x004000d0

We don't ever fragment "line groups".
 */

#[enum_dispatch]
pub trait BreakView {
    fn produce(&mut self, space: &sync::Arc<dyn space::AddressSpace>) -> Option<LineGroup>;

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

// TODO: replace this with const generics when they actually work
pub trait BreakViewDir {}
pub struct UpDir;
pub struct DownDir;
impl BreakViewDir for UpDir {}
impl BreakViewDir for DownDir {}

#[enum_dispatch(BreakView)]
#[derive(Debug)]
pub enum BreakViewUpward {
    Hex(brk::hex::HexBreakView<UpDir>),
}

#[enum_dispatch(BreakView)]
#[derive(Debug)]
pub enum BreakViewDownward {
    Hex(brk::hex::HexBreakView<DownDir>),
}

/// A listing window with a fixed height. Useful for scrolling by lines.
/// It is up to the user to make sure that this gets properly notified with break list invalidation events.
pub struct FixedWindow {
    flex: FlexWindow,
    
    pub line_groups: std::collections::VecDeque<LineGroup>,
    pub top_margin: usize,
    pub window_height: usize,
    
    num_lines: usize, /* number of lines contained in line_groups */

    pub wants_update: bool,
}

/// A listing window with variable, unspecified height.
pub struct FlexWindow {
    document: document::Document,
    top_view: BreakViewUpward,
    bottom_view: BreakViewDownward,
}

impl FixedWindow {
    pub fn new(document: &document::Document) -> FixedWindow {
        FixedWindow {
            flex: FlexWindow::new(document, addr::Address::default()),
            
            line_groups: std::collections::VecDeque::<LineGroup>::new(),
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
            updated = lg.update(document, cx) || updated;
        }

        updated
    }
}

impl FlexWindow {
    pub fn new(document: &document::Document, addr: addr::Address) -> FlexWindow {
        let (top_view, bottom_view) = match document.get_breaks().break_at(addr) {
            brk if brk.addr == addr => {
                (BreakViewUpward::new_from_top(&document.get_breaks(), brk.clone()),
                 BreakViewDownward::new_from_top(&document.get_breaks(), brk.clone()))
            },
            brk => {
                (BreakViewUpward::new_from_middle(&document.get_breaks(), brk.clone(), addr),
                 BreakViewDownward::new_from_middle(&document.get_breaks(), brk.clone(), addr))
            }
        };

        FlexWindow {
            document: document.clone(),
            top_view,
            bottom_view
        }
    }

    pub fn get_document(&self) -> &document::Document {
        &self.document
    }
    
    /// Useful for if you need to re-create the window at a new address
    pub fn get_breaks(&self) -> &document::BreakMap {
        &self.document.get_breaks()
    }

    /// Useful for if you need to re-create the window at a new address
    pub fn get_space(&self) -> &sync::Arc<dyn space::AddressSpace> {
        &self.document.get_space()
    }

    pub fn is_outdated(&self, new_document: &document::Document) -> bool {
        &self.document != new_document
    }
    
    /// NOTE: zero-sizes the window
    pub fn seek(&mut self, target: addr::Address) {
        *self = Self::new(&self.document, target); // a shame to have to clone Document only for the original to be dropped
    }

    /// Shrinks the window by one line group on the bottom.
    pub fn trim_up(&mut self, lg: &LineGroup) {
        while !self.bottom_view.trim(lg) {
            match self.document.get_breaks().break_before(self.bottom_view.get_break()) {
                Some(p) => self.bottom_view = BreakViewDownward::new_from_bottom(&self.document.get_breaks(), p.clone()),
                None => panic!("tried to trim up to top of address space"),
            }
        }
    }

    /// Shrinks the window by one line group on the top.
    pub fn trim_down(&mut self, lg: &LineGroup) {
        while !self.top_view.trim(lg) {
            if self.top_view.get_break().addr == addr::unit::REAL_END {
                panic!("tried to trim down to bottom of address space");
            } else {
                match self.document.get_breaks().get_next(&(self.top_view.get_break().addr + addr::unit::BIT)) {
                    Some(n) => self.top_view = BreakViewUpward::new_from_top(&self.document.get_breaks(), n.1.clone()),
                    None => panic!("tried to trim down to bottom of address space"),
                }
            }
        }
    }

    /// Tries to grow the window by one line group on the top.
    pub fn produce_up(&mut self) -> Option<LineGroup> {
        Some(loop {
            let lg = self.top_view.produce(&self.document.get_space());

            match lg {
                Some(lg) => break lg,
                None => match self.document.get_breaks().break_before(self.top_view.get_break()) {
                    Some(p) => self.top_view = BreakViewUpward::new_from_bottom(&self.document.get_breaks(), p.clone()),
                    None => return None,
                }
            }
        })
    }

    /// Tries to grow the window by one line group on the bottom.
    pub fn produce_down(&mut self) -> Option<LineGroup> {
        Some(loop {
            let lg = self.bottom_view.produce(&self.document.get_space());

            match lg {
                Some(lg) => break lg,
                None => match self.document.get_breaks().break_after(self.bottom_view.get_break()) {
                    Some(n) => self.bottom_view = BreakViewDownward::new_from_top(&self.document.get_breaks(), n.clone()),
                    None => return None,
                }
            }
        })
    }

    pub fn get_bottom_hit_end(&self) -> bool {
        self.bottom_view.hit_boundary() && self.document.get_breaks().break_after(self.bottom_view.get_break()).is_none()
    }
}

impl BreakViewUpward {
    pub fn new_from_top(breaks: &document::BreakMap, brk: sync::Arc<brk::Break>) -> BreakViewUpward {
        match brk.class {
            brk::BreakClass::Hex(_) => BreakViewUpward::Hex(brk::hex::HexBreakView::<UpDir>::new_from_top(breaks, brk)),
        }
    }

    pub fn new_from_middle(breaks: &document::BreakMap, brk: sync::Arc<brk::Break>, addr: addr::Address) -> BreakViewUpward {
        match brk.class {
            brk::BreakClass::Hex(_) => BreakViewUpward::Hex(brk::hex::HexBreakView::<UpDir>::new_from_middle(breaks, brk, addr)),
        }
    }

    pub fn new_from_bottom(breaks: &document::BreakMap, brk: sync::Arc<brk::Break>) -> BreakViewUpward {
        match brk.class {
            brk::BreakClass::Hex(_) => BreakViewUpward::Hex(brk::hex::HexBreakView::<UpDir>::new_from_bottom(breaks, brk)),
        }
    }
}

impl BreakViewDownward {
    pub fn new_from_top(breaks: &document::BreakMap, brk: sync::Arc<brk::Break>) -> BreakViewDownward {
        match brk.class {
            brk::BreakClass::Hex(_) => BreakViewDownward::Hex(brk::hex::HexBreakView::<DownDir>::new_from_top(breaks, brk)),
        }
    }

    pub fn new_from_middle(breaks: &document::BreakMap, brk: sync::Arc<brk::Break>, addr: addr::Address) -> BreakViewDownward {
        match brk.class {
            brk::BreakClass::Hex(_) => BreakViewDownward::Hex(brk::hex::HexBreakView::<DownDir>::new_from_middle(breaks, brk, addr)),
        }
    }

    pub fn new_from_bottom(breaks: &document::BreakMap, brk: sync::Arc<brk::Break>) -> BreakViewDownward {
        match brk.class {
            brk::BreakClass::Hex(_) => BreakViewDownward::Hex(brk::hex::HexBreakView::<DownDir>::new_from_bottom(breaks, brk)),
        }
    }
}

pub struct ListingIterator<'a> {
    iter: std::collections::vec_deque::Iter<'a, LineGroup>,
    line_no: isize,
}

impl<'a> ListingIterator<'a> {
    fn new(window: &FixedWindow) -> ListingIterator {
        ListingIterator {
            iter: window.line_groups.iter(),
            line_no: -(window.top_margin as isize),
        }
    }
}

impl<'a> std::iter::Iterator for ListingIterator<'a> {
    type Item = (isize, &'a LineGroup);
                 
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
            .field("top_view", &self.top_view)
            .field("bottom_view", &self.bottom_view)
            .finish()
    }
}
