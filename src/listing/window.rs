use std::sync;
use std::vec;
use std::task;
use std::pin;

use crate::addr;
use crate::space;
use crate::listing;
use crate::listing::BreakMapExt;
use crate::listing::brk;
pub use crate::listing::line_group::LineGroup;

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

/// It is up to the user to make sure that this gets properly notified with break list invalidation events.
pub struct ListingWindow {
    micro: MicroWindow,
    
    pub line_groups: std::collections::VecDeque<LineGroup>,
    pub top_margin: usize,
    pub window_height: usize,
    
    num_lines: usize, /* number of lines contained in line_groups */
    
    wakers: vec::Vec<task::Waker>,
    has_loaded: bool,
}

pub struct MicroWindow {
    pub listing: sync::Arc<listing::Listing>,
    breaks: listing::BreakMap,
    top_view: BreakViewUpward,
    bottom_view: BreakViewDownward,
}

impl ListingWindow {
    pub fn new(listing: &sync::Arc<listing::Listing>) -> ListingWindow {
        ListingWindow {
            micro: MicroWindow::new(&listing, addr::Address::default()),
            
            line_groups: std::collections::VecDeque::<LineGroup>::new(),
            top_margin: 0,
            window_height: 0,
            
            num_lines: 0,
            
            wakers: vec::Vec::new(),
            has_loaded: false,
        }
    }

    /// Returns and clear the flag for whether any lines asynchronously loaded data and need to be re-rendered.
    pub fn clear_has_loaded(&mut self) -> bool {
        std::mem::replace(&mut self.has_loaded, false)
    }

    pub fn get_listing(&self) -> &sync::Arc<listing::Listing> {
        &self.micro.listing
    }
    
    /// Moves the top of the window to the specified address. Returns amount
    /// window was adjusted upwards by due to hitting the bottom of the address space.
    pub fn seek(&mut self, target: addr::Address) -> usize {
        self.line_groups.clear();
        self.num_lines = 0;
        self.top_margin = 0;

        self.micro.seek(target);

        let mut offset = 0;
        let mut hit_bottom = false;
        
        while self.num_lines < self.top_margin + self.window_height {
            if hit_bottom {
                offset+= self.scroll_up(1);
            } else {
                hit_bottom = self.produce_lines_bottom();
            }
        }

        self.wake();

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

        self.wake();
        
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

        self.wake();
        
        actual
    }

    /// Changes the size of the window.
    pub fn resize_window(&mut self, size: usize) {
        self.window_height = size;

        while self.num_lines < self.top_margin + self.window_height {
            self.produce_lines_bottom();
        }

        self.wake();
    }

    pub fn get_window_height(&self) -> usize {
        self.window_height
    }

    pub fn get_bottom_hit_end(&self) -> bool {
        self.micro.get_bottom_hit_end()
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

            self.micro.trim_down(&lg);
        }

        self.wake();
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

            self.micro.trim_up(&lg);
        }

        self.wake();
    }

    /// Tries to add a line group to the top margin. Returns true if no lines
    /// could be produced because the top of the address space was reached.
    fn produce_lines_top(&mut self) -> bool {
        let lg = match self.micro.produce_up() {
            Some(lg) => lg,
            None => return true
        };
        
        let nl = lg.num_lines();
        self.top_margin+= nl;
        self.num_lines+= nl;
        self.line_groups.push_front(lg);

        self.wake();
        
        false
    }

    /// Tries to add a line group to the bottom margin. Returns true if no lines
    /// could be produced because the bottom of the address space was reached.
    fn produce_lines_bottom(&mut self) -> bool {
        let lg = match self.micro.produce_down() {
            Some(lg) => lg,
            None => return true
        };
        
        self.num_lines+= lg.num_lines();
        self.line_groups.push_back(lg);

        self.wake();
        
        false
    }
    
    /* state bookkeeping */

    /// Wakes any tasks that were waiting on us to update our line list. Call
    /// whenever the line list is updated so we can start polling new lines.
    fn wake(&mut self) {
        for wk in self.wakers.drain(..) {
            wk.wake();
        }
    }

    /// Notifies us that the upstream break list may have been updated and we
    /// may need to redraw our window from scratch based on it.
    pub fn update(&mut self) -> bool {
        if self.micro.is_outdated() {
            self.seek(self.micro.top_view.get_addr());
            true
        } else {
            false
        }
    }
}

impl std::future::Future for ListingWindow {
    type Output = ();

    fn poll(mut self: pin::Pin<&mut Self>, cx: &mut task::Context) -> task::Poll<()> {
        let has_loaded = self.has_loaded;
        let line_groups = &mut self.line_groups;
        self.has_loaded = line_groups.iter_mut().fold(has_loaded, |acc, lg| lg.progress(cx) || acc);
        self.wakers.push(cx.waker().clone());
        
        task::Poll::Pending
    }
}

impl MicroWindow {
    pub fn new(listing: &sync::Arc<listing::Listing>, addr: addr::Address) -> MicroWindow {
        let breaks = listing.get_break_map().clone();
        let brk = breaks.break_at(addr);

        let top_view = BreakViewUpward::new_from_top(&breaks, brk.clone());
        let bottom_view = BreakViewDownward::new_from_top(&breaks, brk.clone());

        MicroWindow {
            listing: listing.clone(),
            breaks,
            top_view,
            bottom_view
        }
    }

    pub fn is_outdated(&self) -> bool {
        !self.breaks.ptr_eq(&self.listing.get_break_map())
    }
    
    /// NOTE: zero-sizes the window and updates breaks from upstream
    pub fn seek(&mut self, target: addr::Address) {
        self.breaks = self.listing.get_break_map().clone();
        
        match self.breaks.break_at(target) {
            brk if brk.addr == target => {
                self.top_view = BreakViewUpward::new_from_top(&self.breaks, brk.clone());
                self.bottom_view = BreakViewDownward::new_from_top(&self.breaks, brk.clone());
            },
            brk => {
                self.top_view = BreakViewUpward::new_from_middle(&self.breaks, brk.clone(), target);
                self.bottom_view = BreakViewDownward::new_from_middle(&self.breaks, brk.clone(), target);
            }
        };
    }

    /// Shrinks the window by one line group on the bottom.
    pub fn trim_up(&mut self, lg: &LineGroup) {
        while !self.bottom_view.trim(lg) {
            match self.breaks.break_before(self.bottom_view.get_break()) {
                Some(p) => self.bottom_view = BreakViewDownward::new_from_bottom(&self.breaks, p.clone()),
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
                match self.breaks.get_next(&(self.top_view.get_break().addr + addr::unit::BIT)) {
                    Some(n) => self.top_view = BreakViewUpward::new_from_top(&self.breaks, n.1.clone()),
                    None => panic!("tried to trim down to bottom of address space"),
                }
            }
        }
    }

    /// Tries to grow the window by one line group on the top.
    pub fn produce_up(&mut self) -> Option<LineGroup> {
        Some(loop {
            let lg = self.top_view.produce(&self.listing.space);

            match lg {
                Some(lg) => break lg,
                None => match self.breaks.break_before(self.top_view.get_break()) {
                    Some(p) => self.top_view = BreakViewUpward::new_from_bottom(&self.breaks, p.clone()),
                    None => return None,
                }
            }
        })
    }

    /// Tries to grow the window by one line group on the bottom.
    pub fn produce_down(&mut self) -> Option<LineGroup> {
        Some(loop {
            let lg = self.bottom_view.produce(&self.listing.space);

            match lg {
                Some(lg) => break lg,
                None => match self.breaks.break_after(self.bottom_view.get_break()) {
                    Some(n) => self.bottom_view = BreakViewDownward::new_from_top(&self.breaks, n.clone()),
                    None => return None,
                }
            }
        })
    }

    pub fn get_bottom_hit_end(&self) -> bool {
        self.bottom_view.hit_boundary() && self.breaks.break_after(self.bottom_view.get_break()).is_none()
    }
}

impl BreakViewUpward {
    pub fn new_from_top(breaks: &listing::BreakMap, brk: sync::Arc<brk::Break>) -> BreakViewUpward {
        match brk.class {
            brk::BreakClass::Hex(_) => BreakViewUpward::Hex(brk::hex::HexBreakView::<UpDir>::new_from_top(breaks, brk)),
        }
    }

    pub fn new_from_middle(breaks: &listing::BreakMap, brk: sync::Arc<brk::Break>, addr: addr::Address) -> BreakViewUpward {
        match brk.class {
            brk::BreakClass::Hex(_) => BreakViewUpward::Hex(brk::hex::HexBreakView::<UpDir>::new_from_middle(breaks, brk, addr)),
        }
    }

    pub fn new_from_bottom(breaks: &listing::BreakMap, brk: sync::Arc<brk::Break>) -> BreakViewUpward {
        match brk.class {
            brk::BreakClass::Hex(_) => BreakViewUpward::Hex(brk::hex::HexBreakView::<UpDir>::new_from_bottom(breaks, brk)),
        }
    }
}

impl BreakViewDownward {
    pub fn new_from_top(breaks: &listing::BreakMap, brk: sync::Arc<brk::Break>) -> BreakViewDownward {
        match brk.class {
            brk::BreakClass::Hex(_) => BreakViewDownward::Hex(brk::hex::HexBreakView::<DownDir>::new_from_top(breaks, brk)),
        }
    }

    pub fn new_from_middle(breaks: &listing::BreakMap, brk: sync::Arc<brk::Break>, addr: addr::Address) -> BreakViewDownward {
        match brk.class {
            brk::BreakClass::Hex(_) => BreakViewDownward::Hex(brk::hex::HexBreakView::<DownDir>::new_from_middle(breaks, brk, addr)),
        }
    }

    pub fn new_from_bottom(breaks: &listing::BreakMap, brk: sync::Arc<brk::Break>) -> BreakViewDownward {
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
    fn new(window: &ListingWindow) -> ListingIterator {
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

impl std::fmt::Debug for MicroWindow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MicroWindow")
            .field("top_view", &self.top_view)
            .field("bottom_view", &self.bottom_view)
            .finish()
    }
}
