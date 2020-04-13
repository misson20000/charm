use std::sync;
use std::vec;
use std::task;
use std::pin;

use crate::addr;
use crate::space;
use crate::space::edit;

pub mod hex_line;
pub mod break_line;
pub mod line_group;

pub use line_group::LineGroup;

//extern crate futures;

pub const LINE_SIZE: addr::Size = addr::Size { bytes: 16, bits: 0 };

// TODO: clean this up
trait BreakExt {
    fn begin_line_including(&self, addr: addr::Address) -> addr::Address;
    fn num_lines(&self) -> usize;
}

impl BreakExt for space::edit::Break {
    fn begin_line_including(&self, addr: addr::Address) -> addr::Address {
        let mut offset = addr - self.address;
        offset.bits = 0;
        offset.bytes-= offset.bytes % LINE_SIZE.bytes;
        self.address + offset
    }

    fn num_lines(&self) -> usize {
        2
    }
}

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

struct ListingInterior {
    observer: Option<sync::Arc<ListingEngineBreakObserver>>,
    
    top_address: addr::Address,
    top_break_index: Option<usize>,
    
    bottom_address: addr::Address,
    bottom_current_break_index: usize,
    bottom_next_break_index: usize,
    pub bottom_hit_end: bool,
    
    pub line_groups: std::collections::VecDeque<LineGroup>,
    pub top_margin: usize,
    pub window_height: usize,
    
    num_lines: usize, // number of lines contained in line_groups
    
    wakers: vec::Vec<task::Waker>,
}

pub struct ListingEngine {
    pub editor: sync::Arc<edit::SpaceEditor>,
    interior: sync::Mutex<ListingInterior>
}

impl ListingEngine {
    pub fn new(editor: sync::Arc<edit::SpaceEditor>, window_height: usize) -> sync::Arc<ListingEngine> {
        let le = ListingEngine {
            editor: editor.clone(),

            interior: sync::Mutex::new(ListingInterior {
                observer: None,
                
                top_address: addr::Address::default(),
                top_break_index: None,
            
                bottom_address: addr::Address::default(),
                bottom_current_break_index: 0,
                bottom_next_break_index: 0,
                bottom_hit_end: false,
            
                line_groups: std::collections::VecDeque::<LineGroup>::new(),
                top_margin: 0,
                window_height,
            
                num_lines: 0,

                wakers: vec::Vec::new()
            }),
        };
        
        le.seek(addr::Address::default());

        let arc = sync::Arc::new(le);
        {
            let mut interior = arc.interior.lock().unwrap();
        
            let observer = ListingEngineBreakObserver::new(&arc);
            editor.add_break_list_observer(&(observer.clone() as sync::Arc<dyn edit::BreakListObserver>));
            interior.observer = Some(observer);
        }
        
        arc
    }

    /// Creates a future that loads hex line contents.
    pub fn create_future(self: &sync::Arc<Self>) -> ListingFuture {
        ListingFuture { engine: sync::Arc::downgrade(&self) }
    }

    /// Moves the top of the window to the specified address. Returns amount
    /// window was adjusted by.
    pub fn seek(&self, target: addr::Address) -> usize {
        self.interior.lock().unwrap().seek(self, target)
    }

    /// Scrolls the window upwards. Returns amount window was actually moved
    /// by. This can be less than what was requested if the beginning or end of
    /// the address space was hit.
    pub fn scroll_up(&self, count: usize) -> usize {
        self.interior.lock().unwrap().scroll_up(self, count)
    }

    /// Scrolls the window downwards. Returns amount window was actually moved
    /// by. This can be less than what was requested if the beginning or end of
    /// the address space was hit.
    pub fn scroll_down(&self, count: usize) -> usize {
        self.interior.lock().unwrap().scroll_down(self, count)
    }

    /// Changes the size of the window.
    pub fn resize_window(&self, size: usize) {
        let mut interior = self.interior.lock().unwrap();
        interior.window_height = size;
        
        while interior.num_lines < interior.top_margin + interior.window_height {
            interior.produce_lines_bottom(self);
        }

        interior.wake();
    }

    pub fn get_window_height(&self) -> usize {
        let interior = self.interior.lock().unwrap();
        interior.window_height
    }

    pub fn get_bottom_hit_end(&self) -> bool {
        let interior = self.interior.lock().unwrap();
        interior.bottom_hit_end
    }

    /// Iterates over lines in the window. Outputs are tuples of (line_no,
    /// line_group). The line_no represents an offset from the top of the window
    /// and can be negative for lines in the top margin.
    pub fn iter(&self) -> ListingIterator {
        ListingIterator::new(self.interior.lock().unwrap())
    }

    /// Gets the extents of the hex line containing the target address.
    pub fn line_extents_at(&self, target: addr::Address) -> addr::Extent {
        let breaks = self.editor.get_breaks();
        
        let containing_break_index = match &breaks.binary_search_by(|b| b.address.cmp(&target)) {
            Result::Ok(idx) => *idx,
            Result::Err(idx) if *idx == 0 => panic!("somehow we're above the zero break"),
            Result::Err(idx) => idx-1
        };

        let addr = breaks[containing_break_index].begin_line_including(target);
        let size = match breaks.get(containing_break_index + 1) {
            Some(next_break) if next_break.address - addr >= LINE_SIZE => LINE_SIZE,
            Some(next_break) => next_break.address - addr,
            None if addr.is_close_to_end(LINE_SIZE) => addr.bounded_distance_to_end(LINE_SIZE),
            None => LINE_SIZE
        };

        addr::Extent::new(addr, size)
    }

    /// Gets the extents of the hex line containing the target address, and if
    /// they exist, the extents of the lines before and after.
    pub fn line_extents_near(&self, target: addr::Address) -> addr::Triplet<addr::Extent> {
        let at = self.line_extents_at(target);
        let before = if at.addr == addr::unit::NULL {
            None
        } else {
            Some(self.line_extents_at(at.addr - addr::unit::BIT))
        };

        addr::Triplet::<addr::Extent> {
            before,
            at,
            after: if at.hits_end_of_space() { None } else { Some(self.line_extents_at(at.end())) }
        }
    }

    /// Finds the line number for a given address
    pub fn find_lineno(&self, addr: addr::Address) -> LineFindResult {
        let mut last_line: Option<(isize, &LineGroup)> = None;
        
        for (no, lg) in self.iter() {
            if lg.get_addr() > addr {
                return match last_line {
                    None => LineFindResult::Before,
                    Some((lln, _)) => LineFindResult::Found(lln),
                };
            }
            last_line = Some((no, lg));
        }

        match last_line {
            Some((ln, lg)) => match &lg {
                line_group::LineGroup::Hex(hl) if hl.extent.contains(addr) => LineFindResult::Found(ln),
                _ => LineFindResult::After
            },
            _ => LineFindResult::After
        }
    }
}

impl ListingInterior {
    /* useful functionality */

    /// See ListingEngine::seek.
    fn seek(&mut self, engine: &ListingEngine, target: addr::Address) -> usize {
        let breaks = engine.editor.get_breaks();
        
        self.line_groups.clear();
        self.num_lines = 0;
        self.top_margin = 0;
        self.bottom_hit_end = false;

        let b = &breaks.binary_search_by(|b| b.address.cmp(&target));

        match b {
            Result::Ok(idx) if *idx == 0 => {
                self.top_break_index = None;
                self.top_address = addr::Address::default();

                self.bottom_current_break_index = 0; // ignored
                self.bottom_next_break_index = 0;
                self.bottom_address = addr::Address::default();
            },
            Result::Ok(idx) => {
                self.top_break_index = Some(*idx - 1);
                self.top_address = breaks[*idx].begin_line_including(target);

                self.bottom_current_break_index = *idx - 1;
                self.bottom_next_break_index = *idx;
                self.bottom_address = self.top_address;
            },
            Result::Err(idx) if *idx == 0 => {
                panic!("somehow we're above the zero break");
            },
            Result::Err(idx) => {
                self.top_break_index = Some(*idx - 1);
                self.top_address = breaks[*idx - 1].begin_line_including(target);

                self.bottom_current_break_index = *idx - 1;
                self.bottom_next_break_index = *idx;
                self.bottom_address = self.top_address;
            }
        };

        let mut offset = 0;
        
        while self.num_lines < self.top_margin + self.window_height {
            if self.bottom_hit_end {
                offset+= self.scroll_up(engine, 1);
            } else {
                self.produce_lines_bottom(engine);
            }
        }

        self.wake();

        offset
    }

    /// See ListingEngine::scroll_up.
    fn scroll_up(&mut self, engine: &ListingEngine, count: usize) -> usize {
        // produce lines in the top margin until we can just shrink it
        while self.top_margin < count {
            if self.produce_lines_top(engine) {
                break
            }
        }

        // shrink the top margin then try to trim the bottom
        let actual = std::cmp::min(self.top_margin, count);
        self.top_margin-= actual;
        self.try_trim_bottom();

        actual
    }

    /// See ListingEngine::scroll_down.
    fn scroll_down(&mut self, engine: &ListingEngine, count: usize) -> usize {
        // grow bottom margin
        while self.num_lines < self.top_margin + self.window_height + count {
            if self.produce_lines_bottom(engine) {
                break
            }
        }

        // expand top margin (shifting window) then try to trim it
        let actual = std::cmp::min(count, self.num_lines - (self.top_margin + self.window_height));
        self.top_margin+= actual;
        self.try_trim_top();

        self.wake();
        
        actual
    }

    /* internal actions */

    /// Tries to trim as many lines as possible off the top margin, since they
    /// are outside the window. Will not fragment line groups.
    fn try_trim_top(&mut self) {
        while match self.line_groups.front() {
            std::option::Option::Some(lg) => lg.num_lines() <= self.top_margin,
            std::option::Option::None => false
        } {
            let lg = self.line_groups.pop_front().unwrap();

            let num_trimmed = lg.num_lines();
            self.num_lines-= num_trimmed;
            self.top_margin-= num_trimmed;
            
            match lg {
                LineGroup::Hex(hex) => {
                    self.top_address = hex.extent.end();
                },
                LineGroup::Break(brk) => {
                    self.top_break_index = Some(brk.i);
                }
            };
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

            match lg {
                LineGroup::Hex(hex) => {
                    self.bottom_hit_end = false;
                    self.bottom_address = hex.extent.addr;
                },
                LineGroup::Break(brk) => {
                    self.bottom_current_break_index = brk.i - 1; // if i was 0, we wind up ignoring this anyway
                    self.bottom_next_break_index = brk.i;
                }
            };
        }

        self.wake();
    }

    /// Tries to add a line group to the top margin. Returns true if no lines
    /// could be produced because the top of the address space was reached.
    fn produce_lines_top(&mut self, engine: &ListingEngine) -> bool {
        let breaks = engine.editor.get_breaks();
        
        let lg = match self.top_break_index {
            None => return true,
            Some(tbi) => {
                let b = &breaks[tbi];
                if self.top_address <= b.address {
                    // regress break
                    if tbi == 0 {
                        self.top_break_index = None;
                    } else {
                        self.top_break_index = Some(tbi - 1);
                    }
                    LineGroup::make_break(tbi, b)
                } else {
                    let end_addr = self.top_address;
                    self.top_address = b.begin_line_including(self.top_address - addr::Size { bytes: 0, bits: 1});
                    LineGroup::make_line(
                        engine.editor.space.clone(),
                        addr::Extent::new(self.top_address, end_addr - self.top_address),
                        (self.top_address - b.address).bytes)
                }
            }
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
    fn produce_lines_bottom(&mut self, engine: &ListingEngine) -> bool {
        let breaks = engine.editor.get_breaks();
        
        let addr = self.bottom_address;

        if self.bottom_hit_end {
            return true;
        }
        
        let lg = if self.bottom_next_break_index >= breaks.len() || breaks[self.bottom_next_break_index].address - addr >= LINE_SIZE {
            let cb = &breaks[self.bottom_current_break_index];
            
            if self.bottom_address.is_close_to_end(LINE_SIZE) {
                // EOF
                self.bottom_hit_end = true;
                LineGroup::make_line(
                    engine.editor.space.clone(),
                    addr::Extent::new(addr, self.bottom_address.bounded_distance_to_end(LINE_SIZE)),
                    (addr - cb.address).bytes)
            } else {
                // produce a regular line
                self.bottom_address+= LINE_SIZE;
                LineGroup::make_line(
                    engine.editor.space.clone(),
                    addr::Extent::new(addr, LINE_SIZE),
                    (addr - cb.address).bytes)
            }
        } else {
            let b = &breaks[self.bottom_next_break_index];
            if b.address <= addr {
                // produce a break line group
                self.bottom_current_break_index = self.bottom_next_break_index; // advance break
                self.bottom_next_break_index+= 1;
                self.bottom_address = b.address;
                LineGroup::make_break(self.bottom_current_break_index, b)
            } else {
                // produce a truncated line
                let cb = &breaks[self.bottom_current_break_index];
                self.bottom_address = b.address;
                LineGroup::make_line(
                    engine.editor.space.clone(),
                    addr::Extent::new(addr, b.address - addr),
                    (addr - cb.address).bytes)
            }
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

    /// Notifies us that the break list was updated out from under us and we
    /// need to redraw our window from scratch.
    fn break_list_changed(&mut self, engine: &ListingEngine) {
        self.seek(engine, self.top_address);
    }
}

struct ListingEngineBreakObserver {
    engine: sync::Weak<ListingEngine>,
    editor: sync::Arc<edit::SpaceEditor>,
    self_weak: sync::Mutex<Option<sync::Weak<Self>>>,
}

impl ListingEngineBreakObserver {
    pub fn new(engine: &sync::Arc<ListingEngine>) -> sync::Arc<Self> {
        let arc = sync::Arc::new(ListingEngineBreakObserver {
            engine: sync::Arc::downgrade(engine),
            editor: engine.editor.clone(),
            self_weak: sync::Mutex::new(None)
        });

        *arc.self_weak.lock().unwrap() = Some(sync::Arc::downgrade(&arc));
        
        arc
    }
}

impl Drop for ListingEngineBreakObserver {
    fn drop(&mut self) {
        match self.self_weak.lock().unwrap().take() {
            Some(w) => self.editor.remove_break_list_observer(&(w as sync::Weak<dyn edit::BreakListObserver + 'static>)),
            //Some(w) => self.editor.remove_break_list_observer(&w),
            None => ()
        }
    }
}

impl edit::BreakListObserver for ListingEngineBreakObserver {
    fn break_list_changed(&self) {
        match self.engine.upgrade() {
            Some(engine) => engine.interior.lock().unwrap().break_list_changed(&engine),
            None => ()
        }
    }
}

pub struct ListingFuture {
    engine: sync::Weak<ListingEngine>
}

impl std::future::Future for ListingFuture {
    type Output = ();

    fn poll(self: pin::Pin<&mut Self>, cx: &mut task::Context) -> task::Poll<()> {
        match self.engine.upgrade() {
            Some(ptr) => {
                let mut interior = ptr.interior.lock().unwrap();
                interior.wakers.push(cx.waker().clone());

                for lg in &interior.line_groups {
                    lg.progress(cx);
                }
                        
                task::Poll::Pending
            }, None => task::Poll::Ready(())
        }
    }
}

/* lol https://github.com/Kimundi/owning-ref-rs/issues/27#issuecomment-285807894 */
struct Holder<T> {
    held: T
}

impl<T> std::ops::Deref for Holder<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.held
    }
}

impl<T> std::ops::DerefMut for Holder<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.held
    }
}

impl<T> Holder<T> {
    fn new(value: T) -> Holder<T> {
        Holder { held: value }
    }
}

pub struct ListingIterator<'a> {
    owning_ref: owning_ref::OwningHandle<sync::MutexGuard<'a, ListingInterior>, Holder<std::collections::vec_deque::Iter<'a, LineGroup>>>,
    lineno: isize,
}

impl<'a> ListingIterator<'a> {
    fn new(lock: sync::MutexGuard<'a, ListingInterior>) -> ListingIterator<'a> {
        ListingIterator {
            lineno: -(lock.top_margin as isize),
            owning_ref: owning_ref::OwningHandle::new_with_fn(
                lock,
                |interior| Holder::new(unsafe { (*interior).line_groups.iter() }))
        }
    }
}

impl<'a> std::iter::Iterator for ListingIterator<'a> {
    type Item = (isize, &'a LineGroup);

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.owning_ref.next();
        match item {
            Some(lg) => Some({
                let num_lines = lg.num_lines();
                let v = (self.lineno, lg);
                self.lineno+= num_lines as isize;
                v
            }),
            None => None
        }
    }
}

pub enum LineFindResult {
    Before,
    Found(isize),
    After,
}
