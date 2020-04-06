use std::sync;
use std::vec;
use std::string;
use std::task;

use std::future::Future;

use crate::addr;
use crate::space;

//extern crate futures;

const LINE_SIZE: u64 = 16;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct Break {
    address: addr::Address,
    label: string::String,
}

impl Break {
    fn begin_line_including(&self, addr: addr::Address) -> addr::Address {
        let mut offset = addr - self.address;
        offset.bits = 0;
        offset.bytes-= offset.bytes % LINE_SIZE;
        self.address + offset
    }

    fn num_lines(&self) -> usize {
        2
    }

    fn render<'a>(&self, vec: &'a mut vec::Vec<string::String>) {
        vec.clear();
        vec.push(string::String::new());
        vec.push(format!("{}:", self.label));
    }
}

enum HexLineAsyncState {
    Pending(std::pin::Pin<Box<dyn Future<Output = space::FetchResult> + Send + Sync>>),
    Finished(space::FetchResult)
}

struct HexLine {
    addr: addr::Address,
    size: addr::Size,
    internal: sync::RwLock<HexLineAsyncState>
}

impl HexLine {
    fn num_lines(&self) -> usize {
        1
    }

    fn progress(&self, cx: &mut task::Context) {
        let mut ih = self.internal.write().unwrap();
        match &mut *ih {
            HexLineAsyncState::Pending(future) => {
                println!("HexLine {} polling", self.addr);
                match future.as_mut().poll(cx) {
                    task::Poll::Ready(result) => { std::mem::replace(&mut *ih, HexLineAsyncState::Finished(result)); },
                    task::Poll::Pending => ()
                }
            },
            HexLineAsyncState::Finished(_) => ()
        }
    }
    
    fn render<'out>(&'out self, vec: &'out mut vec::Vec<string::String>) {
        vec.clear();
        
        let bytes = self.size.round_up().bytes as usize;
        let ih = self.internal.read().unwrap();
        
        let (state, region) = match *ih {
            HexLineAsyncState::Finished(space::FetchResult::Ok(ref data)) => ("Ok", &data[..bytes]),
            HexLineAsyncState::Finished(space::FetchResult::Partial(ref data, amt)) => ("Partial", &data[..std::cmp::min(amt, bytes)]),
            HexLineAsyncState::Finished(space::FetchResult::Unreadable) => ("Unreadable", &[] as &[u8]),
            HexLineAsyncState::Finished(space::FetchResult::IoError(_)) => ("IO Error", &[] as &[u8]),
            HexLineAsyncState::Pending(_) => ("Pending", &[] as &[u8])
        };
        let mut str = std::string::String::new();
        str+= &format!("{} ", self.addr);
        str+= &std::str::from_utf8(&[' ' as u8; 20]).unwrap()[0..20 - str.len()];
        
        for i in 0..(LINE_SIZE as usize) {
            if i == 8 {
                str+= " ";
            }
            if i < region.len() {
                str+= &format!("{:02x} ", region[i]);
            } else {
                str+= "   ";
            }
        }
        str+= "| ";
        for i in 0..(LINE_SIZE as usize) {
            if i == 8 {
                str+= " ";
            }
            if i < region.len() && (region[i] as char).is_ascii_alphanumeric() {
                str.push(region[i] as char)
            } else {
                str+= ".";
            }
        }
        str+= "  ";
        str+= state;
        vec.push(str);
    }
}

enum LineGroupType {
    Hex(HexLine),
    Break(usize) // index
}

pub struct LineGroup {
    space: sync::Arc<dyn space::AddressSpace + Send + Sync>,
    group_type: LineGroupType,
    pub lines: sync::RwLock<vec::Vec<string::String>>,
}

impl LineGroup {
    fn make_break(space: sync::Arc<dyn space::AddressSpace + Send + Sync>, index: usize) -> LineGroup {
        LineGroup {
            space,
            group_type: LineGroupType::Break(index),
            lines: sync::RwLock::new(vec![]),
        }
    }
    
    fn make_line(space: sync::Arc<dyn space::AddressSpace + Send + Sync>, a: addr::Address, s: addr::Size) -> LineGroup {
        LineGroup {
            space: space.clone(),
            group_type: LineGroupType::Hex(HexLine {
                addr: a, size: s,
                internal: sync::RwLock::new(
                    HexLineAsyncState::Pending(
                        Box::pin(space.fetch(a, s, vec![0; (LINE_SIZE + 1) as usize]))))
            }),
            lines: sync::RwLock::new(vec![]),
        }
    }
    
    fn num_lines<'b>(&'b self, breaks: &'b vec::Vec<Break>) -> usize {
        match self.group_type {
            LineGroupType::Hex(ref hex) => hex.num_lines(),
            LineGroupType::Break(i) => breaks[i].num_lines()
        }
    }

    pub fn progress(&self, cx: &mut task::Context) {
        match self.group_type {
            LineGroupType::Hex(ref hex) => hex.progress(cx),
            LineGroupType::Break(_) => ()
        }
    }

    
    pub fn render<'b>(&'b self, breaks: &'b vec::Vec<Break>) {
        match self.group_type {
            LineGroupType::Hex(ref hex) => hex.render(&mut self.lines.write().unwrap()),
            LineGroupType::Break(i) => breaks[i].render(&mut self.lines.write().unwrap())
        }
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
top_margin (measured in lines):
 [] 0x00400080  00 11 22 33 44 55 66 77  88 99 aa bb cc dd ee ff
 [] 0x00400090  00 11 22 33 44 55 66 77  88 99 aa bb cc dd ee ff
 [] 0x004000a0  00 11 22 33 44 55 66 77  88 99 aa bb cc dd ee ff
 [  0x004000b0  [intentionally empty line]
    0x004000b0  label2:
top_margin + window_height: (measured in lines)
  ] 0x004000b0  00 11 22 33 44 55 66 77  88 99 aa bb cc dd ee ff
 [] 0x004000c0  00 11 22 33 44 55 66 77  88 99 aa bb cc dd ee ff
num_lines: (this is the extent of our buffer)
    bottom_address 0x004000d0

We don't ever fragment "line groups" with the same address.
 */

pub struct ListingEngine {
    space: sync::Arc<dyn space::AddressSpace + Send + Sync>,

    pub breaks: vec::Vec<Break>,
    
    top_address: addr::Address,
    top_break_index: Option<usize>,
    
    bottom_address: addr::Address,
    bottom_current_break_index: usize,
    bottom_next_break_index: usize,

    pub line_groups: std::collections::VecDeque<LineGroup>,
    pub top_margin: usize,
    pub window_height: usize,

    num_lines: usize, // number of lines contained in line_groups
}

impl ListingEngine {
    pub fn new(space: sync::Arc<dyn space::AddressSpace + Send + Sync>, window_height: usize) -> ListingEngine {
        let mut le = ListingEngine {
            space,
            breaks: vec![
                Break { address: addr::Address { byte: 0x00000000, bit: 0}, label: "zero break".to_string() },
                Break { address: addr::Address { byte: 0x00000010, bit: 1}, label: "first break".to_string() },
                Break { address: addr::Address { byte: 0x00000048, bit: 0}, label: "second break".to_string() }
            ],

            top_address: addr::Address::default(),
            top_break_index: None,
            bottom_address: addr::Address::default(),
            bottom_current_break_index: 0,
            bottom_next_break_index: 0,
            line_groups: std::collections::VecDeque::<LineGroup>::new(),
            num_lines: 0,
            top_margin: 0,
            window_height
        };
        le.seek(addr::Address::default());
        le
    }

    #[cfg(feature = "test_listing")]
    pub fn test_engine(space: sync::Arc<dyn space::AddressSpace + Send + Sync>) {
        let mut le = ListingEngine::new(space, 12);
        
        ncurses::initscr();
        ncurses::raw();
        ncurses::keypad(ncurses::stdscr(), true);
        ncurses::noecho();

        loop {
            le.test_engine_render();
            ncurses::refresh();

            match ncurses::getch() {
                ncurses::KEY_UP => {le.scroll_up(1);},
                ncurses::KEY_DOWN => {le.scroll_down(1);},
                113 => break, // q
                _ => ()
            };
        }

        ncurses::endwin();
    }
    
    #[cfg(feature = "test_listing")]
    fn test_engine_render(&mut self) {
        let mut lineno: usize = 0;

        ncurses::erase();
        ncurses::mv(10 - self.top_margin as i32, 0);
        ncurses::addstr(" begin\n");

        {
            
            for lg in self.line_groups.iter() {
                lg.render(&self.breaks);
                for l in lg.lines.read().unwrap().iter() {
                    if lineno == self.top_margin {
                        ncurses::addstr("  top margin\n");
                    }
                    if lineno == self.top_margin + self.window_height {
                        ncurses::addstr("  top margin + window height\n");
                    }
                    ncurses::addstr(&format!("    | {}\n", l));
                    lineno+= 1;
                }
            }
        }
        if lineno == self.top_margin {
            ncurses::addstr("  top margin\n");
        }
        if lineno == self.top_margin + self.window_height {
            ncurses::addstr("  top margin + window height\n");
        }
        ncurses::addstr(" end\n");
    }
    
    fn seek(&mut self, target: addr::Address) {
        self.line_groups.clear();
        self.num_lines = 0;
        self.top_margin = 0;

        let b = &self.breaks.binary_search_by(|b| b.address.cmp(&target));

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
                self.top_address = self.breaks[*idx].begin_line_including(target);

                self.bottom_current_break_index = *idx - 1;
                self.bottom_next_break_index = *idx;
                self.bottom_address = self.top_address;
            },
            Result::Err(idx) if *idx == 0 => {
                panic!("somehow we're above the zero break");
            },
            Result::Err(idx) => {
                self.top_break_index = Some(*idx - 1);
                self.top_address = self.breaks[*idx - 1].begin_line_including(target);

                self.bottom_current_break_index = *idx - 1;
                self.bottom_next_break_index = *idx;
                self.bottom_address = self.top_address;
            }
        };
        
        while self.num_lines < self.top_margin + self.window_height {
            self.produce_lines_bottom();
        }
    }

    // returns amount actually scrolled (less on bonk)
    pub fn scroll_up(&mut self, count: usize) -> usize {
        // produce lines in the top margin until we can just shrink it
        while self.top_margin < count {
            if self.produce_lines_top() {
                break
            }
        }

        // shrink the top margin then try to trim the bottom
        let actual = std::cmp::min(self.top_margin, count);
        self.top_margin-= actual;
        self.try_trim_bottom();

        actual
    }

    // returns amount actually scrolled (less on bonk)
    pub fn scroll_down(&mut self, count: usize) -> usize {
        // grow bottom margin
        while self.num_lines < self.top_margin + self.window_height + count {
            if self.produce_lines_bottom() {
                break
            }
        }

        // expand top margin (shifting window) then try to trim it
        let actual = std::cmp::min(count, self.num_lines - (self.top_margin + self.window_height));
        self.top_margin+= actual;
        self.try_trim_top();

        actual
    }

    fn try_trim_top(&mut self) {
        while match self.line_groups.front() {
            std::option::Option::Some(lg) => lg.num_lines(&self.breaks) <= self.top_margin,
            std::option::Option::None => false
        } {
            let lg = self.line_groups.pop_front().unwrap();

            let num_trimmed = lg.num_lines(&self.breaks);
            self.num_lines-= num_trimmed;
            self.top_margin-= num_trimmed;
            
            match lg.group_type {
                LineGroupType::Hex(hex) => {
                    self.top_address = hex.addr + hex.size;
                },
                LineGroupType::Break(i) => {
                    self.top_break_index = Some(i);
                }
            };
        }
    }

    fn try_trim_bottom(&mut self) {
        while match self.line_groups.back() {
            Some(lg) => self.top_margin + self.window_height <= self.num_lines - lg.num_lines(&self.breaks),
            None => false
        } {
            let lg = self.line_groups.pop_back().unwrap();
            
            self.num_lines-= lg.num_lines(&self.breaks);

            match lg.group_type {
                LineGroupType::Hex(hex) => {
                    self.bottom_address = hex.addr;
                },
                LineGroupType::Break(i) => {
                    self.bottom_current_break_index = i - 1; // if i was 0, we wind up ignoring this anyway
                    self.bottom_next_break_index = i;
                }
            };
        }
    }

    pub fn resize_window(&mut self, size: usize) {
        self.window_height = size;
        
        while self.num_lines < self.top_margin + self.window_height {
            self.produce_lines_bottom();
        }
    }

    fn produce_lines_top(&mut self) -> bool { // returns true on EOF
        let lg = match self.top_break_index {
            None => return true,
            Some(tbi) => {
                let b = &self.breaks[tbi];
                if self.top_address <= b.address {
                    // regress break
                    if tbi == 0 {
                        self.top_break_index = None;
                    } else {
                        self.top_break_index = Some(tbi - 1);
                    }
                    LineGroup::make_break(self.space.clone(), tbi)
                } else {
                    let end_addr = self.top_address;
                    self.top_address = b.begin_line_including(self.top_address - addr::Size { bytes: 0, bits: 1});
                    LineGroup::make_line(self.space.clone(), self.top_address, end_addr - self.top_address)
                }
            }
        };

        let nl = lg.num_lines(&self.breaks);
        self.top_margin+= nl;
        self.num_lines+= nl;
        self.line_groups.push_front(lg);

        false
    }
    
    fn produce_lines_bottom(&mut self) -> bool { // returns true on EOF
        let addr = self.bottom_address;
        
        let lg = if self.bottom_next_break_index >= self.breaks.len() || self.breaks[self.bottom_next_break_index].address >= addr + LINE_SIZE {
            // produce a regular line
            self.bottom_address+= LINE_SIZE;
            LineGroup::make_line(self.space.clone(), addr, addr::Size::from(LINE_SIZE))
        } else {
            let b = &self.breaks[self.bottom_next_break_index];
            if b.address <= addr {
                // produce a break line group
                self.bottom_current_break_index = self.bottom_next_break_index; // advance break
                self.bottom_next_break_index+= 1;
                self.bottom_address = b.address;
                LineGroup::make_break(self.space.clone(), self.bottom_current_break_index)
            } else {
                // produce a truncated line
                self.bottom_address = b.address;
                LineGroup::make_line(self.space.clone(), addr, b.address - addr)
            }
        };
        
        self.num_lines+= lg.num_lines(&self.breaks);
        self.line_groups.push_back(lg);

        false
    }
}
