use std::cell;

use crate::addr;
use crate::util;
use crate::listing;
use crate::widget;
use crate::widget::listing::component;
use crate::config;

use crate::ext::CairoExt;

struct CursorRenderingContext<'a> {
    hl: &'a listing::hex_line::HexLine,
    bonk: f64,
    blink: bool,
    focus: bool,
}

#[derive(Debug, Default)]
struct NybbleCursor {
    addr: addr::Address, // byte-aligned
    low_nybble: bool,
    attempted_vertical_offset: Option<addr::Size>,
}

impl NybbleCursor {
    fn draw<'a>(&self, irc: &'a widget::listing::InternalRenderingContext<'a>, crc: &'a CursorRenderingContext<'a>) {
        let cidx = Self::char_index_for_offset(self.addr - crc.hl.extent.addr) + if self.low_nybble { 1 } else { 0 };
        let cx = irc.lw.layout.addr_pane_width + irc.pad + cidx as f64 * irc.font_extents().max_x_advance;

        let fe = irc.font_extents();
        irc.cr.set_source_gdk_rgba(irc.cfg.cursor_bg_color);
                
        if crc.focus {
            if crc.blink {
                irc.cr.rectangle(
                    cx.round() + crc.bonk,
                    fe.height - fe.ascent,
                    fe.max_x_advance,
                    fe.height);
                irc.cr.fill();
                
                irc.cr.set_source_gdk_rgba(irc.cfg.cursor_fg_color);
                irc.cr.move_to(cx, irc.font_extents().height);

                match crc.hl.byte_at((self.addr - crc.hl.extent.addr).bytes as usize) {
                    Some(b) => irc.cr.show_text(unsafe { std::str::from_utf8_unchecked(&[util::nybble_to_hex((b >> if self.low_nybble { 0 } else { 4 }) & 0xf) as u8]) }),
                    None => ()
                };
            } 
        } else {
            irc.cr.set_line_width(1.0);
            irc.cr.rectangle(
                cx.round() + crc.bonk - 0.5,
                fe.height - fe.ascent - 0.5,
                fe.max_x_advance + 1.0,
                fe.height + 1.0);
            irc.cr.stroke();
        }
    }

    fn char_index_for_offset(o: addr::Size) -> usize {
        let mut i:usize = 0;

        // add 3 for every byte
        i+= o.bytes as usize * 3;

        // add for nybbles (displayed little endian in octet, which is unfortunate)
        //i+= ((7 - o.bits) / 4) as usize;

        // add 1 for every 8 bytes (double spacing in center column)
        i+= o.bytes as usize / 8;

        return i;
    }

    fn move_left(&mut self, le: &listing::ListingEngine) -> bool {
        self.attempted_vertical_offset = None;
        
        let extents = le.editor.break_extents_near(self.addr);

        if self.low_nybble {
            self.low_nybble = false;
            false
        } else {                                              
            self.addr = match extents.before {
                // crossing extent boundaries, align with end of extent
                Some(b) if extents.at.addr == self.addr && b.size.expect("a before extent should be finite") > addr::unit::BYTE => b.addr + (self.addr - addr::unit::BIT - b.addr).floor(),
                Some(b) if extents.at.addr == self.addr => b.addr,
                _ if self.addr.magnitude() < addr::unit::BYTE => return true, // bonk
                _ => self.addr - addr::unit::BYTE
            };

            self.low_nybble = true; // bonk returns past this
            
            false
        }
    }

    fn move_right(&mut self, le: &listing::ListingEngine) -> bool {
        self.attempted_vertical_offset = None;
        
        let extents = le.editor.break_extents_near(self.addr);

        if self.low_nybble {            
            self.addr = match extents.after {
                // crossing extent boundaries, align with beginning of extent
                // TODO: handle end of address space
                Some(a) if a.addr - self.addr < addr::unit::BYTE => a.addr,
                _ if self.addr.is_close_to_end(addr::unit::BYTE) => return true,
                _ => self.addr + addr::unit::BYTE
            };

            self.low_nybble = false;
            
            false
        } else {
            self.low_nybble = true;
            false
        }
    }

    fn move_up(&mut self, le: &listing::ListingEngine) -> bool {
        let lexts = le.line_extents_near(self.addr);
        let coff = self.addr - lexts.at.addr;
        let toff = match self.attempted_vertical_offset {
            Some(avo) if avo > coff => avo,
            None => { self.attempted_vertical_offset = Some(coff); coff },
            _ => coff
        };

        match lexts.before {
            None => true,
            Some(b) if b.contains_size(toff) => { self.addr = b.addr + toff; false },
            Some(b) => { self.addr = b.addr + (b.size - addr::unit::BIT).floor(); false },
        }
    }

    fn move_down(&mut self, le: &listing::ListingEngine) -> bool {
        let lexts = le.line_extents_near(self.addr);
        let coff = self.addr - lexts.at.addr;
        let toff = match self.attempted_vertical_offset {
            Some(avo) if avo > coff => avo,
            None => { self.attempted_vertical_offset = Some(coff); coff },
            _ => coff
        };
        
        match lexts.after {
            None => true,
            Some(a) if a.contains_size(toff) => { self.addr = a.addr + toff; false },
            Some(a) => { self.addr = a.addr + (a.size - addr::unit::BIT).floor(); false },
        }
    }

    fn move_left_by_qword(&mut self, le: &listing::ListingEngine) -> bool {
        self.attempted_vertical_offset = None;
        let bexts = le.editor.break_extents_near(self.addr);

        let offset = self.addr - bexts.at.addr;
        if offset == addr::unit::ZERO && !self.low_nybble {
            if let Some(prev) = bexts.before {
                let pz = prev.size.expect("a before break should be finite");
                if pz > addr::unit::QWORD {
                    self.low_nybble = false;
                    self.addr = prev.addr + addr::Size::from((pz - addr::unit::BIT).bytes & !7); // floor to QWORD multiple
                    false
                } else {
                    self.low_nybble = false;
                    self.addr = prev.addr;
                    false
                }
            } else {
                if self.low_nybble {
                    self.low_nybble = false;
                    false
                } else {
                    true
                }
            }
        } else if self.low_nybble {
            self.addr = bexts.at.addr + addr::Size::from(offset.bytes & !7); // floor to QWORD multiple
            self.low_nybble = false;
            false
        } else {
            self.addr = bexts.at.addr + addr::Size::from((offset - addr::unit::BIT).bytes & !7); // floor to QWORD multiple
            false
        }
    }

    fn move_right_by_qword(&mut self, le: &listing::ListingEngine) -> bool {
        self.attempted_vertical_offset = None;
        let bexts = le.editor.break_extents_near(self.addr);

        let offset = addr::Size::from((self.addr - bexts.at.addr).bytes & !7); // floor to QWORD multiple
        if !bexts.at.contains_size(addr::unit::QWORD) || offset >= bexts.at.clip_size_from_end(addr::unit::QWORD).unwrap() {
            if let Some(after) = bexts.after {
                self.low_nybble = false;
                self.addr = after.addr;
                false
            } else {
                true
            }
        } else {
            self.low_nybble = false;
            self.addr = bexts.at.addr + offset + addr::unit::QWORD;
            false
        }
    }

    fn move_up_to_break(&mut self, le: &listing::ListingEngine) -> bool {
        self.attempted_vertical_offset = None;
        let bexts = le.editor.break_extents_near(self.addr);

        if bexts.at.addr < self.addr || self.low_nybble {
            self.low_nybble = false;
            self.addr = bexts.at.addr;
            false
        } else {
            match bexts.before {
                Some(b) => {
                    self.addr = b.addr;
                    self.low_nybble = false;
                    false
                },
                None if self.low_nybble => {
                    self.low_nybble = false;
                    false
                }
                None => { true }
            }
        }
    }

    fn move_down_to_break(&mut self, le: &listing::ListingEngine) -> bool {
        self.attempted_vertical_offset = None;
        let bexts = le.editor.break_extents_near(self.addr);

        match bexts.after {
            Some(b) => {
                self.addr = b.addr;
                self.low_nybble = false;
                false
            },
            None if self.addr.is_close_to_end(addr::unit::BYTE) => { true },
            None => {
                self.addr = bexts.at.addr + (addr::unit::END - bexts.at.addr).floor();
                self.low_nybble = false;
                false
            }
        }
    }

    fn move_to_start_of_line(&mut self, le: &listing::ListingEngine) -> bool {
        self.attempted_vertical_offset = None;
        
        let lexts = le.line_extents_at(self.addr);

        if self.addr == lexts.addr && !self.low_nybble {
            true
        } else {
            self.low_nybble = false;
            self.addr = lexts.addr;
            false
        }
    }

    fn move_to_end_of_line(&mut self, le: &listing::ListingEngine) -> bool {
        self.attempted_vertical_offset = None;
        
        let lexts = le.line_extents_at(self.addr);

        let target = lexts.addr + (lexts.size - addr::unit::BIT).floor();
        if self.addr == target && self.low_nybble {
            true
        } else {
            self.low_nybble = true;
            self.addr = target;
            false
        }
    }

    fn goto(&mut self, le: &listing::ListingEngine, addr: addr::Address) {
        self.attempted_vertical_offset = None;
        
        let extents = le.editor.break_extents_at(addr);
        let offset = addr - extents.addr;
        self.addr = extents.addr + (offset.floor());
    }
}

#[derive(Debug)]
struct BitCursor {
    addr: addr::Address, // no alignment
}

#[derive(Debug)]
enum CursorMode {
    Nybble(NybbleCursor),
    Bit(BitCursor),
}

impl CursorMode {
    fn draw<'a>(&self, irc: &'a widget::listing::InternalRenderingContext<'a>, crc: &'a CursorRenderingContext<'a>) {
        match self {
            CursorMode::Nybble(c) => c.draw(irc, crc),
            CursorMode::Bit(_c) => todo!("bit cursor drawing")
        }
    }

    fn get_addr(&self) -> addr::Address {
        match self {
            CursorMode::Nybble(c) => c.addr,
            CursorMode::Bit(c) => c.addr,
        }
    }

    fn move_left(&mut self, le: &listing::ListingEngine) -> bool {
        match self { CursorMode::Nybble(c) => c.move_left(le), CursorMode::Bit(_c) => todo!("bit cursor movement") } }
    fn move_right(&mut self, le: &listing::ListingEngine) -> bool {
        match self { CursorMode::Nybble(c) => c.move_right(le), CursorMode::Bit(_c) => todo!("bit cursor movement") } }
    fn move_up(&mut self, le: &listing::ListingEngine) -> bool {
        match self { CursorMode::Nybble(c) => c.move_up(le), CursorMode::Bit(_c) => todo!("bit cursor movement") } }
    fn move_down(&mut self, le: &listing::ListingEngine) -> bool {
        match self { CursorMode::Nybble(c) => c.move_down(le), CursorMode::Bit(_c) => todo!("bit cursor movement") } }
    fn move_left_by_qword(&mut self, le: &listing::ListingEngine) -> bool {
        match self { CursorMode::Nybble(c) => c.move_left_by_qword(le), CursorMode::Bit(_c) => true, /* TODO: bit cursor move left by qword */ } }
    fn move_right_by_qword(&mut self, le: &listing::ListingEngine) -> bool {
        match self { CursorMode::Nybble(c) => c.move_right_by_qword(le), CursorMode::Bit(_c) => true, /* TODO: bit cursor move right by qword */ } }
    fn move_up_to_break(&mut self, le: &listing::ListingEngine) -> bool {
        match self { CursorMode::Nybble(c) => c.move_up_to_break(le), CursorMode::Bit(_c) => true, /* TODO: bit cursor move up to break */ } }
    fn move_down_to_break(&mut self, le: &listing::ListingEngine) -> bool {
        match self { CursorMode::Nybble(c) => c.move_down_to_break(le), CursorMode::Bit(_c) => true, /* TODO: bit cursor move down to break */ } }
    fn move_to_start_of_line(&mut self, le: &listing::ListingEngine) -> bool {
        match self { CursorMode::Nybble(c) => c.move_to_start_of_line(le), CursorMode::Bit(_c) => true, /* TODO: bit cursor move to start of line */ } }
    fn move_to_end_of_line(&mut self, le: &listing::ListingEngine) -> bool {
        match self { CursorMode::Nybble(c) => c.move_to_end_of_line(le), CursorMode::Bit(_c) => true, /* TODO: bit cursor move to end of line */ } }
    fn goto(&mut self, le: &listing::ListingEngine, addr: addr::Address) {
        match self { CursorMode::Nybble(c) => c.goto(le, addr), CursorMode::Bit(c) => c.addr = addr, } }
}

#[derive(Debug)]
struct CursorInterior {
    mode: CursorMode,
    blink_timer: f64,
    bonk_timer: f64,
}

#[derive(Debug)]
pub struct Cursor {
    pub events: component::Events,

    i: cell::RefCell<CursorInterior>
}

#[derive(Debug, Copy, Clone)]
pub enum EnsureInViewDirection {
    Up,
    Down,
    Any
}

impl EnsureInViewDirection {
    pub fn maybe_upwards(&self) -> bool {
        match self {
            EnsureInViewDirection::Up => true,
            EnsureInViewDirection::Down => false,
            EnsureInViewDirection::Any => true
        }
    }

    pub fn maybe_downwards(&self) -> bool {
        match self {
            EnsureInViewDirection::Up => false,
            EnsureInViewDirection::Down => true,
            EnsureInViewDirection::Any => true
        }
    }
}

impl Cursor {
    pub fn new() -> Cursor {
        Cursor {
            events: component::Events::new(),
            i: cell::RefCell::new(CursorInterior {
                mode: CursorMode::Nybble(NybbleCursor::default()),
                blink_timer: 0.0,
                bonk_timer: 0.0,
            }),
        }
    }

    pub fn draw<'a>(&self, irc: &'a widget::listing::InternalRenderingContext<'a>, hl: &'a listing::hex_line::HexLine) {
        let i = self.i.borrow();
        
        let bonk = (i.bonk_timer / 0.25) * 3.0 * ((0.25 - i.bonk_timer) * 10.0 * 2.0 * std::f64::consts::PI).cos();
        let crc = CursorRenderingContext {
            hl,
            bonk,
            blink: i.blink_timer < irc.cfg.cursor_blink_period / 2.0,
            focus: irc.lw.has_focus
        };
        i.mode.draw(irc, &crc);
    }

    pub fn animate(&self, cfg: &config::Config, ais: f64) {
        let mut i = self.i.borrow_mut();
        
        let old_blink = i.blink_timer < cfg.cursor_blink_period / 2.0;
        i.blink_timer+= ais;
        i.blink_timer%= cfg.cursor_blink_period;
        let new_blink = i.blink_timer < cfg.cursor_blink_period / 2.0;

        if old_blink != new_blink {
            self.events.want_draw();
        }

        if i.bonk_timer > 0.0 {
            i.bonk_timer-= ais;
            i.bonk_timer = f64::max(0.0, i.bonk_timer);
            self.events.want_draw();
        } else {
            i.bonk_timer = 0.0;
        }
    }
    
    pub fn get_addr(&self) -> addr::Address {
        self.i.borrow().mode.get_addr()
    }
    
    pub fn blink(&mut self) {
        self.events.want_draw();
        self.events.want_animate();
        self.i.borrow_mut().blink_timer = 0.0;
    }

    pub fn bonk(&mut self) {
        self.i.borrow_mut().bonk_timer = 0.25;
    }

    // TODO: macro this
    pub fn move_left(&mut self, le: &listing::ListingEngine) { self.blink(); if self.i.borrow_mut().mode.move_left(le) { self.bonk(); } }
    pub fn move_right(&mut self, le: &listing::ListingEngine) { self.blink(); if self.i.borrow_mut().mode.move_right(le) { self.bonk(); } }
    pub fn move_up(&mut self, le: &listing::ListingEngine) { self.blink(); if self.i.borrow_mut().mode.move_up(le) { self.bonk(); } }
    pub fn move_down(&mut self, le: &listing::ListingEngine) { self.blink(); if self.i.borrow_mut().mode.move_down(le) { self.bonk(); } }
    pub fn move_left_by_qword(&mut self, le: &listing::ListingEngine) { self.blink(); if self.i.borrow_mut().mode.move_left_by_qword(le) { self.bonk(); } }
    pub fn move_right_by_qword(&mut self, le: &listing::ListingEngine) { self.blink(); if self.i.borrow_mut().mode.move_right_by_qword(le) { self.bonk(); } }
    pub fn move_up_to_break(&mut self, le: &listing::ListingEngine) { self.blink(); if self.i.borrow_mut().mode.move_up_to_break(le) { self.bonk(); } }
    pub fn move_down_to_break(&mut self, le: &listing::ListingEngine) { self.blink(); if self.i.borrow_mut().mode.move_down_to_break(le) { self.bonk(); } }
    pub fn move_to_start_of_line(&mut self, le: &listing::ListingEngine) { self.blink(); if self.i.borrow_mut().mode.move_to_start_of_line(le) { self.bonk(); } }
    pub fn move_to_end_of_line(&mut self, le: &listing::ListingEngine) { self.blink(); if self.i.borrow_mut().mode.move_to_end_of_line(le) { self.bonk(); } }
    
    pub fn goto(&mut self, le: &listing::ListingEngine, addr: addr::Address) {
        self.blink();
        self.i.borrow_mut().mode.goto(le, addr);
    }
}
