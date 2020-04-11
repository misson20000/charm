use std::sync;
use std::pin;
use std::task;
use std::option;

use crate::listing;
use crate::space;
use crate::addr;
use crate::config;

use crate::ext::CairoExt;

use gtk::prelude::*;

const MICROSECONDS_PER_SECOND: f64 = 1000000.0;

#[derive(Debug)]
enum InternalRenderingPhase {
    Extents,
    Background,
    Foreground
}

struct InternalRenderingContext<'a> {
    lw: &'a ListingWidget,
    cr: &'a cairo::Context,
    phase: InternalRenderingPhase,
    cfg: &'a config::Config,

    pad: f64,
}

impl<'a> InternalRenderingContext<'a> {
    fn font_extents(&self) -> &'a cairo::FontExtents {
        &self.lw.fonts.extents
    }
}

struct CursorRenderingContext<'a> {
    hl: &'a listing::HexLine,
    bonk: f64,
    blink: bool,
    focus: bool,
}

struct NybbleCursor {
    addr: addr::Address, // byte-aligned
    low_nybble: bool,
}

impl NybbleCursor {
    fn draw<'a>(&self, irc: &'a InternalRenderingContext<'a>, crc: &'a CursorRenderingContext<'a>) {
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
                /* TODO: redraw byte
                cr.set_source_gdk_rgba(irc.cfg.cursor_fg_color);
                cr.move_to(cx, irc.line_height);
                cr.show_text(&text.chars().nth(cidx).unwrap().to_string());
                 */
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
}

struct BitCursor {
    addr: addr::Address, // no alignment
}

enum CursorMode {
    Nybble(NybbleCursor),
    Bit(BitCursor),
}

impl CursorMode {
    fn draw<'a>(&self, irc: &'a InternalRenderingContext<'a>, crc: &'a CursorRenderingContext<'a>) {
        match self {
            CursorMode::Nybble(c) => c.draw(irc, crc),
            CursorMode::Bit(c) => todo!("bit cursor drawing")
        }
    }

    fn get_addr(&self) -> addr::Address {
        match self {
            CursorMode::Nybble(c) => c.addr,
            CursorMode::Bit(c) => c.addr,
        }
    }
}

struct Cursor {
    mode: CursorMode,
    blink_timer: f64,
    bonk_timer: f64,
}

impl Cursor {
    fn new() -> Cursor {
        Cursor {
            mode: CursorMode::Nybble(NybbleCursor {
                addr: addr::Address::default(),
                low_nybble: false,
            }),
            blink_timer: 0.0,
            bonk_timer: 0.0,
        }
    }

    fn draw<'a>(&self, irc: &'a InternalRenderingContext<'a>, hl: &'a listing::HexLine) {
        let bonk = (self.bonk_timer / 0.25) * 3.0 * ((0.25 - self.bonk_timer) * 10.0 * 2.0 * std::f64::consts::PI).cos();
        let crc = CursorRenderingContext {
            hl,
            bonk,
            blink: self.blink_timer < irc.cfg.cursor_blink_period / 2.0,
            focus: irc.lw.has_focus
        };
        self.mode.draw(irc, &crc);
    }

    fn animate(&mut self, cfg: &config::Config, ais: f64) {
        self.blink_timer+= ais;
        self.blink_timer%= cfg.cursor_blink_period;
        self.bonk_timer-= ais;
        self.bonk_timer = f64::max(0.0, self.bonk_timer);
    }
    
    fn get_addr(&self) -> addr::Address {
        self.mode.get_addr()
    }
    
    fn blink(&mut self) {
        self.blink_timer = 0.0;
    }

    fn bonk(&mut self) {
        self.bonk_timer = 0.5;
    }
}

struct Fonts {
    mono_face: cairo::FontFace,
    bold_face: cairo::FontFace,
    mono_scaled: cairo::ScaledFont,
    bold_scaled: cairo::ScaledFont,

    extents: cairo::FontExtents,
}

impl Fonts {
    fn new(cfg: &config::Config) -> Fonts {
        let mono_face = cairo::FontFace::toy_create("monospace", cairo::FontSlant::Normal, cairo::FontWeight::Normal);
        let bold_face = cairo::FontFace::toy_create("monospace", cairo::FontSlant::Normal, cairo::FontWeight::Bold);
        
        let mut font_matrix = cairo::Matrix::identity();
        font_matrix.scale(cfg.font_size, cfg.font_size);
        let ctm = cairo::Matrix::identity();
        let options = cairo::FontOptions::new();

        let mono_scaled = cairo::ScaledFont::new(&mono_face, &font_matrix, &ctm, &options);
        let bold_scaled = cairo::ScaledFont::new(&bold_face, &font_matrix, &ctm, &options);
        
        Fonts {
            extents: mono_scaled.extents(),
            
            mono_face,
            bold_face,
            mono_scaled,
            bold_scaled,
        }
    }
}

struct Layout {
    width: f64,
    height: f64,
    addr_pane_width: f64,
}

pub struct ListingWidget {
    engine: sync::Arc<sync::RwLock<listing::ListingEngine>>,
    rt: tokio::runtime::Handle,
    update_task: option::Option<tokio::task::JoinHandle<()>>,

    last_config_version: usize,
    last_animation_time: i64,
    
    scroll_position: f64,
    scroll_velocity: f64,
    scroll_bonked_top: bool,
    scroll_bonked_bottom: bool,

    has_focus: bool,

    fonts: Fonts,
    layout: Layout,

    cursor: Cursor,
}

impl ListingWidget {
    pub fn new(
        aspace: sync::Arc<dyn space::AddressSpace + Send + Sync>,
        rt: tokio::runtime::Handle) -> ListingWidget {
        let cfg = config::get();
        ListingWidget {
            engine: sync::Arc::new(
                sync::RwLock::new(
                    listing::ListingEngine::new(aspace, 12))),
            rt,
            update_task: None,

            last_config_version: 0,
            last_animation_time: 0,
            
            scroll_position: 0.0,
            scroll_velocity: 0.0,
            scroll_bonked_bottom: false,
            scroll_bonked_top: false,

            has_focus: false,

            fonts: Fonts::new(&cfg),
            layout: Layout {
                width: 0.0,
                height: 0.0,
                addr_pane_width: 0.0,
            },

            cursor: Cursor::new(),
        }
    }
    
    pub fn attach(self, da: &gtk::DrawingArea) {
        let rc: sync::Arc<sync::RwLock<Self>> = sync::Arc::new(sync::RwLock::new(self));

        let mut sh = rc.write().unwrap();
        (*sh).update_task = Some(
            (*sh).rt.spawn(
                listing::ListingFuture::new(
                    sync::Arc::downgrade(&sh.engine))));

        sh.has_focus = da.is_focus();
        
        /* Events I might be interested in
        O connect_button_press_event
        - connect_button_release_event
        - connect_configure_event
        O connect_draw
        O connect_focus_in_event (animate cursor)
        O connect_focus_out_event (animate cursor)
        O connect_key_press_event
        - connect_key_release_event
        - connect_popup_menu
        - connect_screen_changed (for hidpi)
        O connect_scroll_event
        - connect_selection_clear_event
        - connect_selection_get
        - connect_selection_notify_event
        - connect_selection_received
        - connect_selection_request_event
        O connect_size_allocate
         */

        { let rc_clone = rc.clone(); da.connect_button_press_event(move |da, eb| rc_clone.write().unwrap().button_event(da, eb)); }
        { let rc_clone = rc.clone(); da.connect_button_release_event(move |da, eb| rc_clone.write().unwrap().button_event(da, eb)); }
        { let rc_clone = rc.clone(); da.connect_draw(move |da, cr| rc_clone.write().unwrap().draw(da, cr)); }
        { let rc_clone = rc.clone(); da.connect_focus_in_event(move |da, ef| rc_clone.write().unwrap().focus_change_event(da, ef)); }
        { let rc_clone = rc.clone(); da.connect_focus_out_event(move |da, ef| rc_clone.write().unwrap().focus_change_event(da, ef)); }
        { let rc_clone = rc.clone(); da.connect_key_press_event(move |da, ek| rc_clone.write().unwrap().key_press_event(da, ek)); }
        { let rc_clone = rc.clone(); da.connect_scroll_event(move |da, es| rc_clone.write().unwrap().scroll_event(da, es)); }
        { let rc_clone = rc.clone(); da.connect_size_allocate(move |da, al| rc_clone.write().unwrap().size_allocate(da, al)); }
        { let rc_clone = rc.clone(); da.add_tick_callback(move |da, fc| rc_clone.write().unwrap().tick_callback(da, fc)); }

        da.add_events(
            gdk::EventMask::FOCUS_CHANGE_MASK |
            gdk::EventMask::SCROLL_MASK |
            gdk::EventMask::SMOOTH_SCROLL_MASK |
            gdk::EventMask::KEY_PRESS_MASK |
            gdk::EventMask::KEY_RELEASE_MASK |
            gdk::EventMask::BUTTON_PRESS_MASK |
            gdk::EventMask::BUTTON_RELEASE_MASK
        );
        da.set_can_focus(true);
        da.set_focus_on_click(true);

        sh.reconfigure(da, &config::get());
        
        //da.set_size_request(1300, 400);
    }

    pub fn draw<'a>(&'a mut self, da: &gtk::DrawingArea, cr: &cairo::Context) -> gtk::Inhibit {
        let cfg = config::get();
                
        let irc = InternalRenderingContext {
            lw: &self,
            cr,
            phase: InternalRenderingPhase::Extents,
            cfg: &cfg,

            pad: cfg.padding, // it is helpful to have a shorthand for this one
        };

        /* fill background */
        cr.set_source_gdk_rgba(irc.cfg.background_color);
        cr.paint();

        /* fill address pane */
        cr.set_source_gdk_rgba(cfg.addr_pane_color);
        cr.rectangle(0.0, 0.0, self.layout.addr_pane_width, self.layout.height);
        cr.fill();

        /* render lines in each pass */
        cr.save();
        cr.translate(0.0, irc.pad + irc.font_extents().height * -self.scroll_position);
        for phase in &[
            InternalRenderingPhase::Extents,
            InternalRenderingPhase::Background,
            InternalRenderingPhase::Foreground] {

            let le = self.engine.read().unwrap();
            let mut lineno: usize = 0;
            for lg in le.line_groups.iter() {
                cr.save();
                cr.translate(0.0, irc.font_extents().height * ((lineno as isize - le.top_margin as isize) as f64));

                match &lg.group_type {
                    listing::LineGroupType::Hex(hl) => {
                        self.draw_hex_group(&hl, &cr, phase, &cfg, &irc);

                        /* draw cursor */
                        if hl.extent.contains(self.cursor.get_addr()) {
                            self.cursor.draw(&irc, &hl);
                        }
                    },
                    listing::LineGroupType::Break(bidx) => {
                        self.draw_break_group(&le.breaks[*bidx], &cr, phase, &cfg, &irc);
                    },
                }

                cr.restore();
                lineno+= lg.num_lines(&le.breaks);
            }
        }
        cr.restore();

        /* DEBUG */
        /*
        let debug = vec![
            format!("lookahead: {}", self.config.lookahead),
            format!("scroll position: {} lines", self.scroll_position),
            format!("scroll velocity: {}", self.scroll_velocity),
            format!("scroll bonked on top: {}", self.scroll_bonked_top),
            format!("scroll bonked on bottom: {}", self.scroll_bonked_bottom),
        ];
        lineno = 0;
        for line in debug {
            cr.move_to(1000.0, 40.0 + (self.config.font_size * lineno as f64));
            cr.show_text(&line);
            lineno+= 1;
        }*/
        
        gtk::Inhibit(false)
    }
    
    fn draw_hex_group<'a>(&self, hl: &listing::HexLine, cr: &cairo::Context, phase: &InternalRenderingPhase, cfg: &config::Config, irc: &'a InternalRenderingContext<'a>) {
        match phase {
            InternalRenderingPhase::Extents => {
            },
            InternalRenderingPhase::Background => {
                // draw ridge
                if hl.distance_from_break % 0x100 == 0 {
                    cr.set_source_gdk_rgba(cfg.ridge_color);
                    cr.move_to(self.layout.addr_pane_width, irc.font_extents().descent);
                    cr.line_to(self.layout.width, irc.font_extents().descent);
                    cr.stroke();
                }
            },
            InternalRenderingPhase::Foreground => {
                // draw address in addr pane
                cr.set_scaled_font(&self.fonts.bold_scaled);
                cr.set_source_gdk_rgba(cfg.addr_color);
                cr.move_to(irc.pad, irc.font_extents().height);
                cr.show_text(&format!("{}", hl.extent.addr));

                let text = hl.render();
                                
                // draw hex string
                cr.set_scaled_font(&self.fonts.mono_scaled);
                cr.set_source_gdk_rgba(cfg.text_color);
                cr.move_to(self.layout.addr_pane_width + irc.pad, irc.font_extents().height);
                cr.show_text(&text);
            }
        }
    }

    fn draw_break_group<'a>(&self, brk: &listing::Break, cr: &cairo::Context, phase: &InternalRenderingPhase, cfg: &config::Config, irc: &'a InternalRenderingContext<'a>) {
        match phase {
            InternalRenderingPhase::Foreground => {
                // draw label
                cr.set_scaled_font(&self.fonts.bold_scaled);
                cr.set_source_gdk_rgba(cfg.text_color);
                cr.move_to(self.layout.addr_pane_width + irc.pad, irc.font_extents().height * 2.0);
                cr.show_text(&brk.label);
            },
            _ => ()
        }
    }

    fn scroll_event(&mut self, da: &gtk::DrawingArea, es: &gdk::EventScroll) -> gtk::Inhibit {
        let cfg = config::get();
        
        self.scroll_velocity+= es.get_delta().1 * cfg.scroll_wheel_impulse;
        self.scroll_bonked_top = false;
        self.scroll_bonked_bottom = false;
        da.queue_draw();
        
        gtk::Inhibit(true)
    }
    
    fn tick_callback(&mut self, da: &gtk::DrawingArea, fc: &gdk::FrameClock) -> Continue {
        let cfg = config::get();

        if cfg.version > self.last_config_version {
            self.reconfigure(da, &cfg);
            da.queue_draw();
        }
        
        if fc.get_frame_time() - self.last_animation_time > (MICROSECONDS_PER_SECOND as i64) {
            // if we fall too far behind, just drop frames
            self.last_animation_time = fc.get_frame_time();
        }

        let ais:f64 = (fc.get_frame_time() - self.last_animation_time) as f64 / MICROSECONDS_PER_SECOND;
        
        /* integrate velocity */
        self.scroll_position+= self.scroll_velocity * ais;
        if self.scroll_velocity != 0.0 { da.queue_draw(); }

        {
            /* try to scroll listing engine, setting bonk flags if necessary */

            let mut le = self.engine.write().unwrap();
            if self.scroll_position > (cfg.lookahead as f64) {
                let amt_attempted = self.scroll_position as usize - cfg.lookahead;
                let amt_actual = le.scroll_down(amt_attempted);
                self.scroll_position-= amt_actual as f64;
                if amt_actual < amt_attempted && self.scroll_velocity > 0.0 {
                    /* we are now bonked on the bottom... */
                    self.scroll_bonked_bottom = true;
                    todo!("bonk bottom");
                }
            }
            
            if self.scroll_position < (cfg.lookahead as f64) {
                let amt_attempted = ((cfg.lookahead as f64) - self.scroll_position) as usize;
                let amt_actual = le.scroll_up(amt_attempted);
                self.scroll_position+= amt_actual as f64;
                if amt_actual < amt_attempted && self.scroll_velocity < 0.0 {
                    /* we are now bonked on the top... */
                    self.scroll_bonked_top = true;
                }
            }
        }

        /* if we are bonked, apply spring */
        if self.scroll_bonked_top {
            self.scroll_velocity-= self.scroll_position * cfg.scroll_spring * ais;
            self.scroll_velocity-= self.scroll_velocity * cfg.scroll_spring_damping * ais;
        } else {
            /* apply constant deceleration */
            if self.scroll_velocity > 0.0 {
                self.scroll_velocity-= cfg.scroll_deceleration * ais;
                self.scroll_velocity = f64::max(0.0, self.scroll_velocity);
            }
            if self.scroll_velocity < 0.0 {
                self.scroll_velocity+= cfg.scroll_deceleration * ais;
                self.scroll_velocity = f64::min(0.0, self.scroll_velocity);
            }
            /* apply alignment spring */
            if cfg.scroll_align_integer {
                let target = (self.scroll_position * self.fonts.extents.height).round() / self.fonts.extents.height;
                let delta = self.scroll_position - target;
                if delta.abs() < cfg.scroll_align_position_tolerance && self.scroll_velocity.abs() < cfg.scroll_align_velocity_tolerance {
                    // if we're close and we're moving slow, just snap
                    self.scroll_position = target;
                    self.scroll_velocity = 0.0;
                } else if self.scroll_velocity.abs() < cfg.scroll_align_velocity_tolerance {
                    // otherwise apply a spring force
                    self.scroll_velocity+= (target - self.scroll_position) * cfg.scroll_align_integer_spring * ais;
                    //self.scroll_velocity-= (self.scroll_velocity) * cfg.scroll_align_integer_spring_damping * ais;
                }
            }
        }

        self.cursor.animate(&cfg, ais);

        self.last_animation_time = fc.get_frame_time();

        da.queue_draw(); // TODO: lol
        
        Continue(true)
    }

    fn reconfigure(&mut self, da: &gtk::DrawingArea, cfg: &config::Config) {
        self.fonts = Fonts::new(cfg);

        self.layout.width = da.get_allocated_width() as f64;
        self.layout.height = da.get_allocated_height() as f64;
        self.layout.addr_pane_width = {
            let addr_pane_extents = self.fonts.bold_scaled.text_extents("0x0000000000000000.0");
            cfg.padding * 2.0 + addr_pane_extents.width
        };
        
        self.resize_window(&cfg);
        self.last_config_version = cfg.version;
    }
    
    fn resize_window(&mut self, cfg: &config::Config) {
        let lines = f64::max((self.layout.height - 2.0 * cfg.padding) / self.fonts.extents.height, 0.0) as usize;
        let window_size =
            lines
            + 1 // round-up
            + 1 // to accomodate scrolling
            + (2 * cfg.lookahead); // lookahead works in both directions
        
        self.engine.write().unwrap().resize_window(window_size);
    }

    fn button_event(self: sync::RwLockWriteGuard<Self>, da: &gtk::DrawingArea, eb: &gdk::EventButton) -> gtk::Inhibit {
        std::mem::drop(self); // grab_focus triggers focus_change_event before we return
        match eb.get_event_type() {
            gdk::EventType::ButtonPress => {
                da.grab_focus();
            },
            _ => ()
        }
        da.queue_draw();

        gtk::Inhibit(false)
    }

    fn focus_change_event(&mut self, da: &gtk::DrawingArea, ef: &gdk::EventFocus) -> gtk::Inhibit {
        self.cursor.blink();
        self.has_focus = ef.get_in();
        da.queue_draw();

        gtk::Inhibit(false)
    }
    
    fn size_allocate(&mut self, _da: &gtk::DrawingArea, al: &gtk::Rectangle) {
        self.layout.width = al.width as f64;
        self.layout.height = al.height as f64;
        
        self.resize_window(&config::get());
    }

    fn key_press_event(&mut self, _da: &gtk::DrawingArea, ek: &gdk::EventKey) -> gtk::Inhibit {
        match ek.get_keyval() {
            /*
            gdk::enums::key::Left => { self.cursor_move_left(); gtk::Inhibit(true) },
            gdk::enums::key::Right => { self.cursor_move_right(); gtk::Inhibit(true) },
            gdk::enums::key::Up => { self.cursor_move_up(); gtk::Inhibit(true) },
            gdk::enums::key::Down => { self.cursor_move_down(); gtk::Inhibit(true) },
             */
            _ => gtk::Inhibit(false)
        }
    }

    /*
    fn cursor_move_left(&mut self) {
        let extents = self.engine.break_extents_near(self.cursor);
        self.cursor_blink_timer = 0.0;
        if (self.cursor - extents.at.addr).bits < 4 {
            // if we're in a low nybble, move to the high nybble (this moves the
            // cursor to the left because big endian)
            self.cursor+= addr::unit::NYBBLE;
        } else {
            // move from high nybble to low nybble of previous byte
            if self.cursor < extents.at.addr + addr::unit::BYTE_NYBBLE {
                // if we're crossing an extent boundary, put the cursor eight bits from the end of the extent
                match extents.before {
                    Some(b) => {
                        self.cursor = std::cmp::min(b.addr, b.end().expect("before extent should not be infinite") - addr::unit::BYTE);
                    },
                    None => self.cursor_bonk()
                }
            } else {
                self.cursor-= addr::Size { bytes: 1, bits: 4 };
            }
        }
    }

    fn cursor_move_right(&mut self) {
        let lexts = self.engine.line_extents_near(self.cursor);
        self.cursor_blink_timer = 0.0;
        if (self.cursor - lexts.at.addr).bits < 4 {
            self.cursor+= addr::Size { bytes: 1, bits: 4 };
        } else {
            self.cursor-= addr::unit::NYBBLE;
        }
    }

    fn cursor_move_up(&mut self) {
        let lexts = self.engine.line_extents_near(self.cursor);
        self.cursor_blink_timer = 0.0;
        match lexts.before {
            None => self.cursor_bonk(),
            Some(b) => {
                self.cursor = b.addr + std::cmp::min(std::cmp::max(b.size, addr::unit::BIT) - addr::unit::BIT, self.cursor - lexts.at.addr);
            }
        }
    }

    fn cursor_move_down(&mut self) {
        let lexts = self.engine.line_extents_near(self.cursor);
        self.cursor_blink_timer = 0.0;
        match lexts.after {
            None => self.cursor_bonk(),
            Some(a) => {
                self.cursor = a.addr + std::cmp::min(std::cmp::max(a.size, addr::unit::BIT) - addr::unit::BIT, self.cursor - lexts.at.addr);
            }
        }
    }*/
}
