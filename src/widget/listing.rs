use std::sync;
use std::pin;
use std::task;
use std::option;

use crate::listing;
use crate::space;
use crate::addr;

use lazy_static::lazy_static;

use hex_literal::hex;
use gtk::prelude::*;

const MICROSECONDS_PER_SECOND: f64 = 1000000.0;

pub struct Config {
    lookahead: usize, // lines
    scroll_wheel_impulse: f64, // lines/second
    scroll_deceleration: f64, // lines/second^2
    scroll_spring: f64, // 1/second^2
    scroll_spring_damping: f64, // viscous damping coefficient
    
    scroll_align_integer: bool,
    scroll_align_integer_spring: f64,
    scroll_align_integer_spring_damping: f64,
    scroll_align_position_tolerance: f64,
    scroll_align_velocity_tolerance: f64,
    
    padding: f64, // pixels
    font_size: f64, // pixels

    background_color: gdk::RGBA,
    addr_pane_color: gdk::RGBA,
    ridge_color: gdk::RGBA,

    addr_color: gdk::RGBA,
    text_color: gdk::RGBA,

    addr_pane_bold: bool,
    breaks_bold: bool,

    cursor_bg_color: gdk::RGBA,
    cursor_fg_color: gdk::RGBA,
    cursor_blink_period: f64,
    
    version: usize, // incremented when this changes
}

fn make_gdk_rgb(red: f64, blue: f64, green: f64) -> gdk::RGBA {
    gdk::RGBA { red, blue, green, alpha: 1.0 }
}

fn make_gdk_rgba(red: f64, blue: f64, green: f64, alpha: f64) -> gdk::RGBA {
    gdk::RGBA { red, blue, green, alpha }
}

fn make_gdk_black(alpha: f64) -> gdk::RGBA {
    gdk::RGBA { red: 0.0, blue: 0.0, green: 0.0, alpha }
}

fn make_gdk(bytes: [u8; 4]) -> gdk::RGBA {
    gdk::RGBA {
        red: bytes[0] as f64 / 255.0,
        green: bytes[1] as f64 / 255.0,
        blue: bytes[2] as f64 / 255.0,
        alpha: bytes[3] as f64 / 255.0
    }
}

lazy_static! {
    static ref CONFIG: sync::Mutex<Config> = sync::Mutex::new(Config {
        lookahead: 5,
        scroll_wheel_impulse: 60.0,
        scroll_deceleration: 620.0,
        scroll_spring: 240.0,
        scroll_spring_damping: 17.0,
        
        scroll_align_integer: true,
        scroll_align_integer_spring: 50.0,
        scroll_align_integer_spring_damping: 80.0,
        scroll_align_position_tolerance: 0.05,
        scroll_align_velocity_tolerance: 2.0,

        padding: 15.0,
        font_size: 14.0,

        background_color: make_gdk(hex!("090909ff")),
        addr_pane_color: make_gdk(hex!("ffffff12")),
        ridge_color: make_gdk(hex!("0000000c")),

        addr_color: make_gdk(hex!("8891efff")),
        text_color: make_gdk(hex!("dbdbe6ff")),

        addr_pane_bold: true,
        breaks_bold: true,

        cursor_bg_color: make_gdk(hex!("8891efff")),
        cursor_fg_color: make_gdk(hex!("090909ff")),
        cursor_blink_period: 1.0,
        
        version: 0,
    });
}

pub mod config {
    use gtk::prelude::*;
    
    type Config = crate::widget::listing::Config;
    
    fn edit_spinbutton_usize<F: 'static>(label: &str, default: usize, setter: F) -> gtk::ListBoxRow where
        F: Fn(&mut Config, usize) {
        let lbr = gtk::ListBoxRow::new();
        let bx = gtk::Box::new(gtk::Orientation::Horizontal, 0);
        bx.pack_start(&gtk::Label::new(Some(label)), false, false, 0);

        let sb = gtk::SpinButton::new(
            Some(&gtk::Adjustment::new(
                default as f64,
                0.0,
                f64::MAX,
                1.0,
                10.0,
                10.0
            )), 0.001, 3);
        sb.set_numeric(true);
        sb.set_snap_to_ticks(true);
        sb.set_update_policy(gtk::SpinButtonUpdatePolicy::IfValid);
        sb.set_wrap(false);
        sb.connect_value_changed(move |sb| setter(&mut crate::widget::listing::CONFIG.lock().unwrap(), sb.get_value_as_int() as usize));
        
        bx.pack_end(&sb, false, false, 0);
        lbr.add(&bx);
        
        lbr
    }

    fn edit_spinbutton_f64<F: 'static>(label: &str, default: f64, setter: F) -> gtk::ListBoxRow where
        F: Fn(&mut Config, f64) {
        let lbr = gtk::ListBoxRow::new();
        let bx = gtk::Box::new(gtk::Orientation::Horizontal, 0);
        bx.pack_start(&gtk::Label::new(Some(label)), false, false, 0);

        let sb = gtk::SpinButton::new(
            Some(&gtk::Adjustment::new(
                default,
                0.0,
                f64::MAX,
                1.0,
                10.0,
                10.0
            )), 0.001, 3);
        sb.set_numeric(true);
        sb.set_update_policy(gtk::SpinButtonUpdatePolicy::IfValid);
        sb.set_wrap(false);
        sb.connect_value_changed(move |sb| setter(&mut crate::widget::listing::CONFIG.lock().unwrap(), sb.get_value()));
        
        bx.pack_end(&sb, false, false, 0);
        lbr.add(&bx);
        
        lbr
    }

    fn edit_color<F: 'static>(label: &str, default: gdk::RGBA, setter: F) -> gtk::ListBoxRow where
        F: Fn(&mut Config, gdk::RGBA) {
        let lbr = gtk::ListBoxRow::new();
        let bx = gtk::Box::new(gtk::Orientation::Horizontal, 0);
        bx.pack_start(&gtk::Label::new(Some(label)), false, false, 0);

        let cb = gtk::ColorButtonBuilder::new()
            .rgba(&default)
            .show_editor(true)
            .title(label)
            .use_alpha(true)
            .build();
        gtk::ColorChooserExt::connect_property_rgba_notify(&cb, move |cb| setter(&mut crate::widget::listing::CONFIG.lock().unwrap(), cb.get_rgba()));
        
        bx.pack_end(&cb, false, false, 0);
        lbr.add(&bx);
        
        lbr
    }
    
    pub fn build_config_editor() -> gtk::ListBox {
        let lb = gtk::ListBox::new();

        let current = crate::widget::listing::CONFIG.lock().unwrap();
        
        lb.add(&edit_spinbutton_usize("Lookahead", current.lookahead, |cfg, v| { cfg.lookahead = v; }));
        lb.add(&edit_spinbutton_f64("Scroll Wheel Impulse", current.scroll_wheel_impulse, |cfg, v| { cfg.scroll_wheel_impulse = v; }));
        lb.add(&edit_spinbutton_f64("Scroll Deceleration", current.scroll_deceleration, |cfg, v| { cfg.scroll_deceleration = v; }));
        lb.add(&edit_spinbutton_f64("Scroll Spring", current.scroll_spring, |cfg, v| { cfg.scroll_spring = v; }));
        lb.add(&edit_spinbutton_f64("Scroll Spring Damping", current.scroll_spring_damping, |cfg, v| { cfg.scroll_spring_damping = v; }));

        lb.add(&edit_spinbutton_f64("Scroll Align Integer Spring", current.scroll_align_integer_spring, |cfg, v| { cfg.scroll_align_integer_spring = v; }));
        lb.add(&edit_spinbutton_f64("Scroll Align Position Tolerance", current.scroll_align_position_tolerance, |cfg, v| { cfg.scroll_align_position_tolerance = v; }));
        lb.add(&edit_spinbutton_f64("Scroll Align Velocity Tolerance", current.scroll_align_velocity_tolerance, |cfg, v| { cfg.scroll_align_velocity_tolerance = v; }));

        lb.add(&edit_spinbutton_f64("Padding", current.padding, |cfg, v| { cfg.padding = v; }));
        lb.add(&edit_spinbutton_f64("Font Size", current.font_size, |cfg, v| { cfg.font_size = v; }));

        lb.add(&edit_color("Background Color", current.background_color, |cfg, v| { cfg.background_color = v; }));
        lb.add(&edit_color("Address Pane Color", current.addr_pane_color, |cfg, v| { cfg.addr_pane_color = v; }));
        lb.add(&edit_color("Ridge Color", current.ridge_color, |cfg, v| { cfg.ridge_color = v; }));

        lb.add(&edit_color("Address Color", current.addr_color, |cfg, v| { cfg.addr_color = v; }));
        lb.add(&edit_color("Text Color", current.text_color, |cfg, v| { cfg.text_color = v; }));

        lb.add(&edit_color("Cursor Background Color", current.cursor_bg_color, |cfg, v| { cfg.cursor_bg_color = v; }));
        lb.add(&edit_color("Cursor Text Color", current.cursor_fg_color, |cfg, v| { cfg.cursor_fg_color = v; }));
        
        lb.show_all();
        
        return lb;
    }
}

struct InternalRenderingExtents {
    line_height: f64,
    width: f64,
    font_extents: cairo::FontExtents,
    addr_pane_width: f64,
    padding: f64,
}

#[derive(Debug)]
enum InternalRenderingPhase {
    Extents,
    Background,
    Foreground
}

pub struct ListingWidget {
    engine: listing::ListingEngine,
    rt: tokio::runtime::Handle,
    render_task: option::Option<tokio::task::JoinHandle<()>>,
    render_waker: sync::Mutex<option::Option<task::Waker>>,

    last_config_version: usize,
    last_animation_time: i64,
    
    scroll_position: f64,
    scroll_velocity: f64,
    scroll_bonked_top: bool,
    scroll_bonked_bottom: bool,

    line_height: f64,

    cursor: addr::Address,
    cursor_blink_timer: f64,
    cursor_bonk_timer: f64,
}

struct ListingPoller {
    lw: sync::Arc<sync::RwLock<ListingWidget>>
}

fn cairo_set_source_rgba(cr: &cairo::Context, rgba: gdk::RGBA) {
    cr.set_source_rgba(rgba.red, rgba.green, rgba.blue, rgba.alpha);
}

fn char_index_for_offset(o: addr::Size) -> usize {
    let mut i:usize = 0;

    // add 3 for every byte
    i+= o.bytes as usize * 3;

    // add for nybbles (displayed little endian in octet, which is unfortunate)
    i+= ((7 - o.bits) / 4) as usize;

    // add 1 for every 8 bytes (double spacing in center column)
    i+= o.bytes as usize / 8;

    return i;
}

impl ListingWidget {
    pub fn new(
        aspace: sync::Arc<dyn space::AddressSpace + Send + Sync>,
        rt: tokio::runtime::Handle) -> ListingWidget {
        ListingWidget {
            engine: listing::ListingEngine::new(aspace, 12),
            rt,
            render_task: None,
            render_waker: sync::Mutex::new(None),

            last_config_version: 0,
            last_animation_time: 0,
            
            scroll_position: 0.0,
            scroll_velocity: 0.0,
            scroll_bonked_bottom: false,
            scroll_bonked_top: false,

            line_height: 10.0,

            cursor: addr::Address::default(),
            cursor_blink_timer: 0.0,
            cursor_bonk_timer: 0.0,
        }
    }
    
    pub fn attach(self, da: &gtk::DrawingArea) {
        let rc: sync::Arc<sync::RwLock<Self>> = sync::Arc::new(sync::RwLock::new(self));

        let mut sh = rc.write().unwrap();
        (*sh).render_task = Some((*sh).rt.spawn(ListingPoller { lw: rc.clone() }));
        
        /* Events I might be interested in
        - connect_button_press_event
        - connect_button_release_event
        - connect_configure_event
        O connect_draw
        - connect_focus_in_event (animate cursor)
        - connect_focus_out_event (animate cursor)
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

        { let rc_clone = rc.clone(); da.connect_draw(move |da, cr| rc_clone.write().unwrap().draw(da, cr)); }
        { let rc_clone = rc.clone(); da.connect_key_press_event(move |da, ek| rc_clone.write().unwrap().key_press_event(da, ek)); }
        { let rc_clone = rc.clone(); da.connect_scroll_event(move |da, es| rc_clone.write().unwrap().scroll_event(da, es)); }
        { let rc_clone = rc.clone(); da.connect_size_allocate(move |da, al| rc_clone.write().unwrap().size_allocate(da, al)); }
        { let rc_clone = rc.clone(); da.add_tick_callback(move |da, fc| rc_clone.write().unwrap().tick_callback(da, fc)); }

        da.add_events(
            gdk::EventMask::SCROLL_MASK |
            gdk::EventMask::SMOOTH_SCROLL_MASK |
            gdk::EventMask::KEY_PRESS_MASK |
            gdk::EventMask::KEY_RELEASE_MASK);
        da.set_can_focus(true);
        //da.set_size_request(1300, 400);
    }

    fn setup_font(&self, cfg: &Config, cr: &cairo::Context, bold: bool) {
        cr.select_font_face(
            "monospace",
            cairo::FontSlant::Normal,
            if bold { cairo::FontWeight::Bold }
            else { cairo::FontWeight::Normal });
        cr.set_font_size(cfg.font_size);
    }
    
    pub fn draw(&mut self, da: &gtk::DrawingArea, cr: &cairo::Context) -> gtk::Inhibit {
        let cfg = CONFIG.lock().unwrap();
        
        self.setup_font(&cfg, cr, cfg.addr_pane_bold);
        let font_extents = cr.font_extents();

        if font_extents.height != self.line_height {
            self.resize_window(&cfg, da.get_allocated_height() as f64);
            self.line_height = font_extents.height;
        }

        let addr_pane_extents = cr.text_extents("0x0000000000000000.0");
        let addr_pane_width = cfg.padding * 2.0 + addr_pane_extents.width;
        
        let ire = InternalRenderingExtents {
            line_height: font_extents.height,
            width: da.get_allocated_width() as f64,
            font_extents,
            addr_pane_width,
            padding: cfg.padding,
        };

        cairo_set_source_rgba(cr, cfg.background_color);
        cr.paint();
                
        cairo_set_source_rgba(cr, cfg.addr_pane_color);
        cr.rectangle(0.0, 0.0, addr_pane_width, da.get_allocated_height() as f64);
        cr.fill();

        cr.save();
        cr.translate(0.0, cfg.padding + ire.line_height * -self.scroll_position);
        for phase in &[
            InternalRenderingPhase::Extents,
            InternalRenderingPhase::Background,
            InternalRenderingPhase::Foreground] {

            let mut lineno: usize = 0;
            for lg in self.engine.line_groups.iter() {
                cr.save();
                cr.translate(0.0, ire.line_height * ((lineno as isize - self.engine.top_margin as isize) as f64));

                match &lg.group_type {
                    listing::LineGroupType::Hex(hl) => {
                        self.draw_hex_group(&hl, &cr, phase, &cfg, &ire);
                    },
                    listing::LineGroupType::Break(bidx) => {
                        self.draw_break_group(&self.engine.breaks[*bidx], &cr, phase, &cfg, &ire);
                    },
                }

                cr.restore();
                lineno+= lg.num_lines(&self.engine.breaks);
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
    
    fn draw_hex_group(&self, hl: &listing::HexLine, cr: &cairo::Context, phase: &InternalRenderingPhase, cfg: &Config, ire: &InternalRenderingExtents) {
        match phase {
            InternalRenderingPhase::Extents => {
            },
            InternalRenderingPhase::Background => {
                // draw ridge
                if hl.distance_from_break % 0x100 == 0 {
                    cairo_set_source_rgba(cr, cfg.ridge_color);
                    cr.move_to(ire.addr_pane_width, ire.font_extents.descent);
                    cr.line_to(ire.width, ire.font_extents.descent);
                    cr.stroke();
                }
            },
            InternalRenderingPhase::Foreground => {
                // draw address in addr pane
                self.setup_font(&cfg, cr, cfg.addr_pane_bold);
                cairo_set_source_rgba(cr, cfg.addr_color);
                cr.move_to(cfg.padding, ire.line_height);
                cr.show_text(&format!("{}", hl.extent.addr));

                let text = hl.render();
                                
                // draw hex string
                self.setup_font(&cfg, cr, false);
                cairo_set_source_rgba(cr, cfg.text_color);
                cr.move_to(ire.addr_pane_width + ire.padding, ire.line_height);
                cr.show_text(&text);

                // draw cursor
                if hl.extent.contains(self.cursor) && self.cursor_blink_timer < cfg.cursor_blink_period / 2.0 {
                    let cidx = char_index_for_offset(self.cursor - hl.extent.addr);
                    let cx = ire.addr_pane_width + ire.padding + cidx as f64 * ire.font_extents.max_x_advance;
                    let bonk = (self.cursor_bonk_timer / 0.25) * 3.0 * ((0.25 - self.cursor_bonk_timer) * 10.0 * 2.0 * std::f64::consts::PI).cos();
                    
                    cairo_set_source_rgba(cr, cfg.cursor_bg_color);
                    cr.rectangle(
                        cx.round() + bonk, // x extents look better if we round them
                        ire.line_height - ire.font_extents.ascent, // y extents tend to be integer already
                        ire.font_extents.max_x_advance.round(),
                        ire.line_height);
                    cr.fill();

                    cairo_set_source_rgba(cr, cfg.cursor_fg_color);
                    cr.move_to(cx, ire.line_height);
                    cr.show_text(&text.chars().nth(cidx).unwrap().to_string());
                }
            }
        }
    }

    fn draw_break_group(&self, brk: &listing::Break, cr: &cairo::Context, phase: &InternalRenderingPhase, cfg: &Config, ire: &InternalRenderingExtents) {
        match phase {
            InternalRenderingPhase::Foreground => {
                // draw label
                self.setup_font(&cfg, cr, true);
                cairo_set_source_rgba(cr, cfg.text_color);
                cr.move_to(ire.addr_pane_width + ire.padding, ire.line_height * 2.0);
                cr.show_text(&brk.label);
            },
            _ => ()
        }
    }

    fn scroll_event(&mut self, da: &gtk::DrawingArea, es: &gdk::EventScroll) -> gtk::Inhibit {
        let cfg = CONFIG.lock().unwrap();
        
        self.scroll_velocity+= es.get_delta().1 * cfg.scroll_wheel_impulse;
        self.scroll_bonked_top = false;
        self.scroll_bonked_bottom = false;
        da.queue_draw();
        
        gtk::Inhibit(true)
    }

    fn tick_callback(&mut self, da: &gtk::DrawingArea, fc: &gdk::FrameClock) -> Continue {
        let cfg = CONFIG.lock().unwrap();

        if cfg.version > self.last_config_version {
            self.resize_window(&cfg, da.get_allocated_height() as f64);
            da.queue_draw();
            
            self.last_config_version = cfg.version;
        }
        
        if fc.get_frame_time() - self.last_animation_time > (MICROSECONDS_PER_SECOND as i64) {
            // if we fall too far behind, just drop frames
            self.last_animation_time = fc.get_frame_time();
        }

        let ais:f64 = (fc.get_frame_time() - self.last_animation_time) as f64 / MICROSECONDS_PER_SECOND;
        
        /* integrate velocity */
        self.scroll_position+= self.scroll_velocity * ais;
        if self.scroll_velocity != 0.0 { da.queue_draw(); }
        
        /* try to scroll listing engine, setting bonk flags if necessary */
        if self.scroll_position > (cfg.lookahead as f64) {
            let amt_attempted = self.scroll_position as usize - cfg.lookahead;
            let amt_actual = self.engine.scroll_down(amt_attempted);
            self.update_futures();
            self.scroll_position-= amt_actual as f64;
            if amt_actual < amt_attempted && self.scroll_velocity > 0.0 {
                /* we are now bonked on the bottom... */
                self.scroll_bonked_bottom = true;
                todo!("bonk bottom");
            }
        }
        
        if self.scroll_position < (cfg.lookahead as f64) {
            let amt_attempted = ((cfg.lookahead as f64) - self.scroll_position) as usize;
            let amt_actual = self.engine.scroll_up(amt_attempted);
            self.update_futures();
            self.scroll_position+= amt_actual as f64;
            if amt_actual < amt_attempted && self.scroll_velocity < 0.0 {
                /* we are now bonked on the top... */
                self.scroll_bonked_top = true;
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
                let target = (self.scroll_position * self.line_height).round() / self.line_height;
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

        self.cursor_blink_timer+= ais;
        self.cursor_blink_timer%= cfg.cursor_blink_period;
        self.cursor_bonk_timer-= ais;
        self.cursor_bonk_timer = f64::max(0.0, self.cursor_bonk_timer);

        self.last_animation_time = fc.get_frame_time();

        da.queue_draw(); // TODO: lol
        
        Continue(true)
    }

    fn update_futures(&self) {
        let mut waker = self.render_waker.lock().unwrap();

        let opt = std::mem::replace(&mut *waker, None);
        match opt {
            Some(wk) => wk.wake(),
            None => ()
        };
    }

    fn resize_window(&mut self, cfg: &Config, height: f64) {
        let lines = f64::max((height - 2.0 * cfg.padding) / self.line_height, 0.0) as usize;
        let window_size =
            lines
            + 1 // round-up
            + 1 // to accomodate scrolling
            + (2 * cfg.lookahead); // lookahead works in both directions
        
        self.engine.resize_window(window_size);
        self.update_futures();
    }
    
    fn size_allocate(&mut self, _da: &gtk::DrawingArea, al: &gtk::Rectangle) {
        self.resize_window(&CONFIG.lock().unwrap(), al.height as f64);
    }

    fn key_press_event(&mut self, _da: &gtk::DrawingArea, ek: &gdk::EventKey) -> gtk::Inhibit {
        match ek.get_keyval() {
            gdk::enums::key::Left => { self.cursor_move_left(); gtk::Inhibit(true) },
            gdk::enums::key::Right => { self.cursor_move_right(); gtk::Inhibit(true) },
            gdk::enums::key::Up => { self.cursor_move_up(); gtk::Inhibit(true) },
            gdk::enums::key::Down => { self.cursor_move_down(); gtk::Inhibit(true) },
            _ => gtk::Inhibit(false)
        }
    }

    fn cursor_bonk(&mut self) {
        self.cursor_bonk_timer = 0.25;
    }
    
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
    }
}

impl std::future::Future for ListingPoller {
    type Output = ();

    fn poll(self: pin::Pin<&mut Self>, cx: &mut task::Context) -> task::Poll<()> {
        let lw = self.lw.read().unwrap();
        
        let mut waker = lw.render_waker.lock().unwrap();
        std::mem::replace(&mut *waker, Some(cx.waker().clone()));

        for lg in &lw.engine.line_groups {
            lg.progress(cx);
        }
        
        task::Poll::Pending // live forever
    }
}
