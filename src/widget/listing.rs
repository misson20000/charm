use std::sync;
use std::pin;
use std::task;
use std::option;

use crate::listing;
use crate::space;

use lazy_static::lazy_static;

use gtk::prelude::*;

const MICROSECONDS_PER_SECOND: f64 = 1000000.0;

pub struct Config {
    lookahead: usize, // lines
    scroll_wheel_impulse: f64, // lines/second
    scroll_deceleration: f64, // lines/second^2
    scroll_spring: f64, // 1/second^2
    scroll_spring_damping: f64, // viscous damping coefficient
    
    padding: f64, // pixels
    font_size: f64, // pixels

    background_color: gdk::RGBA,
    ridge_color: gdk::RGBA,
    text_color: gdk::RGBA,
    
    version: usize, // incremented when this changes
}

fn make_gdk_rgb(red: f64, blue: f64, green: f64) -> gdk::RGBA {
    gdk::RGBA { red, blue, green, alpha: 1.0 }
}

fn make_gdb_black(alpha: f64) -> gdk::RGBA {
    gdk::RGBA { red: 0.0, blue: 0.0, green: 0.0, alpha }
}

lazy_static! {
    static ref CONFIG: sync::Mutex<Config> = sync::Mutex::new(Config {
        lookahead: 5,
        scroll_wheel_impulse: 60.0,
        scroll_deceleration: 620.0,
        scroll_spring: 240.0,
        scroll_spring_damping: 17.0,

        padding: 10.0,
        font_size: 14.0,

        background_color: make_gdk_rgb(0.1, 0.1, 0.1),
        ridge_color: make_gdb_black(0.05),
        text_color: make_gdk_rgb(0.86, 0.9, 0.86),

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

        lb.add(&edit_spinbutton_f64("Padding", current.padding, |cfg, v| { cfg.padding = v; }));
        lb.add(&edit_spinbutton_f64("Font Size", current.font_size, |cfg, v| { cfg.font_size = v; }));

        lb.add(&edit_color("Background Color", current.background_color, |cfg, v| { cfg.background_color = v; }));
        lb.add(&edit_color("Ridge Color", current.ridge_color, |cfg, v| { cfg.ridge_color = v; }));
        lb.add(&edit_color("Text Color", current.text_color, |cfg, v| { cfg.text_color = v; }));
        
        lb.show_all();
        
        return lb;
    }
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
}

struct ListingPoller {
    lw: sync::Arc<sync::RwLock<ListingWidget>>
}

fn cairo_set_source_rgba(cr: &cairo::Context, rgba: gdk::RGBA) {
    cr.set_source_rgba(rgba.red, rgba.green, rgba.blue, rgba.alpha);
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
        - connect_draw
        - connect_focus_in_event (animate cursor)
        - connect_focus_out_event (animate cursor)
        - connect_key_press_event
        - connect_key_release_event
        - connect_popup_menu
        - connect_screen_changed (for hidpi)
        - connect_scroll_event
        - connect_selection_clear_event
        - connect_selection_get
        - connect_selection_notify_event
        - connect_selection_received
        - connect_selection_request_event
        - connect_size_allocate
         */

        { let rc_clone = rc.clone(); da.connect_draw(move |da, cr| rc_clone.read().unwrap().draw(da, cr)); }
        { let rc_clone = rc.clone(); da.connect_scroll_event(move |da, es| rc_clone.write().unwrap().scroll_event(da, es)); }
        { let rc_clone = rc.clone(); da.connect_size_allocate(move |da, al| rc_clone.write().unwrap().size_allocate(da, al)); }
        { let rc_clone = rc.clone(); da.add_tick_callback(move |da, fc| rc_clone.write().unwrap().tick_callback(da, fc)); }

        da.add_events(gdk::EventMask::SCROLL_MASK | gdk::EventMask::SMOOTH_SCROLL_MASK);
        da.set_size_request(1300, 400);
    }

    fn setup_font(&self, cfg: &Config, cr: &cairo::Context) {
        cr.select_font_face("monospace", cairo::FontSlant::Normal, cairo::FontWeight::Normal);
        cr.set_font_size(cfg.font_size);
    }
    
    pub fn draw(&self, da: &gtk::DrawingArea, cr: &cairo::Context) -> gtk::Inhibit {
        let cfg = CONFIG.lock().unwrap();
        
        /* our bounds are given by get_allocated_width and get_allocated_height */
        cairo_set_source_rgba(cr, cfg.background_color);
        cr.paint();

        self.setup_font(&cfg, cr);
        let extents = cr.font_extents();
        
        let mut lineno: usize = 0;
        
        let leb = &self.engine;
        for lg in leb.line_groups.iter() {
            lg.render(&leb.breaks);
            for l in lg.lines.read().unwrap().iter() {
                let offset = (lineno as isize - leb.top_margin as isize) as f64 - self.scroll_position;
                
                if lineno >= leb.top_margin && lineno < leb.top_margin + leb.window_height {
                    let y = cfg.padding + extents.height * offset;
                    cairo_set_source_rgba(cr, cfg.text_color);
                    cr.move_to(cfg.padding, y + extents.height);
                    cr.show_text(l);

                    match &lg.group_type {
                        listing::LineGroupType::Hex(hl) => {
                            if hl.distance_from_break % 0x100 == 0 {
                                cairo_set_source_rgba(cr, cfg.ridge_color);
                                cr.move_to(0.0, y + extents.descent);
                                cr.line_to(da.get_allocated_width() as f64, y + extents.descent);
                                cr.stroke();
                            }
                        },
                        _ => ()
                    }
                }
                lineno+= 1;
            }
        }

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
            self.size_allocate(da, &da.get_allocation());
            da.queue_draw();
            
            self.last_config_version = cfg.version;
        }
        
        if fc.get_frame_time() - self.last_animation_time > (MICROSECONDS_PER_SECOND as i64) {
            // if we fall too far behind, just drop frames
            self.last_animation_time = fc.get_frame_time();
        }

        let ais:f64 = (fc.get_frame_time() - self.last_animation_time) as f64 / MICROSECONDS_PER_SECOND;
        
        //while self.last_animation_time < fc.get_frame_time() {
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
            }
            
            self.last_animation_time = fc.get_frame_time();
        //}

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
    
    fn size_allocate(&mut self, _da: &gtk::DrawingArea, al: &gtk::Rectangle) {
        let cfg = CONFIG.lock().unwrap();
        
        let height = al.height as f64;
        let lines = f64::max((height - 2.0 * cfg.padding) / cfg.font_size, 0.0) as usize;
        let window_size =
            lines
            + 1 // round-up
            + 1 // to accomodate scrolling
            + (2 * cfg.lookahead); // lookahead works in both directions
        
        self.engine.resize_window(window_size);
        self.update_futures();
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
