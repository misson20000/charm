use std::rc;
use std::sync;
use std::cell;

use crate::listing;
use crate::space;

use gtk::prelude::*;

const MICROSECONDS_PER_SECOND: f64 = 1000000.0;

struct Config {
    lookahead: usize, // lines
    scroll_wheel_impulse: f64, // lines/second
    scroll_deceleration: f64, // lines/second^2
    scroll_spring: f64, // 1/second^2
    scroll_spring_damping: f64, // viscous damping coefficient
    animation_integration_step: f64, // seconds
    
    padding: f64, // pixels
    font_size: f64, // pixels
}

pub struct ListingWidget {
    engine: listing::ListingEngine,
    config: Config,

    last_animation_time: i64,
    scroll_position: f64,
    scroll_velocity: f64,
    scroll_bonked_top: bool,
    scroll_bonked_bottom: bool,
}

impl ListingWidget {
    pub fn new(aspace: sync::Arc<dyn space::AddressSpace>) -> ListingWidget {
        ListingWidget {
            engine: listing::ListingEngine::new(aspace, 12),
            
            config: Config {
                lookahead: 5,
                scroll_wheel_impulse: 80.0,
                scroll_deceleration: 400.0,
                scroll_spring: 120.0,
                scroll_spring_damping: 17.0,
                animation_integration_step: 1.0/120.0,
                padding: 20.0,
                font_size: 13.0,
            },

            last_animation_time: 0,
            scroll_position: 0.0,
            scroll_velocity: 0.0,
            scroll_bonked_bottom: false,
            scroll_bonked_top: false,
        }
    }
    
    pub fn attach(self, da: &gtk::DrawingArea) {
        let rc: rc::Rc<cell::RefCell<Self>> = rc::Rc::new(cell::RefCell::new(self));

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
        
        { let rc_clone = rc.clone(); da.connect_draw(move |da, cr| rc_clone.borrow().draw(da, cr)); }
        { let rc_clone = rc.clone(); da.connect_scroll_event(move |da, es| rc_clone.borrow_mut().scroll_event(da, es)); }
        { let rc_clone = rc.clone(); da.connect_size_allocate(move |da, al| rc_clone.borrow_mut().size_allocate(da, al)); }

        { let rc_clone = rc.clone(); da.add_tick_callback(move |da, fc| rc_clone.borrow_mut().tick_callback(da, fc)); }

        da.add_events(gdk::EventMask::SCROLL_MASK | gdk::EventMask::SMOOTH_SCROLL_MASK);
        da.set_size_request(1300, 400);
    }
    
    pub fn draw(&self, _da: &gtk::DrawingArea, cr: &cairo::Context) -> gtk::Inhibit {
        /* our bounds are given by get_allocated_width and get_allocated_height */
        cr.set_source_rgb(0.1, 0.1, 0.1);
        cr.paint();
        
        cr.select_font_face("monospace", cairo::FontSlant::Normal, cairo::FontWeight::Normal);
        cr.set_font_size(self.config.font_size);
        cr.set_source_rgb(0.9, 1.0, 0.9);

        let mut lineno: usize = 0;
        
        let leb = &self.engine;
        for lg in leb.line_groups.iter() {
            lg.render(&leb.breaks);
            for l in lg.lines.borrow().iter() {
                let offset = (lineno as isize - leb.top_margin as isize) as f64 - self.scroll_position;
                
                if lineno >= leb.top_margin && lineno < leb.top_margin + leb.window_height {
                    cr.move_to(self.config.padding, self.config.padding + self.config.font_size * (1.0 + offset));
                    cr.show_text(l);
                }
                lineno+= 1;
            }
        }

        /* DEBUG */
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
        }
        
        gtk::Inhibit(false)
    }

    fn scroll_event(&mut self, da: &gtk::DrawingArea, es: &gdk::EventScroll) -> gtk::Inhibit {
        self.scroll_velocity+= es.get_delta().1 * self.config.scroll_wheel_impulse;
        self.scroll_bonked_top = false;
        self.scroll_bonked_bottom = false;
        da.queue_draw();
        
        gtk::Inhibit(true)
    }

    fn tick_callback(&mut self, da: &gtk::DrawingArea, fc: &gdk::FrameClock) -> Continue {
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
            if self.scroll_position > (self.config.lookahead as f64) {
                let amt_attempted = self.scroll_position as usize - self.config.lookahead;
                let amt_actual = self.engine.scroll_down(amt_attempted);
                self.scroll_position-= amt_actual as f64;
                if amt_actual < amt_attempted && self.scroll_velocity > 0.0 {
                    /* we are now bonked on the bottom... */
                    self.scroll_bonked_bottom = true;
                    todo!("bonk bottom");
                }
            }
            
            if self.scroll_position < (self.config.lookahead as f64) {
                let amt_attempted = ((self.config.lookahead as f64) - self.scroll_position) as usize;
                let amt_actual = self.engine.scroll_up(amt_attempted);
                self.scroll_position+= amt_actual as f64;
                if amt_actual < amt_attempted && self.scroll_velocity < 0.0 {
                    /* we are now bonked on the top... */
                    self.scroll_bonked_top = true;
                }
            }

            /* if we are bonked, apply spring */
            if self.scroll_bonked_top {
                self.scroll_velocity-= self.scroll_position * self.config.scroll_spring * ais;
                self.scroll_velocity-= self.scroll_velocity * self.config.scroll_spring_damping * ais;
            } else {
                /* apply constant deceleration */
                if self.scroll_velocity > 0.0 {
                    self.scroll_velocity-= self.config.scroll_deceleration * ais;
                    self.scroll_velocity = f64::max(0.0, self.scroll_velocity);
                }
                if self.scroll_velocity < 0.0 {
                    self.scroll_velocity+= self.config.scroll_deceleration * ais;
                    self.scroll_velocity = f64::min(0.0, self.scroll_velocity);
                }
            }
            
            self.last_animation_time = fc.get_frame_time();
        //}
                
        Continue(true)
    }

    fn size_allocate(&mut self, _da: &gtk::DrawingArea, al: &gtk::Rectangle) {
        let height = al.height as f64;
        let lines = f64::max((height - 2.0 * self.config.padding) / self.config.font_size, 0.0) as usize;
        let window_size =
            lines
            + 1 // round-up
            + 1 // to accomodate scrolling
            + (2 * self.config.lookahead); // lookahead works in both directions
        
        self.engine.resize_window(window_size);
    }
}
