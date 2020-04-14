use std::cell;

use crate::addr;
use crate::config;
use crate::listing;
use crate::widget;
use crate::widget::listing::component;

#[derive(Debug)]
struct Interior {
    position: f64,
    velocity: f64,
    bonked_top: bool,
    bonked_bottom: bool,
    cursor_spring: bool,
    cursor_direction: component::cursor::EnsureInViewDirection,
}

#[derive(Debug)]
pub struct Scroller {
    pub events: component::Events,

    i: cell::RefCell<Interior>
}

impl Scroller {
    pub fn new() -> Scroller {
        Scroller {
            events: component::Events::new(),
            i: cell::RefCell::new(Interior {
                position: 0.0,
                velocity: 0.0,
                bonked_top: false,
                bonked_bottom: false,
                cursor_spring: false,
                cursor_direction: component::cursor::EnsureInViewDirection::Any,
            }),
        }
    }

    pub fn get_position(&self) -> f64 {
        self.i.borrow().position
    }

    pub fn scroll_wheel_impulse(&mut self, delta: f64) {
        let cfg = config::get();
        let mut i = self.i.borrow_mut();
        
        i.velocity+= delta * cfg.scroll_wheel_impulse;
        i.bonked_top = false;
        i.bonked_bottom = false;

        self.events.want();
    }

    pub fn animate(&self, lw: &widget::listing::ListingWidget, ais: f64) {
        let cfg = config::get();
        let mut i = self.i.borrow_mut();
        let engine = &lw.engine;
        
        {
            /* try to scroll listing engine, setting bonk flags if necessary */

            if i.position > (cfg.lookahead as f64) {
                let amt_attempted = i.position as usize - cfg.lookahead;
                let amt_actual = engine.scroll_down(amt_attempted);
                i.position-= amt_actual as f64;
                if amt_actual < amt_attempted && i.velocity > 0.0 {
                    /* we are now bonked on the bottom... */
                    i.bonked_bottom = true;
                }
            }
            
            if i.position < (cfg.lookahead as f64) {
                let amt_attempted = ((cfg.lookahead as f64) - i.position) as usize;
                let amt_actual = engine.scroll_up(amt_attempted);
                i.position+= amt_actual as f64;
                if amt_actual < amt_attempted && i.velocity < 0.0 && i.position < 1.0 {
                    /* we are now bonked on the top... */
                    i.bonked_top = true;
                }
            }
        }

        /* if we are bonked, apply spring */
        if i.bonked_top {
            i.velocity-= i.position * cfg.scroll_spring * ais;
            i.velocity-= i.velocity * cfg.scroll_spring_damping * ais;
        } else if i.bonked_bottom {
            i.velocity-= (i.position - cfg.lookahead as f64 * 2.0 - cfg.page_navigation_leadup as f64) * cfg.scroll_spring * ais;
            i.velocity-= i.velocity * cfg.scroll_spring_damping * ais;
        } else {
            /* apply constant deceleration */
            if i.velocity > 0.0 {
                if i.velocity > cfg.scroll_deceleration * 1.0 { // never take more than 1 second to decelerate
                    i.velocity = cfg.scroll_deceleration * 1.0;
                } else {
                    i.velocity-= cfg.scroll_deceleration * ais;
                    i.velocity = f64::max(0.0, i.velocity);
                }
            }
            if i.velocity < 0.0 {
                if i.velocity < -cfg.scroll_deceleration * 1.0 { // never take more than 1 second to decelerate
                    i.velocity = -cfg.scroll_deceleration * 1.0;
                } else {
                    i.velocity+= cfg.scroll_deceleration * ais;
                    i.velocity = f64::min(0.0, i.velocity);
                }
            }
            
            /* apply alignment spring */
            if cfg.scroll_align_integer {
                let target = (i.position * lw.fonts.extents.height).round() / lw.fonts.extents.height;
                let delta = i.position - target;
                if delta.abs() < cfg.scroll_align_position_tolerance && i.velocity.abs() < cfg.scroll_align_velocity_tolerance {
                    // if we're close and we're moving slow, just snap
                    i.position = target;
                    i.velocity = 0.0;
                } else if i.velocity.abs() < cfg.scroll_align_velocity_tolerance {
                    // otherwise apply a spring force
                    i.velocity+= (target - i.position) * cfg.scroll_align_integer_spring * ais;
                    //i.velocity-= (i.velocity) * cfg.scroll_align_integer_spring_damping * ais;
                }
            }
        }

        if i.cursor_spring {
            let direction = i.cursor_direction; // evaluation order... ugh
            self.ensure_cursor_is_in_view_internal(&mut i, lw, direction);
        }

        /* integrate velocity - done last for 1 frame tighter feedback loop */
        i.position+= i.velocity * ais;
        
        if f64::abs(i.velocity) > 0.01 {
            self.events.want();
        } else {
            i.velocity = 0.0;
        }
    }
    
    pub fn ensure_cursor_is_in_view(&self, lw: &widget::listing::ListingWidget, dir: component::cursor::EnsureInViewDirection) {
        self.ensure_cursor_is_in_view_internal(&mut self.i.borrow_mut(), lw, dir);
    }

    fn ensure_cursor_is_in_view_internal(&self, i: &mut Interior, lw: &widget::listing::ListingWidget, dir: component::cursor::EnsureInViewDirection) {
        let cfg = config::get();
        
        /*
         * super threshold: move the window no matter what direction the cursor is moving
         * normal threshold: move the window if the cursor is moving out of the window
         */
        let top_super_threshold = f64::max(0.0, i.position) as isize;
        let top_threshold = top_super_threshold + cfg.page_navigation_leadup as isize;
        let bottom_super_threshold = lw.engine.get_window_height() as isize + f64::max(0.0, i.position) as isize - 2 * cfg.lookahead as isize;
        let bottom_threshold = bottom_super_threshold - cfg.page_navigation_leadup as isize;
        let cln = lw.engine.find_lineno(lw.cursor.get_addr());
        
        i.cursor_direction = dir;
        
        match cln {
            listing::LineFindResult::Before => {
                self.seek_internal(i, lw, lw.cursor.get_addr());
            },
            listing::LineFindResult::Found(ln) if ln < top_threshold && i.position > 1.0 && (dir.maybe_upwards() || ln < top_super_threshold) => {
                i.bonked_top = false;
                i.bonked_bottom = false;
                i.cursor_spring = true;
                i.velocity = -((top_threshold - ln) as f64) * 20.0;

                self.events.want_animate();
            },
            listing::LineFindResult::Found(ln) if ln > bottom_threshold && (dir.maybe_downwards() || ln > bottom_super_threshold) => {
                i.bonked_top = false;
                i.bonked_bottom = false;
                i.cursor_spring = true;
                i.velocity = (ln - bottom_threshold) as f64 * 20.0;

                self.events.want_animate();
            },
            listing::LineFindResult::After => {
                self.seek_internal(i, lw, lw.cursor.get_addr());
            },
            _ => {
                i.cursor_spring = false;
            }
        }
    }

    pub fn seek(&mut self, lw: &widget::listing::ListingWidget, addr: addr::Address) {
        self.seek_internal(&mut self.i.borrow_mut(), lw, addr);
    }
    
    fn seek_internal(&self, i: &mut Interior, lw: &widget::listing::ListingWidget, addr: addr::Address) {
        let cfg = config::get();
        
        lw.engine.seek(addr);
        if lw.engine.get_bottom_hit_end() {
            i.position = cfg.lookahead as f64 * 2.0 + cfg.page_navigation_leadup as f64;
            i.bonked_bottom = true;
        }  else {
            i.position = 0.0; // let lookahead logic in tick handler work this out
            i.bonked_bottom = false;
        }
        i.velocity = 0.0;
        i.bonked_top = false;
        i.cursor_spring = false;
        lw.engine.scroll_up(cfg.page_navigation_leadup);
    }
    
    pub fn page_up(&self, lw: &widget::listing::ListingWidget) {
        let cfg = config::get();
        let mut i = self.i.borrow_mut();
        
        // kinematics 101
        i.bonked_bottom = false;
        i.velocity = -f64::sqrt(i.velocity.powi(2) + 2.0 * cfg.scroll_deceleration * (lw.engine.get_window_height() - 2 * cfg.lookahead - 2 * cfg.page_navigation_leadup) as f64);
    }

    pub fn page_down(&self, lw: &widget::listing::ListingWidget) {
        let cfg = config::get();
        let mut i = self.i.borrow_mut();
        
        // kinematics 101
        i.bonked_top = false;
        i.velocity = f64::sqrt(i.velocity.powi(2) + 2.0 * cfg.scroll_deceleration * (lw.engine.get_window_height() - 2 * cfg.lookahead - 2 * cfg.page_navigation_leadup) as f64);
    }
}
