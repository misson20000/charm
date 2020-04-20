use crate::addr;
use crate::config;
use crate::listing;
use crate::widget::listing::component;

#[derive(Debug)]
pub struct Scroller {
    pub events: component::Events,

    position: f64,
    velocity: f64,
    bonked_top: bool,
    bonked_bottom: bool,
    //cursor_spring: bool,
    //cursor_direction: component::cursor::EnsureInViewDirection,
}

impl Scroller {
    pub fn new() -> Scroller {
        Scroller {
            events: component::Events::new(),
            position: 0.0,
            velocity: 0.0,
            bonked_top: false,
            bonked_bottom: false,
            //cursor_spring: false,
            //cursor_direction: component::cursor::EnsureInViewDirection::Any,
        }
    }

    pub fn get_position(&self) -> f64 {
        self.position
    }

    pub fn scroll_wheel_impulse(&mut self, delta: f64) {
        let cfg = config::get();
        
        self.velocity+= delta * cfg.scroll_wheel_impulse;
        self.bonked_top = false;
        self.bonked_bottom = false;

        self.events.want();
    }

    pub fn animate(&mut self, window: &mut listing::window::ListingWindow, ais: f64) {
        let cfg = config::get();
        
        {
            /* try to scroll listing engine, setting bonk flags if necessary */

            if self.position > (cfg.lookahead as f64) {
                let amt_attempted = self.position as usize - cfg.lookahead;
                let amt_actual = window.scroll_down(amt_attempted);
                self.position-= amt_actual as f64;
                if amt_actual < amt_attempted && self.velocity > 0.0 {
                    /* we are now bonked on the bottom... */
                    self.bonked_bottom = true;
                }
            }
            
            if self.position < (cfg.lookahead as f64) {
                let amt_attempted = ((cfg.lookahead as f64) - self.position) as usize;
                let amt_actual = window.scroll_up(amt_attempted);
                self.position+= amt_actual as f64;
                if amt_actual < amt_attempted && self.velocity < 0.0 && self.position < 1.0 {
                    /* we are now bonked on the top... */
                    self.bonked_top = true;
                }
            }
        }

        /* if we are bonked, apply spring */
        if self.bonked_top {
            self.velocity-= self.position * cfg.scroll_spring * ais;
            self.velocity-= self.velocity * cfg.scroll_spring_damping * ais;
        } else if self.bonked_bottom {
            self.velocity-= (self.position - cfg.lookahead as f64 * 2.0 - cfg.page_navigation_leadup as f64) * cfg.scroll_spring * ais;
            self.velocity-= self.velocity * cfg.scroll_spring_damping * ais;
        } else {
            /* apply constant deceleration */
            if self.velocity > 0.0 {
                if self.velocity > cfg.scroll_deceleration * 1.0 { /* never take more than 1 second to decelerate */
                    self.velocity = cfg.scroll_deceleration * 1.0;
                } else {
                    self.velocity-= cfg.scroll_deceleration * ais;
                    self.velocity = f64::max(0.0, self.velocity);
                }
            }
            if self.velocity < 0.0 {
                if self.velocity < -cfg.scroll_deceleration * 1.0 { /* never take more than 1 second to decelerate */
                    self.velocity = -cfg.scroll_deceleration * 1.0;
                } else {
                    self.velocity+= cfg.scroll_deceleration * ais;
                    self.velocity = f64::min(0.0, self.velocity);
                }
            }
            
            /* apply alignment spring */
            /* TODO
            if cfg.scroll_align_integer {
                let target = (self.position * lw.fonts.extents.height).round() / lw.fonts.extents.height;
                let delta = self.position - target;
                if delta.abs() < cfg.scroll_align_position_tolerance && self.velocity.abs() < cfg.scroll_align_velocity_tolerance {
                    // if we're close and we're moving slow, just snap
                    self.position = target;
                    self.velocity = 0.0;
                } else if self.velocity.abs() < cfg.scroll_align_velocity_tolerance {
                    // otherwise apply a spring force
                    self.velocity+= (target - self.position) * cfg.scroll_align_integer_spring * ais;
                    //self.velocity-= (self.velocity) * cfg.scroll_align_integer_spring_damping * ais;
                }
            }
             */
        }

        /* TODO
        if self.cursor_spring {
            let direction = self.cursor_direction; // evaluation order... ugh
            self.ensure_cursor_is_in_view_internal(&mut i, lw, direction);
        }
         */

        /* integrate velocity - done last for 1 frame tighter feedback loop */
        self.position+= self.velocity * ais;
        
        if f64::abs(self.velocity) > 0.01 {
            self.events.want();
        } else {
            self.velocity = 0.0;
        }
    }

    /* TODO
    pub fn ensure_cursor_is_in_view(&self, lw: &widget::listing::ListingWidget, dir: component::cursor::EnsureInViewDirection) {
        self.ensure_cursor_is_in_view_internal(&mut self.self.borrow_mut(), lw, dir);
    }

    fn ensure_cursor_is_in_view_internal(&self, i: &mut Interior, lw: &widget::listing::ListingWidget, dir: component::cursor::EnsureInViewDirection) {
        let cfg = config::get();
        
        /*
         * super threshold: move the window no matter what direction the cursor is moving
         * normal threshold: move the window if the cursor is moving out of the window
         */
        let top_super_threshold = f64::max(0.0, self.position) as isize;
        let top_threshold = top_super_threshold + cfg.page_navigation_leadup as isize;
        let bottom_super_threshold = lw.engine.get_window_height() as isize + f64::max(0.0, self.position) as isize - 2 * cfg.lookahead as isize;
        let bottom_threshold = bottom_super_threshold - cfg.page_navigation_leadup as isize;
        let cln = lw.engine.find_lineno(lw.cursor.get_addr());
        
        self.cursor_direction = dir;
        
        match cln {
            listing::LineFindResult::Before => {
                self.seek_internal(i, lw, lw.cursor.get_addr());
            },
            listing::LineFindResult::Found(ln) if ln < top_threshold && self.position > 1.0 && (dir.maybe_upwards() || ln < top_super_threshold) => {
                self.bonked_top = false;
                self.bonked_bottom = false;
                self.cursor_spring = true;
                self.velocity = -((top_threshold - ln) as f64) * 20.0;

                self.events.want_animate();
            },
            listing::LineFindResult::Found(ln) if ln > bottom_threshold && (dir.maybe_downwards() || ln > bottom_super_threshold) => {
                self.bonked_top = false;
                self.bonked_bottom = false;
                self.cursor_spring = true;
                self.velocity = (ln - bottom_threshold) as f64 * 20.0;

                self.events.want_animate();
            },
            listing::LineFindResult::After => {
                self.seek_internal(i, lw, lw.cursor.get_addr());
            },
            _ => {
                self.cursor_spring = false;
            }
        }
    }
*/

    pub fn seek(&mut self, window: &mut listing::window::ListingWindow, addr: addr::Address) {
        let cfg = config::get();
        
        window.seek(addr);
        if window.get_bottom_hit_end() {
            self.position = cfg.lookahead as f64 * 2.0 + cfg.page_navigation_leadup as f64;
            self.bonked_bottom = true;
        }  else {
            self.position = 0.0; /* let lookahead logic in tick handler work this out */
            self.bonked_bottom = false;
        }
        self.velocity = 0.0;
        self.bonked_top = false;
        //self.cursor_spring = false; TODO
        window.scroll_up(cfg.page_navigation_leadup);
    }
    
    pub fn page_up(&mut self, window: &listing::window::ListingWindow) {
        let cfg = config::get();
        
        /* kinematics 101 */
        self.bonked_bottom = false;
        self.velocity = -f64::sqrt(self.velocity.powi(2) + 2.0 * cfg.scroll_deceleration * (window.get_window_height() - 2 * cfg.lookahead - 2 * cfg.page_navigation_leadup) as f64);
    }

    pub fn page_down(&mut self, window: &listing::window::ListingWindow) {
        let cfg = config::get();
        
        /* kinematics 101 */
        self.bonked_top = false;
        self.velocity = f64::sqrt(self.velocity.powi(2) + 2.0 * cfg.scroll_deceleration * (window.get_window_height() - 2 * cfg.lookahead - 2 * cfg.page_navigation_leadup) as f64);
    }
}
