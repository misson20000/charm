use std::sync;

use crate::view::config;
use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::window;
use crate::model::listing::window::LineView;
use crate::view::listing;
use crate::view::listing::facet;
use crate::view::listing::helpers;
use crate::view::listing::line;

/* not used unless gtk4_8 is enabled */
#[allow(unused_imports)]
use gtk::gdk;

pub struct Scroller {
    config: sync::Arc<config::Config>,
    ev_draw: facet::Event,
    ev_work: facet::Event,
    ev_repick: facet::Event,

    pub position: f64,
    pub velocity: f64,
    bonked_top: bool,
    bonked_bottom: bool,
    cursor_spring: bool,
    cursor_direction: EnsureCursorInViewDirection,
}

pub enum ScrollUnit {
    Wheel,
    Surface,
    Unknown
}

#[cfg(feature = "gtk4_8")]
impl<'a> From<&'a gtk::EventControllerScroll> for ScrollUnit {
    fn from(ecs: &'a gtk::EventControllerScroll) -> Self {
        match ecs.unit() {
            gdk::ScrollUnit::Wheel => Self::Wheel,
            gdk::ScrollUnit::Surface => Self::Surface,
            _ => Self::Unknown,
        }
    }
}

#[cfg(not(feature = "gtk4_8"))]
impl<'a> From<&'a gtk::EventControllerScroll> for ScrollUnit {
    fn from(_ecs: &'a gtk::EventControllerScroll) -> Self {
        Self::Unknown
    }
}

impl ScrollUnit {
    pub fn divisor(&self, render: &listing::RenderDetail) -> f64 {
        match self {
            ScrollUnit::Wheel => 1.0,
            ScrollUnit::Surface => helpers::pango_unscale(render.metrics.height()) as f64,
            ScrollUnit::Unknown => 1.0,
        }
    }
}

type Window = window::Window<line::Line>;

impl Scroller {
    pub fn new(config: sync::Arc<config::Config>) -> Scroller {
        Scroller {
            config,
            ev_draw: facet::Event::new(),
            ev_work: facet::Event::new(),
            ev_repick: facet::Event::new(),
            
            position: 0.0,
            velocity: 0.0,
            bonked_top: false,
            bonked_bottom: false,
            cursor_spring: false,
            cursor_direction: EnsureCursorInViewDirection::Any,
        }
    }

    pub fn reconf(&mut self, config: sync::Arc<config::Config>) {
        self.config = config;
    }
    
    pub fn get_position(&self) -> f64 {
        self.position
    }

    pub fn get_lookahead(&self) -> usize {
        self.config.lookahead
    }
    
    pub fn scroll_wheel_impulse(&mut self, delta: f64, kinetic_allowed: bool) {
        if delta < 0.0 && self.bonked_top {
            return;
        }

        if delta > 0.0 && self.bonked_bottom {
            return;
        }
        
        if kinetic_allowed && self.config.scroll_enable_kinetic {
            self.velocity+= delta * self.config.scroll_wheel_impulse;
        } else {
            self.position+= delta * 3.0;
        }

        if delta > 0.0 {
            self.bonked_top = false;
        }

        if delta < 0.0 {
            self.bonked_bottom = false;
        }

        self.ev_draw.want();
    }

    pub fn scroll_decelerate(&mut self, velocity: f64) {
        if velocity < 0.0 && self.bonked_top {
            return;
        }

        if velocity > 0.0 && self.bonked_bottom {
            return;
        }
        
        self.velocity = velocity;

        if self.velocity > 0.0 {
            self.bonked_top = false;
        }

        if self.velocity < 0.0 {
            self.bonked_bottom = false;
        }

        self.ev_draw.want();
    }

    pub fn animate(&mut self, window: &mut Window, cursor_view: &facet::cursor::CursorView, delta: f64) {
        /* try to scroll listing engine, setting bonk flags if necessary */

        let bottom_pos = 2.0 * self.config.lookahead as f64 + self.config.page_navigation_leadup as f64;
        
        if self.position > (self.config.lookahead as f64) {
            let mut amt = self.position as usize - self.config.lookahead;
            
            while amt > 0 {
                if window.scroll_down() {
                    self.position-= 1.0;
                    amt-= 1;
                    
                    /* request work on behalf of the new lines that need to load their content. */
                    self.ev_work.want();
                } else {
                    /* we are now bonked on the bottom... */
                    if self.velocity > -0.005 && self.position > bottom_pos + 0.005 {
                        self.bonked_bottom = true;
                    }
                    break;
                }
            }
        }
        
        if self.position < (self.config.lookahead as f64) {
            let mut amt = ((self.config.lookahead as f64) - self.position) as usize;

            while amt > 0 {
                if window.scroll_up() {
                    self.position+= 1.0;
                    amt-= 1;

                    /* request work on behalf of the new lines that need to load their content. */
                    self.ev_work.want();
                } else {
                    /* we are now bonked on the top... */
                    if self.velocity < 0.005 && self.position < -0.005 {
                        self.bonked_top = true;
                    }
                    break;
                }
            }
        }

        /* if we are bonked, apply spring */
        if self.bonked_top {
            self.velocity-= self.position * self.config.scroll_spring * delta;
            self.velocity-= self.velocity * self.config.scroll_spring_damping * delta;

            if self.position < 0.01 && self.position > -0.01 && self.velocity < 0.5 && self.velocity > -0.5 {
                self.velocity = 0.0;
                self.position = 0.0;
                self.bonked_top = false;
            }
        } else if self.bonked_bottom {
            self.velocity-= (self.position - bottom_pos) * self.config.scroll_spring * delta;
            self.velocity-= self.velocity * self.config.scroll_spring_damping * delta;

            if self.velocity < 0.5 && self.velocity > -0.5 && self.position < bottom_pos+0.01 && self.position > bottom_pos-0.05 {
                self.velocity = 0.0;
                self.position = bottom_pos;
                self.bonked_bottom = false;
            }
        } else {
            /* apply constant deceleration */
            if self.velocity > 0.0 {
                if self.velocity > self.config.scroll_deceleration * 1.0 { /* never take more than 1 second to decelerate */
                    self.velocity = self.config.scroll_deceleration * 1.0;
                } else {
                    self.velocity-= self.config.scroll_deceleration * delta;
                    self.velocity = f64::max(0.0, self.velocity);
                }
            }
            if self.velocity < 0.0 {
                if self.velocity < -self.config.scroll_deceleration * 1.0 { /* never take more than 1 second to decelerate */
                    self.velocity = -self.config.scroll_deceleration * 1.0;
                } else {
                    self.velocity+= self.config.scroll_deceleration * delta;
                    self.velocity = f64::min(0.0, self.velocity);
                }
            }
            
            /* apply alignment spring */
            /* TODO
            if self.config.scroll_align_integer {
                let target = (self.position * lw.fonts.extents.height).round() / lw.fonts.extents.height;
                let delta = self.position - target;
                if delta.abs() < self.config.scroll_align_position_tolerance && self.velocity.abs() < self.config.scroll_align_velocity_tolerance {
                    // if we're close and we're moving slow, just snap
                    self.position = target;
                    self.velocity = 0.0;
                } else if self.velocity.abs() < self.config.scroll_align_velocity_tolerance {
                    // otherwise apply a spring force
                    self.velocity+= (target - self.position) * self.config.scroll_align_integer_spring * delta;
                    //self.velocity-= (self.velocity) * self.config.scroll_align_integer_spring_damping * delta;
                }
            }
             */
        }

        if self.cursor_spring {
            self.ensure_cursor_is_in_view(window, cursor_view, self.cursor_direction);
        }

        /* integrate velocity - done last for 1 frame tighter feedback loop */
        self.position+= self.velocity * delta;
        
        if f64::abs(self.velocity) > 0.01 {
            self.ev_repick.want();
            self.ev_draw.want();
        } else {
            self.velocity = 0.0;
        }
    }

    pub fn ensure_cursor_is_in_view(
        &mut self,
        window: &mut Window,
        cursor_view: &facet::cursor::CursorView,
        dir: EnsureCursorInViewDirection) {
        let cfg = &self.config;
        
        /*
         * super threshold: move the window no matter what direction the cursor is moving
         * normal threshold: move the window if the cursor is moving out of the window
         */
        let top_super_threshold = f64::max(0.0, self.position) as isize;
        let top_threshold = top_super_threshold + cfg.page_navigation_leadup as isize;
        let bottom_super_threshold = window.get_window_height() as isize + f64::max(0.0, self.position) as isize - 2 * cfg.lookahead as isize;
        let bottom_threshold = bottom_super_threshold - cfg.page_navigation_leadup as isize;
        
        let line_no = window.line_views.iter().enumerate().find(|(_, line)| line.iter_tokens().any(|token| cursor_view.cursor.is_over(token))).map(|(i, _)| i as isize);
        
        self.cursor_direction = dir;
        
        match line_no {
            Some(ln) if ln < top_threshold && self.position > 1.0 && (dir.maybe_upwards() || ln < top_super_threshold) => {
                self.bonked_top = false;
                self.bonked_bottom = false;
                self.cursor_spring = true;
                self.velocity = -((top_threshold - ln) as f64) * 20.0;

                self.ev_draw.want();
            },
            Some(ln) if ln > bottom_threshold && (dir.maybe_downwards() || ln > bottom_super_threshold) => {
                self.bonked_top = false;
                self.bonked_bottom = false;
                self.cursor_spring = true;
                self.velocity = (ln - bottom_threshold) as f64 * 20.0;

                self.ev_draw.want();
            },
            None => {
                self.seek(window, cursor_view.cursor.document(), &cursor_view.cursor.structure_path(), cursor_view.cursor.structure_offset());
            },
            _ => {
                self.cursor_spring = false;
            }
        }
    }

    pub fn seek(&mut self, window: &mut Window, document: sync::Arc<document::Document>, path: &structure::Path, offset: addr::Offset) {
        window.seek(document, path, offset);

        if window.get_bottom_hit_end() {
            self.position = self.config.lookahead as f64 * 2.0 + self.config.page_navigation_leadup as f64;
            self.bonked_bottom = true;
        }  else {
            self.position = 0.0; /* let lookahead logic in tick handler work this out */
            self.bonked_bottom = false;
        }
        self.velocity = 0.0;
        self.bonked_top = false;

        for _ in 0..self.config.page_navigation_leadup {
            window.scroll_up();
        }

        /* request work on behalf of the new lines that need to load their content. */
        self.ev_work.want();
    }
    
    pub fn page_up(&mut self, window: &Window) {
        /* kinematics 101 */
        self.bonked_bottom = false;
        self.velocity = -f64::sqrt(self.velocity.powi(2) + 2.0 * self.config.scroll_deceleration * (window.get_window_height() - 2 * self.config.lookahead - 2 * self.config.page_navigation_leadup) as f64);
    }

    pub fn page_down(&mut self, window: &Window) {
        /* kinematics 101 */
        self.bonked_top = false;
        self.velocity = f64::sqrt(self.velocity.powi(2) + 2.0 * self.config.scroll_deceleration * (window.get_window_height() - 2 * self.config.lookahead - 2 * self.config.page_navigation_leadup) as f64);
    }
}

#[derive(Debug, Copy, Clone)]
pub enum EnsureCursorInViewDirection {
    Up,
    Down,
    Any
}

impl EnsureCursorInViewDirection {
    pub fn maybe_upwards(&self) -> bool {
        match self {
            EnsureCursorInViewDirection::Up => true,
            EnsureCursorInViewDirection::Down => false,
            EnsureCursorInViewDirection::Any => true
        }
    }

    pub fn maybe_downwards(&self) -> bool {
        match self {
            EnsureCursorInViewDirection::Up => false,
            EnsureCursorInViewDirection::Down => true,
            EnsureCursorInViewDirection::Any => true
        }
    }
}

impl facet::Facet for Scroller {
    fn wants_draw(&self) -> &facet::Event {
        &self.ev_draw
    }

    fn wants_work(&self) -> &facet::Event {
        &self.ev_work
    }

    fn wants_repick(&self) -> &facet::Event {
        &self.ev_repick
    }
}
