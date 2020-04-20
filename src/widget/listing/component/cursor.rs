use std::sync;

use crate::addr;
use crate::config;
use crate::listing;
use crate::listing::cursor;
use crate::widget::listing::component;

pub struct CursorView {
    pub events: component::Events,
    pub cursor: cursor::Cursor,
    pub has_focus: bool,
    blink_timer: f64,
    bonk_timer: f64,
}

impl CursorView {
    pub fn new(listing: &sync::Arc<listing::Listing>) -> CursorView {
        CursorView {
            events: component::Events::new(),
            cursor: cursor::Cursor::place(listing, cursor::PlacementHint::default()).expect("should be able to default-place cursor"),
            blink_timer: 0.0,
            bonk_timer: 0.0,
            has_focus: true,
        }
    }

    pub fn get_bonk(&self) -> f64 {
        (self.bonk_timer / 0.25) * 3.0 * ((0.25 - self.bonk_timer) * 10.0 * 2.0 * std::f64::consts::PI).cos()
    }

    pub fn get_blink(&self) -> bool {
        self.blink_timer < config::get().cursor_blink_period / 2.0
    }

    pub fn animate(&mut self, cfg: &config::Config, ais: f64) {
        let old_blink = self.get_blink();
        self.blink_timer+= ais;
        self.blink_timer%= cfg.cursor_blink_period;

        if old_blink != self.get_blink() {
            self.events.want_draw();
        }

        if self.bonk_timer > 0.0 {
            self.bonk_timer-= ais;
            self.bonk_timer = f64::max(0.0, self.bonk_timer);
            self.events.want_draw();
        } else {
            self.bonk_timer = 0.0;
        }
    }
    
    pub fn blink(&mut self) {
        if !self.get_blink() {
            self.events.want_draw();
        }
        self.events.want_animate();
        self.blink_timer = 0.0;
    }

    pub fn bonk(&mut self) {
        self.events.want_draw();
        self.events.want_animate();
        self.bonk_timer = 0.25;
    }

    // TODO: macro this
    fn movement<F>(&mut self, mov: F) where F: FnOnce(&mut cursor::Cursor) -> cursor::MovementResult {
        self.events.want_draw();
        self.blink();

        match mov(&mut self.cursor) {
            cursor::MovementResult::Ok => (),
            _ => self.bonk(),
        }
    }
    
    pub fn move_left(&mut self) { self.movement(|c| c.move_left()); }
    pub fn move_right(&mut self) { self.movement(|c| c.move_right()); }
    pub fn move_up(&mut self) { self.movement(|c| c.move_up()); }
    pub fn move_down(&mut self) { self.movement(|c| c.move_down()); }
    pub fn move_to_start_of_line(&mut self) { self.movement(|c| c.move_to_start_of_line()); }
    pub fn move_to_end_of_line(&mut self) { self.movement(|c| c.move_to_end_of_line()); }
    pub fn move_left_large(&mut self) { self.movement(|c| c.move_left_large()); }
    pub fn move_right_large(&mut self) { self.movement(|c| c.move_right_large()); }
    pub fn move_up_to_break(&mut self) { self.movement(|c| c.move_up_to_break()); }
    pub fn move_down_to_break(&mut self) { self.movement(|c| c.move_down_to_break()); }

    pub fn goto(&mut self, addr: addr::Address) -> Result<(), cursor::PlacementFailure>{
        self.blink();
        self.cursor.goto(addr)
    }
}
