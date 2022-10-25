use crate::model::addr;
use crate::model::document;
use crate::model::listing::cursor;
use crate::view::config;
use crate::view::listing::facet;

use std::sync;
use std::task;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Mode {
    Command,
    Entry,
    TextEntry,
}

pub struct CursorView {
    // TODO: rename to model
    pub cursor: cursor::Cursor,
    pub mode: Mode,
    pub has_focus: bool,
    pub insert: bool,
    
    config: sync::Arc<config::Config>,
    ev_draw: facet::Event,
    ev_work: facet::Event,
    
    blink_timer: f64,
    bonk_timer: f32,
}

impl std::fmt::Debug for CursorView {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CursorView")
            .field("cursor", &self.cursor)
            .field("mode", &self.mode)
            .field("has_focus", &self.has_focus)
            .field("insert", &self.insert)
            .field("ev_draw", &self.ev_draw)
            .field("ev_work", &self.ev_work)
            .field("blink_timer", &self.blink_timer)
            .field("bonk_timer", &self.bonk_timer)
            .finish_non_exhaustive()
    }
}

impl CursorView {
    pub fn new(document: sync::Arc<document::Document>) -> CursorView {
        CursorView {
            cursor: cursor::Cursor::new(document).expect("should be able to default-place cursor"),
            mode: Mode::Command,
            has_focus: true,
            insert: false,
            
            config: config::copy(),
            ev_draw: facet::Event::new(),
            ev_work: facet::Event::new(),
            
            blink_timer: 0.0,
            bonk_timer: 0.0,
        }
    }

    pub fn get_bonk(&self) -> f32 {
        (self.bonk_timer / 0.25) * 3.0 * ((0.25 - self.bonk_timer) * 10.0 * 2.0 * std::f32::consts::PI).cos()
    }

    pub fn get_blink(&self) -> bool {
        /*self.mode == Mode::Command || */self.blink_timer < self.config.cursor_blink_period / 2.0
    }

    pub fn animate(&mut self, delta: f64) {
        let old_blink = self.get_blink();
        self.blink_timer+= delta;
        self.blink_timer%= self.config.cursor_blink_period;
        
        if old_blink != self.get_blink() {
            self.ev_draw.want();
        }

        if self.bonk_timer > 0.0 {
            self.bonk_timer-= f32::min(delta as f32, self.bonk_timer);
            self.ev_draw.want();
        } else {
            self.bonk_timer = 0.0;
        }
    }
    
    pub fn blink(&mut self) {
        if !self.get_blink() {
            self.ev_draw.want();
        }
        self.blink_timer = 0.0;
    }

    pub fn bonk(&mut self) {
        self.blink();
        self.ev_draw.want();
        self.bonk_timer = 0.25;
    }

    pub fn change_mode(&mut self, mode: Mode) {
        self.blink();
        self.mode = mode;
        self.ev_draw.want();
    }

    pub fn change_insert(&mut self, insert: bool) {
        self.blink();
        self.insert = insert;
        self.ev_draw.want();
    }
    
    // TODO: macro this
    fn movement<F>(&mut self, mov: F) where F: FnOnce(&mut cursor::Cursor) -> cursor::MovementResult {
        self.ev_draw.want();
        self.blink();

        let result = mov(&mut self.cursor);
        
        match result {
            cursor::MovementResult::Ok => (),
            _ => self.bonk(),
        }
    }
    
    pub fn move_left(&mut self) { self.movement(|c| c.move_left()); }
    pub fn move_right(&mut self) { self.movement(|c| c.move_right()); }
    //pub fn move_up(&mut self) { self.movement(|c| c.move_up()); }
    //pub fn move_down(&mut self) { self.movement(|c| c.move_down()); }
    //pub fn move_to_start_of_line(&mut self) { self.movement(|c| c.move_to_start_of_line()); }
    //pub fn move_to_end_of_line(&mut self) { self.movement(|c| c.move_to_end_of_line()); }
    pub fn move_left_large(&mut self) { self.movement(|c| c.move_left_large()); }
    pub fn move_right_large(&mut self) { self.movement(|c| c.move_right_large()); }
    //pub fn move_up_to_break(&mut self) { self.movement(|c| c.move_up_to_break()); }
    //pub fn move_down_to_break(&mut self) { self.movement(|c| c.move_down_to_break()); }

    pub fn goto(&mut self, addr: addr::Address) -> Result<(), cursor::PlacementFailure>{
        self.blink();
        self.cursor.goto(addr)
    }

    /*
    pub fn entry(&mut self, document_host: &document::DocumentHost, key: &gdk::EventKey) -> bool {
        match match self.mode {
            Mode::Command => return false,
            Mode::Entry => {
                self.events.want_draw();
                self.cursor.enter_standard(document_host, self.insert, &cursor::key::Key::from(key))
            },
            Mode::TextEntry => {
                self.events.want_draw();
                self.cursor.enter_utf8(document_host, self.insert, &cursor::key::Key::from(key))
            },
        } {
            Ok(cursor::MovementResult::Ok) => { self.blink(); true },
            Err(cursor::EntryError::KeyNotRecognized) => { self.blink(); false },
            _ => { self.bonk(); true }
        }
}
    */
}

impl facet::Facet for CursorView {
    fn wants_draw(&mut self) -> &mut facet::Event {
        &mut self.ev_draw
    }

    fn wants_work(&mut self) -> &mut facet::Event {
        &mut self.ev_work
    }

    fn work(&mut self, _cx: &mut task::Context) {
    }
}
