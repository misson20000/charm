use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::cursor;
use crate::model::listing::cursor::CursorClassExt;
use crate::model::listing::token;
use crate::model::listing::token::CloneToken;
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
    pub hidden: bool,
    
    config: sync::Arc<config::Config>,
    ev_draw: facet::Event,
    ev_work: facet::Event,

    blink_timer: f64,
    bonk_timer: f32,

    token_watch_sender: tokio::sync::watch::Sender<(sync::Arc<document::Document>, token::Token)>,
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
    pub fn new(document: sync::Arc<document::Document>, config: sync::Arc<config::Config>) -> CursorView {
        let cursor = cursor::Cursor::new(document);
        let token_watch_sender = tokio::sync::watch::Sender::new((cursor.document(), cursor.class.get_token().clone_token()));
        
        CursorView {
            cursor,
            
            mode: Mode::Command,
            has_focus: true,
            insert: false,
            hidden: false,
            
            config,
            ev_draw: facet::Event::new(),
            ev_work: facet::Event::new_wanted(),
            
            blink_timer: 0.0,
            bonk_timer: 0.0,

            token_watch_sender,
        }
    }

    pub fn reconf(&mut self, config: sync::Arc<config::Config>) {
        self.config = config;
    }
    
    pub fn get_bonk(&self) -> f32 {
        (self.bonk_timer / 0.25) * 3.0 * ((0.25 - self.bonk_timer) * 10.0 * 2.0 * std::f32::consts::PI).cos()
    }

    pub fn get_blink(&self) -> bool {
        !self.hidden && (self.config.cursor_blink_period == 0.0 || self.blink_timer < self.config.cursor_blink_period / 2.0)
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

    pub fn change_focused(&mut self, focused: bool) {
        self.blink();
        self.has_focus = focused;
        self.ev_draw.want();
    }

    pub fn document_updated(&mut self, new_doc: &sync::Arc<document::Document>) {
        if self.cursor.update(new_doc) {
            self.notify_moved();
        }
    }
    
    // TODO: macro this
    fn movement<F>(&mut self, mov: F) where F: FnOnce(&mut cursor::Cursor) -> cursor::MovementResult {
        self.ev_draw.want();
        self.ev_work.want();
        self.blink();

        match mov(&mut self.cursor) {
            cursor::MovementResult::Ok => (),
            _ => self.bonk(),
        }

        self.notify_moved();
    }
    
    pub fn move_left(&mut self) { self.movement(|c| c.move_left()); }
    pub fn move_right(&mut self) { self.movement(|c| c.move_right()); }
    pub fn move_up(&mut self) { self.movement(|c| c.move_up()); }
    pub fn move_down(&mut self) { self.movement(|c| c.move_down()); }
    //pub fn move_to_start_of_line(&mut self) { self.movement(|c| c.move_to_start_of_line()); }
    //pub fn move_to_end_of_line(&mut self) { self.movement(|c| c.move_to_end_of_line()); }
    pub fn move_left_large(&mut self) { self.movement(|c| c.move_left_large()); }
    pub fn move_right_large(&mut self) { self.movement(|c| c.move_right_large()); }
    //pub fn move_up_to_break(&mut self) { self.movement(|c| c.move_up_to_break()); }
    //pub fn move_down_to_break(&mut self) { self.movement(|c| c.move_down_to_break()); }

    pub fn goto(&mut self, document: sync::Arc<document::Document>, path: &structure::Path, offset: addr::Offset, hint: cursor::PlacementHint) {
        self.blink();
        self.ev_draw.want();
        self.ev_work.want();
        self.cursor.goto(document, path, offset, hint);

        self.notify_moved();
    }

    pub fn set(&mut self, new: cursor::Cursor) {
        self.blink();
        self.ev_draw.want();
        self.ev_work.want();
        self.cursor = new;

        self.notify_moved();
    }
    
    pub fn enter_hex(&mut self, document_host: &document::DocumentHost, nybble: u8) -> Result<(), cursor::EntryError> {
        self.ev_draw.want();
        self.ev_work.want();
        self.blink();

        match self.cursor.enter_hex(document_host, nybble) {
            Ok(cursor::MovementResult::Ok) => (),
            Ok(_) => self.bonk(),
            Err(e) => return Err(e)
        }

        self.notify_moved();

        Ok(())
    }
    
    pub fn endpoint_for_rubber_band(&self) -> (structure::Path, usize, addr::Offset) {
        let mut path = self.cursor.structure_path();
        let mut child = self.cursor.structure_child_index();
        let mut offset = self.cursor.structure_offset();

        /* silly tweaks that make it all feel better to use */
        match &self.cursor.class {
            cursor::CursorClass::Hexdump(hxc) if hxc.low_nybble => {
                offset+= addr::Offset::BYTE;
            },
            cursor::CursorClass::Hexstring(hxc) if hxc.low_nybble => {
                offset+= addr::Offset::BYTE;
            },
            cursor::CursorClass::SummaryLabel(_slc) => {
                path.push(child);
                child = 0;
                offset = addr::Offset::NULL;
            },
            _ => {},
        };

        (path, child, offset)
    }

    fn notify_moved(&self) {
        self.token_watch_sender.send_replace((self.cursor.document(), self.get_token().clone_token()));
    }
    
    pub fn get_token(&self) -> token::TokenRef<'_> {
        self.cursor.class.get_token()
    }

    pub fn get_token_watch(&self) -> tokio::sync::watch::Receiver<(sync::Arc<document::Document>, token::Token)> {
        self.token_watch_sender.subscribe()
    }
}

impl facet::Facet for CursorView {
    fn wants_draw(&self) -> &facet::Event {
        &self.ev_draw
    }

    fn wants_work(&self) -> &facet::Event {
        &self.ev_work
    }

    fn work(&mut self, document: &sync::Arc<document::Document>, cx: &mut task::Context) -> bool {
        let (did_work, done) = self.cursor.class.work(document, cx);
        
        if did_work {
            self.wants_draw();
        }

        !done
    }
}
