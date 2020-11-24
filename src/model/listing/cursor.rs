use std::task;
use std::vec;

use crate::model::addr;
use crate::model::document;
use crate::model::document::BreakMapExt;
use crate::model::listing::window;
use crate::model::listing::line_group;

use enum_dispatch::enum_dispatch;

pub mod hex;
pub mod break_header;

#[derive(Debug)]
pub enum MovementResult {
    Ok,
    HitStart,
    HitEnd,
    NotApplicableForPosition, /// e.g. move_to_start_of_line when we're already there
    NotApplicableForType,
    PlacementFailed(PlacementFailure),
}

#[derive(Debug)]
pub enum PlacementFailure {
    HitBottomOfAddressSpace,
}

pub enum EntryError {
    DataNotLoaded,
    KeyNotRecognized,
    /// key was recognized, but invalid for entry in this location
    InvalidForPosition,
    /// key was recognized, but invalid for entry for this type of cursor
    InvalidForType,
}

#[enum_dispatch]
pub trait CursorClassExt {
    fn get_line_group(&self) -> &line_group::LineGroup;
    fn get_line_group_mut(&mut self) -> &mut line_group::LineGroup;
    fn take_line_group(self) -> line_group::LineGroup;
    fn get_addr(&self) -> addr::Address;
    fn get_intended_offset(&self) -> Option<addr::Size>; /// only return null here if there is nothing meaningful to return
    fn get_placement_hint(&self) -> PlacementHintClass;
    fn get_transition_hint(&self) -> TransitionHintClass;

    fn move_left(&mut self) -> MovementResult;
    fn move_right(&mut self) -> MovementResult;
    fn move_up(&mut self) -> MovementResult;
    fn move_down(&mut self) -> MovementResult;
    fn move_to_start_of_line(&mut self) -> MovementResult;
    fn move_to_end_of_line(&mut self) -> MovementResult;
    fn move_left_large(&mut self) -> MovementResult;
    fn move_right_large(&mut self) -> MovementResult;

    fn enter_standard(&mut self, document_host: &document::DocumentHost, key: &gdk::EventKey) -> Result<MovementResult, EntryError>;
    fn enter_utf8    (&mut self, document_host: &document::DocumentHost, key: &gdk::EventKey) -> Result<MovementResult, EntryError>;
}

#[enum_dispatch(CursorClassExt)]
#[derive(Debug)]
pub enum CursorClass {
    Hex(hex::HexCursor),
    BreakHeader(break_header::BreakHeaderCursor),
}

#[derive(Debug)]
pub struct Cursor {
    window: window::FlexWindow,
    pub class: CursorClass,
}

impl Cursor {
    pub fn place(document: &document::Document, hint: PlacementHint) -> Result<Cursor, PlacementFailure> {
        let mut window = window::FlexWindow::new(document, hint.addr);
        window.seek(hint.addr);
        
        Ok(Cursor {
            class: loop {
                match window.produce_down() {
                    Some(nlg) => match CursorClass::new_placement(nlg, &hint) {
                        Ok(cursor) => break cursor,
                        Err(rlg) => window.trim_down(&rlg)
                    },
                    None => return Err(PlacementFailure::HitBottomOfAddressSpace)
                }
            },
            window,
        })
    }

    pub fn update(&mut self, document: &document::Document, cx: &mut task::Context) -> bool {
        let mut updated = false;
        
        updated = if self.window.is_outdated(document) {
            Self::place(document, PlacementHint {
                addr: self.class.get_addr(),
                intended_offset: self.class.get_intended_offset(),
                class: self.class.get_placement_hint()
            }).map(|new| { *self = new; }).is_ok() // TODO: better failure condition
        } else { updated };

        updated = self.class.get_line_group_mut().update(document, cx) || updated;

        updated
    }

    pub fn goto(&mut self, addr: addr::Address) -> Result<(), PlacementFailure> {
        Self::place(self.window.get_document(), PlacementHint {
            addr: addr,
            intended_offset: None,
            class: PlacementHintClass::Unused
        }).map(|new| { *self = new; })
    }
    
    pub fn get_line_group(&self) -> &line_group::LineGroup {
        self.class.get_line_group()
    }

    pub fn get_addr(&self) -> addr::Address {
        self.class.get_addr()
    }

    fn movement<F>(&mut self, mov: F, op: TransitionOp) -> MovementResult where F: FnOnce(&mut CursorClass) -> MovementResult {
        match mov(&mut self.class) {
            MovementResult::HitStart => if self.class.prev(&mut self.window, op) { MovementResult::Ok } else { MovementResult::HitStart }
            MovementResult::HitEnd   => if self.class.next(&mut self.window, op) { MovementResult::Ok } else { MovementResult::HitEnd }
            x => x,
        }
    }
    
    pub fn move_left(&mut self)             -> MovementResult { self.movement(|c| c.move_left(),             TransitionOp::UnspecifiedLeft) }
    pub fn move_right(&mut self)            -> MovementResult { self.movement(|c| c.move_right(),            TransitionOp::UnspecifiedRight) }
    pub fn move_up(&mut self)               -> MovementResult { self.movement(|c| c.move_up(),               TransitionOp::UnspecifiedLeft) }
    pub fn move_down(&mut self)             -> MovementResult { self.movement(|c| c.move_down(),             TransitionOp::UnspecifiedRight) }
    pub fn move_to_start_of_line(&mut self) -> MovementResult { self.movement(|c| c.move_to_start_of_line(), TransitionOp::UnspecifiedLeft) }
    pub fn move_to_end_of_line(&mut self)   -> MovementResult { self.movement(|c| c.move_to_end_of_line(),   TransitionOp::UnspecifiedRight) }
    pub fn move_left_large(&mut self)       -> MovementResult { self.movement(|c| c.move_left_large(),       TransitionOp::MoveLeftLarge) }
    pub fn move_right_large(&mut self)      -> MovementResult { self.movement(|c| c.move_right_large(),      TransitionOp::UnspecifiedRight) }

    pub fn move_up_to_break(&mut self) -> MovementResult {
        match self.goto(match self.window.get_breaks().break_before_addr(self.class.get_addr()) {
            Some(brk) => brk.addr,
            None => return MovementResult::HitStart,
        }) {
            Ok(_) => MovementResult::Ok,
            Err(e) => MovementResult::PlacementFailed(e),
        }
    }
    
    pub fn move_down_to_break(&mut self) -> MovementResult {
        match self.goto(match self.window.get_breaks().break_after_addr(self.class.get_addr()) {
            Some(brk) => brk.addr,
            None => return MovementResult::HitEnd,
        }) {
            Ok(_) => MovementResult::Ok,
            Err(e) => MovementResult::PlacementFailed(e),
        }
    }

    pub fn enter_standard(&mut self, document_host: &document::DocumentHost, key: &gdk::EventKey) -> Result<MovementResult, EntryError> {
        self.class.enter_standard(document_host, key).map(|mr| self.movement(|_| mr, TransitionOp::EntryStandard))
    }

    pub fn enter_utf8(&mut self, document_host: &document::DocumentHost, key: &gdk::EventKey) -> Result<MovementResult, EntryError> {
        self.class.enter_utf8(document_host, key).map(|mr| self.movement(|_| mr, TransitionOp::EntryUTF8))
    }
}

impl CursorClass {
    /// Attempts to place a cursor on the line. Is allowed to fail if the line
    /// does not accept cursors, or if the hint indicates that the cursor should
    /// not be placed on this line. For example, BreakHeaderCursor will only
    /// place a cursor if the cursor is hinted to be on a break header,
    /// otherwise it will prefer to place the cursor on a content line.
    fn new_placement(lg: line_group::LineGroup, hint: &PlacementHint) -> Result<CursorClass, line_group::LineGroup> {
        match lg {
            line_group::LineGroup::Hex(_) => hex::HexCursor::new_placement(lg, &hint).map(|cc| CursorClass::Hex(cc)),
            line_group::LineGroup::BreakHeader(_) => break_header::BreakHeaderCursor::new_placement(lg, &hint).map(|cc| CursorClass::BreakHeader(cc)),
        }
    }

    /// Attempts to transition a cursor onto the line.
    fn new_transition(lg: line_group::LineGroup, hint: &TransitionHint) -> Result<CursorClass, line_group::LineGroup> {
        match lg {
            line_group::LineGroup::Hex(_) => hex::HexCursor::new_transition(lg, &hint).map(|cc| CursorClass::Hex(cc)),
            line_group::LineGroup::BreakHeader(_) => break_header::BreakHeaderCursor::new_transition(lg, &hint).map(|cc| CursorClass::BreakHeader(cc)),
        }
    }

    /// true on success
    fn prev(&mut self, window: &mut window::FlexWindow, op: TransitionOp) -> bool {
        let hint = TransitionHint {
            intended_offset: self.get_intended_offset(),
            op,
            class: self.get_transition_hint(),
        };

        let mut skip_lines = vec::Vec::new();
        
        loop {
            match window.produce_up() {
                None => { /* well, we reached the top of the listing. trim down all the lines we produced and return ourselves back. */
                    for l in skip_lines.iter().rev() {
                        window.trim_down(l);
                    }
                    return false
                },
                Some(lg) => match Self::new_transition(lg, &hint) {
                    Ok(cc) => {
                        /* we were successful, so trim up the original line and any intermediate lines we skipped over */
                        window.trim_up(self.get_line_group());
                        for l in skip_lines.iter() {
                            window.trim_up(l);
                        }
                        *self = cc;
                        return true
                    },
                    Err(lg) => {
                        /* skip this line and try the next */
                        skip_lines.push(lg);
                    }
                },
            }
        }
    }

    /// true on success
    fn next(&mut self, window: &mut window::FlexWindow, op: TransitionOp) -> bool {
        let hint = TransitionHint {
            intended_offset: self.get_intended_offset(),
            op,
            class: self.get_transition_hint(),
        };

        let mut skip_lines = vec::Vec::new();
        
        loop {
            match window.produce_down() {
                None => { /* well, we reached the bottom of the listing. trim up all the lines we produced and return ourselves back. */
                    for l in skip_lines.iter().rev() {
                        window.trim_up(l);
                    }
                    return false
                },
                Some(lg) => match Self::new_transition(lg, &hint) {
                    Ok(cc) => {
                        /* we were successful, so trim down the original line and any intermediate lines we skipped over */
                        window.trim_down(self.get_line_group());
                        for l in skip_lines.iter() {
                            window.trim_down(l);
                        }
                        *self = cc;
                        return true
                    },
                    Err(lg) => {
                        /* skip this line and try the next */
                        skip_lines.push(lg);
                    }
                },
            }
        }
    }
}

/// Stash away enough information to be able to create a new cursor in the same
/// place if the break list changed.
#[derive(Default, Debug)]
pub struct PlacementHint {
    pub addr: addr::Address, /// Closest address to cursor position
    pub intended_offset: Option<addr::Size>,
    pub class: PlacementHintClass,
}

#[derive(Debug)]
pub enum PlacementHintClass {
    Hex(hex::HexPlacementHint),
    BreakHeader,
    Unused
}

impl std::default::Default for PlacementHintClass {
    fn default() -> PlacementHintClass {
        PlacementHintClass::Unused
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TransitionOp {
    MoveLeftLarge,
    EntryStandard,
    EntryUTF8,
    UnspecifiedLeft,
    UnspecifiedRight,
}

impl TransitionOp {
    pub fn is_left(&self) -> bool {
        match self {
            TransitionOp::MoveLeftLarge => true,
            TransitionOp::EntryStandard => false,
            TransitionOp::EntryUTF8 => false,
            TransitionOp::UnspecifiedLeft => true,
            TransitionOp::UnspecifiedRight => false,
        }
    }

    pub fn is_right(&self) -> bool {
        match self {
            TransitionOp::MoveLeftLarge => false,
            TransitionOp::EntryStandard => true,
            TransitionOp::EntryUTF8 => true,
            TransitionOp::UnspecifiedLeft => false,
            TransitionOp::UnspecifiedRight => true,
        }
    }

    pub fn is_entry(&self) -> bool {
        match self {
            TransitionOp::MoveLeftLarge => false,
            TransitionOp::EntryStandard => true,
            TransitionOp::EntryUTF8 => true,
            TransitionOp::UnspecifiedLeft => false,
            TransitionOp::UnspecifiedRight => false,
        }
    }
}

/// Used to hint at how to transition a cursor from one break to another.
#[derive(Debug, Clone)]
pub struct TransitionHint {
    pub intended_offset: Option<addr::Size>,
    pub op: TransitionOp,
    pub class: TransitionHintClass,
}

#[derive(Debug, Clone)]
pub enum TransitionHintClass {
    Hex(hex::HexTransitionHint),
    Unused
}
