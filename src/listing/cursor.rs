use std::sync;
use std::task;
use std::vec;

use crate::addr;
use crate::space;
use crate::listing;
use crate::listing::BreakMapExt;
use crate::listing::window;
use crate::listing::line_group;

use enum_dispatch::enum_dispatch;

pub mod hex;

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
    InvalidForPosition,
    InvalidForType,
    KeyNotRecognized,
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

    fn enter_standard(&mut self, listing: &listing::ListingWatch, key: &gdk::EventKey) -> Result<MovementResult, EntryError>;
    fn enter_utf8    (&mut self, listing: &listing::ListingWatch, key: &gdk::EventKey) -> Result<MovementResult, EntryError>;
}

#[enum_dispatch(CursorClassExt)]
#[derive(Debug)]
pub enum CursorClass {
    Hex(hex::HexCursor)
}

#[derive(Debug)]
pub struct Cursor {
    window: window::MicroWindow,
    pub class: CursorClass,
}

impl Cursor {
    pub fn place(breaks: &listing::BreakMap, space: &sync::Arc<dyn space::AddressSpace>, hint: PlacementHint) -> Result<Cursor, PlacementFailure> {
        let mut window = window::MicroWindow::new(breaks.clone(), space.clone(), hint.addr);
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

    pub fn update(&mut self, listing: &listing::Listing, cx: &mut task::Context) -> bool {
        let mut updated = false;
        
        updated = if self.window.is_outdated(listing) {
            Self::place(listing.get_breaks(), listing.get_space(), PlacementHint {
                addr: self.class.get_addr(),
                intended_offset: self.class.get_intended_offset(),
                class: self.class.get_placement_hint()
            }).map(|new| { *self = new; }).is_ok() // TODO: better failure condition
        } else { updated };

        updated = self.class.get_line_group_mut().update(listing, cx) || updated;

        updated
    }

    pub fn goto(&mut self, addr: addr::Address) -> Result<(), PlacementFailure> {
        Self::place(self.window.get_breaks(), self.window.get_space(), PlacementHint {
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

    pub fn enter_standard(&mut self, listing: &listing::ListingWatch, key: &gdk::EventKey) -> Result<MovementResult, EntryError> {
        self.class.enter_standard(listing, key).map(|mr| self.movement(|_| mr, TransitionOp::EntryStandard))
    }

    pub fn enter_utf8(&mut self, listing: &listing::ListingWatch, key: &gdk::EventKey) -> Result<MovementResult, EntryError> {
        self.class.enter_utf8(listing, key).map(|mr| self.movement(|_| mr, TransitionOp::EntryUTF8))
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
            line_group::LineGroup::BreakHeader(_) => Err(lg), //TODO: implement break header cursor
        }
    }

    /// Attempts to transition a cursor onto the line.
    fn new_transition(lg: line_group::LineGroup, hint: &TransitionHint) -> Result<CursorClass, line_group::LineGroup> {
        match lg {
            line_group::LineGroup::Hex(_) => hex::HexCursor::new_transition(lg, &hint).map(|cc| CursorClass::Hex(cc)),
            line_group::LineGroup::BreakHeader(_) => Err(lg), //TODO: implement break header cursor
        }
    }

    /// true on success
    fn prev(&mut self, window: &mut window::MicroWindow, op: TransitionOp) -> bool {
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
    fn next(&mut self, window: &mut window::MicroWindow, op: TransitionOp) -> bool {
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
}

/// Used to hint at how to transition a cursor from one break to another.
#[derive(Debug)]
pub struct TransitionHint {
    pub intended_offset: Option<addr::Size>,
    pub op: TransitionOp,
    pub class: TransitionHintClass,
}

#[derive(Debug)]
pub enum TransitionHintClass {
    Hex(hex::HexTransitionHint),
    Unused
}
