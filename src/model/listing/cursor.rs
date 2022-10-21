use std::sync;
use std::task;

use crate::model::addr;
use crate::model::document;
use crate::model::listing::token;
use crate::logic::tokenizer;

use enum_dispatch::enum_dispatch;

pub mod key;

pub mod title;
pub mod hexdump;

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
    HitTopOfAddressSpace,
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
    fn update(&mut self, _document: &document::Document, _cx: &mut task::Context) -> bool {
        false
    }

    fn is_over(&self, token: &token::Token) -> bool;
    fn get_addr(&self) -> addr::Address;
    fn get_placement_hint(&self) -> PlacementHint;
    fn get_transition_hint(&self) -> TransitionHintClass;

    fn move_left(&mut self) -> MovementResult;
    fn move_right(&mut self) -> MovementResult;
    fn move_left_large(&mut self) -> MovementResult;
    fn move_right_large(&mut self) -> MovementResult;

    //fn enter_standard(&mut self, document_host: &document::DocumentHost, insert: bool, key: &key::Key) -> Result<MovementResult, EntryError>;
    //fn enter_utf8    (&mut self, document_host: &document::DocumentHost, insert: bool, key: &key::Key) -> Result<MovementResult, EntryError>;
}

#[enum_dispatch(CursorClassExt)]
#[derive(Debug)]
pub enum CursorClass {
    Title(title::Cursor),
    Hexdump(hexdump::Cursor),
}

#[derive(Debug)]
pub struct Cursor {
    tokenizer: tokenizer::Tokenizer,
    pub class: CursorClass,
    document: sync::Arc<document::Document>,
}

impl CursorClass {
    fn place_forward(tokenizer: &mut tokenizer::Tokenizer, addr: addr::Address, hint: &PlacementHint) -> Result<CursorClass, PlacementFailure> {
        loop {
            match tokenizer.next() {
                Some(token) => match CursorClass::new_placement(token, addr, hint) {
                    Ok(cursor) => return Ok(cursor),
                    /* failed to place on this token; try the next */
                    Err(_) => continue,
                },
                None => return Err(PlacementFailure::HitBottomOfAddressSpace)
            }
        }        
    }

    fn place_backward(tokenizer: &mut tokenizer::Tokenizer, addr: addr::Address, hint: &PlacementHint) -> Result<CursorClass, PlacementFailure> {
        loop {
            match tokenizer.prev() {
                Some(token) => match CursorClass::new_placement(token, addr, hint) {
                    Ok(cursor) => return Ok(cursor),
                    /* failed to place on this token; try the previous */
                    Err(_) => continue,
                },
                None => return Err(PlacementFailure::HitTopOfAddressSpace)
            }
        }        
    }
}

impl Cursor {
    pub fn new(document: sync::Arc<document::Document>) -> Result<Cursor, PlacementFailure> {
        let mut tokenizer = tokenizer::Tokenizer::at_beginning(document.root.clone());
        
        Ok(Cursor {
            class: CursorClass::place_forward(&mut tokenizer, addr::unit::NULL, &PlacementHint::default())?,
            tokenizer,
            document,
        })
    }

    pub fn place(document: sync::Arc<document::Document>, addr: addr::Address, hint: PlacementHint) -> Result<Cursor, PlacementFailure> {
        let mut tokenizer = tokenizer::Tokenizer::at_address(document.root.clone(), addr);
        
        Ok(Cursor {
            class: CursorClass::place_forward(&mut tokenizer, addr, &hint)?,
            tokenizer,
            document,
        })
    }

    pub fn update(&mut self, document: &sync::Arc<document::Document>, cx: &mut task::Context) -> bool {
        let mut updated = false;

        /* if we're using an outdated structure hierarchy root, make a
         * new tokenizer and try to put the cursor nearby in the new
         * hierarchy. */
        if self.document.is_outdated(document) {
            self.tokenizer.port_doc(&self.document, document);
            let mut tokenizer = self.tokenizer.clone();

            let class = match CursorClass::place_forward(&mut tokenizer, self.class.get_addr(), &self.class.get_placement_hint()) {
                Ok(cc) => cc,
                Err(PlacementFailure::HitBottomOfAddressSpace) => {
                    tokenizer = self.tokenizer.clone();
                    match CursorClass::place_backward(&mut tokenizer, self.class.get_addr(), &self.class.get_placement_hint()) {
                        Ok(cc) => cc,
                        Err(PlacementFailure::HitTopOfAddressSpace) => panic!("expected to be able to place cursor somewhere"),
                        Err(_) => panic!("unexpected error from CursorClass::place_backward")
                    }
                },
                Err(_) => panic!("unexpected error from CursorClass::place_forward")
            };

            *self = Cursor {
                tokenizer,
                class,
                document: document.clone()
            };

            updated = true;
        }

        updated = self.class.update(&document, cx) || updated;

        updated
    }

    pub fn goto(&mut self, addr: addr::Address) -> Result<(), PlacementFailure> {
        Self::place(self.document.clone(), addr, PlacementHint::Unused).map(|new| { *self = new; })
    }

    pub fn is_over(&self, token: &token::Token) -> bool {
        self.class.is_over(token)
    }
    
    fn movement<F>(&mut self, mov: F, op: TransitionOp) -> MovementResult where F: FnOnce(&mut CursorClass) -> MovementResult {
        match mov(&mut self.class) {
            /* If the movement hit a token boundary, try moving the cursor to another token. */
            MovementResult::HitStart => match self.class.try_move_prev_token(self.tokenizer.clone(), op) {
                Some((cc, tokenizer)) => { (self.class, self.tokenizer) = (cc, tokenizer); MovementResult::Ok }
                None => MovementResult::HitStart
            },
            MovementResult::HitEnd   => match self.class.try_move_next_token(self.tokenizer.clone(), op) {
                Some((cc, tokenizer)) => { (self.class, self.tokenizer) = (cc, tokenizer); MovementResult::Ok }
                None => MovementResult::HitStart
            }
            /* Otherwise, it's a legitimate result. */
            x => x,
        }
    }
    
    pub fn move_left(&mut self)             -> MovementResult { self.movement(|c| c.move_left(),             TransitionOp::UnspecifiedLeft) }
    pub fn move_right(&mut self)            -> MovementResult { self.movement(|c| c.move_right(),            TransitionOp::UnspecifiedRight) }
    //pub fn move_up(&mut self)               -> MovementResult { self.movement(|c| c.move_up(),               TransitionOp::UnspecifiedLeft) }
    //pub fn move_down(&mut self)             -> MovementResult { self.movement(|c| c.move_down(),             TransitionOp::UnspecifiedRight) }
    //pub fn move_to_start_of_line(&mut self) -> MovementResult { self.movement(|c| c.move_to_start_of_line(), TransitionOp::UnspecifiedLeft) }
    //pub fn move_to_end_of_line(&mut self)   -> MovementResult { self.movement(|c| c.move_to_end_of_line(),   TransitionOp::UnspecifiedRight) }
    pub fn move_left_large(&mut self)       -> MovementResult { self.movement(|c| c.move_left_large(),       TransitionOp::MoveLeftLarge) }
    pub fn move_right_large(&mut self)      -> MovementResult { self.movement(|c| c.move_right_large(),      TransitionOp::UnspecifiedRight) }

    /* previous node */
    pub fn move_prev(&mut self) -> MovementResult {
        todo!();
    }

    /* next node */
    pub fn move_next(&mut self) -> MovementResult {
        todo!();
    }

    /*
    pub fn enter_standard(&mut self, document_host: &document::DocumentHost, insert: bool, key: &key::Key) -> Result<MovementResult, EntryError> {
        self.class.enter_standard(document_host, insert, key).map(|mr| self.movement(|_| mr, TransitionOp::EntryStandard))
    }

    pub fn enter_utf8(&mut self, document_host: &document::DocumentHost, insert: bool, key: &key::Key) -> Result<MovementResult, EntryError> {
        self.class.enter_utf8(document_host, insert, key).map(|mr| self.movement(|_| mr, TransitionOp::EntryUTF8))
    }
    */
}

impl CursorClass {
    /// Attempts to place a cursor on the token. Is allowed to fail if the token
    /// does not accept cursors, or if the hint indicates that the cursor should
    /// not be placed on this token. For example, title::Cursor will only place
    /// a cursor if the cursor is hinted to be on a title, otherwise it will
    /// prefer to place the cursor on a content line. This way, goto will put
    /// the cursor on data, but the cursor will stay on a title if it was on
    /// one.
    fn new_placement(token: token::Token, addr: addr::Address, hint: &PlacementHint) -> Result<CursorClass, token::Token> {
        match token.class {
            token::TokenClass::Title => title::Cursor::new_placement(token, addr, &hint).map(|cc| CursorClass::Title(cc)),
            token::TokenClass::Hexdump(_) => hexdump::Cursor::new_placement(token, addr, &hint).map(|cc| CursorClass::Hexdump(cc)),
            _ => Err(token)
        }
    }

    /// Attempts to transition a cursor onto the token.
    fn new_transition(token: token::Token, hint: &TransitionHint) -> Result<CursorClass, token::Token> {
        match token.class {
            token::TokenClass::Title => title::Cursor::new_transition(token, &hint).map(|cc| CursorClass::Title(cc)),
            token::TokenClass::Hexdump(_) => hexdump::Cursor::new_transition(token, &hint).map(|cc| CursorClass::Hexdump(cc)),
            _ => Err(token)
        }
    }
    
    fn try_move_prev_token(&self, mut tokenizer: tokenizer::Tokenizer, op: TransitionOp) -> Option<(CursorClass, tokenizer::Tokenizer)> {
        let hint = TransitionHint {
            op,
            class: self.get_transition_hint(),
        };
        
        loop {
            match tokenizer.prev() {
                None => {
                    return None
                },
                Some(token) => match Self::new_transition(token, &hint) {
                    Ok(cc) => {
                        return Some((cc, tokenizer));
                    },
                    Err(_token) => {
                        /* skip this token and try the one before it */
                    }
                },
            }
        }
    }
    
    fn try_move_next_token(&self, mut tokenizer: tokenizer::Tokenizer, op: TransitionOp) -> Option<(CursorClass, tokenizer::Tokenizer)> {
        let hint = TransitionHint {
            op,
            class: self.get_transition_hint(),
        };
        
        loop {
            match tokenizer.next() {
                None => {
                    return None
                },
                Some(token) => match Self::new_transition(token, &hint) {
                    Ok(cc) => {
                        return Some((cc, tokenizer));
                    },
                    Err(_) => {
                        /* skip this token and try the one after it */
                    }
                },
            }
        }
    }
}

/// Stash away enough information to be able to create a new cursor in the same
/// place if the document structure changed.
#[derive(Debug)]
pub enum PlacementHint {
    Hexdump(hexdump::HexdumpPlacementHint),
    Title,
    Unused
}

impl std::default::Default for PlacementHint {
    fn default() -> PlacementHint{
        PlacementHint::Unused
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
    pub op: TransitionOp,
    pub class: TransitionHintClass,
}

#[derive(Debug, Clone)]
pub enum TransitionHintClass {
    Hexdump(hexdump::HexdumpTransitionHint),
    Unused
}
