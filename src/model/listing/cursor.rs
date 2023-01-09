use std::sync;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::token;
use crate::logic::tokenizer;

use enum_dispatch::enum_dispatch;
use tracing::{Level, event, instrument};

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
    fn update(&mut self, _document: &document::Document) {
    }

    fn is_over(&self, token: &token::Token) -> bool;
    fn get_addr(&self) -> addr::Address;
    fn get_offset(&self) -> addr::Size;
    fn get_token(&self) -> &token::Token;
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
        tokenizer.next_preincrement();
        let mut option = tokenizer.prev();
        loop {
            match option {
                Some(token) => match CursorClass::new_placement(token, addr, hint) {
                    Ok(cursor) => return Ok(cursor),
                    /* failed to place on this token; try the next */
                    Err(_) => option = tokenizer.next_preincrement(),
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

    /// Notify cursor that the underlying document has changed and it needs to renogitiate its position.
    #[instrument]
    pub fn update(&mut self, document: &sync::Arc<document::Document>) {
        /* if we're using an outdated structure hierarchy root, make a
         * new tokenizer and try to put the cursor nearby in the new
         * hierarchy. */
        if self.document.is_outdated(document) {
            tracing::event!(Level::DEBUG, "porting the cursor tokenizer");
            self.tokenizer.port_doc(
                &self.document,
                document,
                &tokenizer::PortOptionsBuilder::new().additional_offset(self.class.get_offset()).build());
            
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
        }

        self.class.update(document);
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

    pub fn insert_node(&self, host: &document::DocumentHost, node: sync::Arc<structure::Node>) -> Result<(), document::change::ApplyError> {
        host.insert_node(
            &self.document,
            self.tokenizer.structure_path(),
            self.tokenizer.structure_position_child(),
            self.tokenizer.structure_position_offset() + self.class.get_offset(),
            node)
    }
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
            token::TokenClass::Title => title::Cursor::new_placement(token, addr, hint).map(CursorClass::Title),
            token::TokenClass::Hexdump(_) => hexdump::Cursor::new_placement(token, addr, hint).map(CursorClass::Hexdump),
            _ => Err(token)
        }
    }

    /// Attempts to transition a cursor onto the token.
    #[instrument]
    fn new_transition(token: token::Token, hint: &TransitionHint) -> Result<CursorClass, token::Token> {
        event!(Level::DEBUG, "making cursor class from transition onto {:?}", token);
        match token.class {
            token::TokenClass::Title => title::Cursor::new_transition(token, hint).map(CursorClass::Title),
            token::TokenClass::Hexdump(_) => hexdump::Cursor::new_transition(token, hint).map(CursorClass::Hexdump),
            _ => Err(token)
        }
    }

    #[instrument]
    fn try_move_prev_token(&self, mut tokenizer: tokenizer::Tokenizer, op: TransitionOp) -> Option<(CursorClass, tokenizer::Tokenizer)> {
        let hint = TransitionHint {
            op,
            class: self.get_transition_hint(),
        };
        
        loop {
            match tokenizer.prev() {
                None => {
                    event!(Level::DEBUG, "hit beginning of token stream");
                    return None
                },
                Some(token) => {
                    event!(Level::DEBUG, "got token {:?}", token);
                    match Self::new_transition(token, &hint) {
                        Ok(cc) => {
                            return Some((cc, tokenizer));
                        },
                        Err(_token) => {
                            /* skip this token and try the one before it */
                        }
                    }
                },
            }
        }
    }

    #[instrument]
    fn try_move_next_token(&self, mut tokenizer: tokenizer::Tokenizer, op: TransitionOp) -> Option<(CursorClass, tokenizer::Tokenizer)> {
        let hint = TransitionHint {
            op,
            class: self.get_transition_hint(),
        };
        
        loop {
            match tokenizer.next_preincrement() {
                None => {
                    event!(Level::DEBUG, "hit end of token stream");
                    return None
                },
                Some(token) => {
                    event!(Level::DEBUG, "got token {:?}", token);
                    match Self::new_transition(token, &hint) {
                        Ok(cc) => {
                            return Some((cc, tokenizer));
                        },
                        Err(_) => {
                            /* skip this token and try the one after it */
                        }
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

#[cfg(test)]
mod tests {
    use super::*;

    use std::vec;
    
    use assert_matches::assert_matches;
    
    #[test]
    fn node_insertion() {
        let document_host = sync::Arc::new(document::DocumentHost::new(document::Document::invalid()));
        let mut document = document_host.get();
        let mut cursor = Cursor::new(document.clone()).unwrap();

        /* when the cursor is first created, it should be placed on the first hexdump token. */
        assert_eq!(cursor.class.get_token(), &token::Token {
            class: token::TokenClass::Hexdump(addr::Extent::sized(addr::unit::NULL, 16.into())),
            node: document.root.clone(),
            node_addr: addr::unit::NULL,
            depth: 0,
            newline: true,
        });
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == addr::unit::ZERO && hxc.low_nybble == false);

        /* insert a node */
        let child_1 = sync::Arc::new(structure::Node {
            props: structure::Properties {
                name: "child_1".to_string(),
                title_display: structure::TitleDisplay::Minor,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::Hexdump(addr::Size::from(16)),
                locked: false,
            },
            children: vec::Vec::new(),
            size: addr::Size::from(4),
        });
        cursor.insert_node(&document_host, child_1.clone()).unwrap();
        document = document_host.get();

        /* port the cursor over */
        cursor.update(&document);

        /* make sure it winds up on the new node */
        assert_eq!(cursor.class.get_token(), &token::Token {
            class: token::TokenClass::Hexdump(addr::Extent::sized(addr::unit::NULL, 4.into())),
            node: child_1.clone(),
            node_addr: addr::unit::NULL,
            depth: 1,
            newline: true,
        });
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == addr::unit::ZERO && hxc.low_nybble == false);

        /* move the cursor off the first child */
        for _ in 0..8 {
            cursor.move_right();
        }

        /* make sure it winds up immediately after the first child */
        assert_eq!(cursor.class.get_token(), &token::Token {
            class: token::TokenClass::Hexdump(addr::Extent::sized(4.into(), 12.into())),
            node: document.root.clone(),
            node_addr: addr::unit::NULL,
            depth: 0,
            newline: true,
        });
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == addr::unit::ZERO && hxc.low_nybble == false);

        /* insert another node */
        let child_2 = sync::Arc::new(structure::Node {
            props: structure::Properties {
                name: "child_2".to_string(),
                title_display: structure::TitleDisplay::Minor,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::Hexdump(addr::Size::from(16)),
                locked: false,
            },
            children: vec::Vec::new(),
            size: addr::Size::from(4),
        });
        cursor.insert_node(&document_host, child_2.clone()).unwrap();
        document = document_host.get();

        /* port the cursor over */
        cursor.update(&document);
        
        /* make sure it winds up on the second new node */
        assert_eq!(cursor.class.get_token(), &token::Token {
            class: token::TokenClass::Hexdump(addr::Extent::sized(addr::unit::NULL, 4.into())),
            node: child_2.clone(),
            node_addr: addr::unit::NULL,
            depth: 1,
            newline: true,
        });
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == addr::unit::ZERO && hxc.low_nybble == false);
    }

    #[test]
    fn node_insertion_simpler() {
        let document_host = sync::Arc::new(document::DocumentHost::new(document::Document::invalid()));
        let mut document = document_host.get();
        let mut cursor = Cursor::new(document.clone()).unwrap();

        /* when the cursor is first created, it should be placed on the first hexdump token. */
        assert_eq!(cursor.class.get_token(), &token::Token {
            class: token::TokenClass::Hexdump(addr::Extent::sized(addr::unit::NULL, 16.into())),
            node: document.root.clone(),
            node_addr: addr::unit::NULL,
            depth: 0,
            newline: true,
        });
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == addr::unit::ZERO && hxc.low_nybble == false);

        /* move the cursor to offset 4 (8 nybbles) */
        for _ in 0..8 {
            cursor.move_right();
        }

        /* make sure it winds up in the correct position on the correct token */
        assert_eq!(cursor.class.get_token(), &token::Token {
            class: token::TokenClass::Hexdump(addr::Extent::sized(addr::unit::NULL, 16.into())),
            node: document.root.clone(),
            node_addr: addr::unit::NULL,
            depth: 0,
            newline: true,
        });
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == addr::Size::from(4) && hxc.low_nybble == false);

        /* insert a node */
        let child = sync::Arc::new(structure::Node {
            props: structure::Properties {
                name: "child".to_string(),
                title_display: structure::TitleDisplay::Minor,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::Hexdump(addr::Size::from(16)),
                locked: false,
            },
            children: vec::Vec::new(),
            size: addr::Size::from(4),
        });
        cursor.insert_node(&document_host, child.clone()).unwrap();
        document = document_host.get();
        
        /* port the cursor over */
        cursor.update(&document);
        
        /* make sure it winds up on the new node */
        assert_eq!(cursor.class.get_token(), &token::Token {
            class: token::TokenClass::Hexdump(addr::Extent::sized(addr::unit::NULL, 4.into())),
            node: child.clone(),
            node_addr: addr::unit::NULL,
            depth: 1,
            newline: true,
        });
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == addr::unit::ZERO && hxc.low_nybble == false);
    }
}
