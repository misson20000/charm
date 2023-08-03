use std::sync;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::token;
use crate::model::versioned::Versioned;
use crate::logic::tokenizer;

use enum_dispatch::enum_dispatch;
use tracing::instrument;

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
    NoSuitableTokens,
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
    fn get_vertical_transition_hint(&self) -> VerticalTransitionHint;

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
    line_addr: addr::Address,
    vertical_transition_hint: VerticalTransitionHint,
    pub class: CursorClass,
    document: sync::Arc<document::Document>,
}

#[derive(Debug)]
enum UpdateMode {
    /* Try to maintain the current cursor position. */
    Default,

    /* If the cursor is affected by an InsertNode, try to put the cursor after the new node. */
    AfterNewNode,
}

/// Stash away enough information to be able to create a new cursor in the same
/// place if the document structure changed.
#[derive(Debug)]
pub enum PlacementHint {
    Hexdump(hexdump::HexdumpPlacementHint),
    Title,
    Unused
}

#[derive(Debug, Clone, Copy)]
pub enum TransitionOp {
    Left,
    Right,
    Up,
    Down,
    
    //EntryStandard,
    //EntryUTF8,
}

#[derive(Debug, Clone)]
pub enum VerticalTransitionHint {
    Hexdump(hexdump::HexdumpVerticalTransitionHint),
    Unused,
}

impl CursorClass {
    fn place(tokenizer: &mut tokenizer::Tokenizer, offset: addr::Address, hint: &PlacementHint) -> Result<CursorClass, PlacementFailure> {
        let starting_position = tokenizer.clone();
        
        tokenizer.next_preincrement();
        let mut option = tokenizer.prev();
        
        loop {
            match option {
                Some(token) => match CursorClass::new_placement(token, offset, hint) {
                    Ok(cursor) => return Ok(cursor),
                    /* failed to place on this token; try the next */
                    Err(_) => option = tokenizer.next_preincrement(),
                },

                None => break
            }
        }
        
        /* We hit the end of the token stream!! Revert back to where we started from and try going backwards instead. */
        let mut tokenizer = starting_position;
        
        loop {
            match tokenizer.prev() {
                Some(token) => match CursorClass::new_placement(token, offset, hint) {
                    Ok(cursor) => return Ok(cursor),
                    /* failed to place on this token; try the previous */
                    Err(_) => continue,
                },
                
                None => return Err(PlacementFailure::NoSuitableTokens)
            }
        }        
    }
}

impl Cursor {
    pub fn new(document: sync::Arc<document::Document>) -> Result<Cursor, PlacementFailure> {
        Self::place_on_token(tokenizer::Tokenizer::at_beginning(document.root.clone()), document, addr::unit::NULL, &PlacementHint::default())
    }

    pub fn place(document: sync::Arc<document::Document>, path: &structure::Path, offset: addr::Address, hint: PlacementHint) -> Result<Cursor, PlacementFailure> {
        Self::place_on_token(tokenizer::Tokenizer::at_path(document.root.clone(), path, offset), document, offset, &hint)
    }

    fn place_on_token(mut tokenizer: tokenizer::Tokenizer, document: sync::Arc<document::Document>, offset: addr::Address, hint: &PlacementHint) -> Result<Cursor, PlacementFailure> {
        let class = CursorClass::place(&mut tokenizer, offset, &hint)?;

        Ok(Cursor {
            class,
            line_addr: Self::find_beginning_of_line(tokenizer.clone()),
            vertical_transition_hint: VerticalTransitionHint::Unused,
            tokenizer,
            document,
        })
    }

    fn find_beginning_of_line(mut tokenizer: tokenizer::Tokenizer) -> addr::Address {
        let mut last_addr = tokenizer.get_addr();

        loop {
            match tokenizer.prev() {
                Some(token) if token.newline => break last_addr,
                Some(_) => last_addr = tokenizer.get_addr(),
                None => break last_addr,
            }
        }
    }
    
    /// Notify cursor that the underlying document has changed and it needs to renogitiate its position.
    #[instrument]
    fn update_internal(&mut self, document: &sync::Arc<document::Document>, update_mode: UpdateMode) {
        /* if we're using an outdated structure hierarchy root, make a
         * new tokenizer and try to put the cursor nearby in the new
         * hierarchy. */
        if self.document.is_outdated(document) {
            let mut options = tokenizer::PortOptionsBuilder::new();
            options = options.additional_offset(self.class.get_offset());

            match update_mode {
                UpdateMode::AfterNewNode => {
                    options = options.prefer_after_new_node();
                },
                _ => {},
            };

            let options = options.build();

            let mut tokenizer = self.tokenizer.clone();
            
            document.changes_since(&self.document, &mut |document, change| {
                tokenizer.port_change(
                    &document.root,
                    change,
                    &options);
            });

            *self = match Self::place_on_token(tokenizer, document.clone(), self.class.get_addr(), &self.class.get_placement_hint()) {
                Ok(cursor) => cursor,
                Err(PlacementFailure::NoSuitableTokens) => panic!("expected to be able to place cursor somewhere"),
            };
        }

        self.class.update(document);
    }

    pub fn update(&mut self, document: &sync::Arc<document::Document>) {
        self.update_internal(document, UpdateMode::Default);
    }
    
    pub fn goto(&mut self, path: &structure::Path, offset: addr::Address) -> Result<(), PlacementFailure> {
        Self::place(self.document.clone(), path, offset, PlacementHint::Unused).map(|new| { *self = new; })
    }

    pub fn is_over(&self, token: &token::Token) -> bool {
        self.class.is_over(token)
    }

    #[instrument]
    fn try_move_prev_token(&mut self, op: TransitionOp) -> MovementResult {
        let mut tokenizer = self.tokenizer.clone();
        let mut newline = false;
        
        loop {
            match tokenizer.prev() {
                None => return MovementResult::HitStart,
                Some(token) => {
                    newline = newline || token.newline;
                    
                    match CursorClass::new_transition(token, op, &self.vertical_transition_hint) {
                        Ok(cc) => {
                            if newline {
                                self.line_addr = Self::find_beginning_of_line(tokenizer.clone());
                            }
                            
                            self.class = cc;
                            self.tokenizer = tokenizer;
                            
                            return MovementResult::Ok;
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
    fn try_move_next_token(&mut self, op: TransitionOp) -> MovementResult {
        let mut last_token_had_newline = self.class.get_token().newline;
        let mut line_addr = self.line_addr;
        let mut tokenizer = self.tokenizer.clone();
        
        loop {
            match tokenizer.next_preincrement() {
                None => return MovementResult::HitEnd,
                Some(token) => {
                    if last_token_had_newline {
                        line_addr = tokenizer.get_addr();
                    }
                    
                    match CursorClass::new_transition(token, op, &self.vertical_transition_hint) {
                        Ok(cc) => {
                            self.line_addr = line_addr;
                            self.class = cc;
                            self.tokenizer = tokenizer;
                            return MovementResult::Ok;
                        },
                        Err(token) => {
                            /* skip this token and try the one after it */
                            last_token_had_newline = token.newline;
                        }
                    };
                },
            }
        }
    }

    /*
    pub fn move_left(&mut self)             -> MovementResult { self.movement(|c| c.move_left(),             TransitionOp::UnspecifiedLeft) }
    pub fn move_right(&mut self)            -> MovementResult { self.movement(|c| c.move_right(),            TransitionOp::UnspecifiedRight) }
    pub fn move_to_start_of_line(&mut self) -> MovementResult { self.movement(|c| c.move_to_start_of_line(), TransitionOp::UnspecifiedLeft) }
    pub fn move_to_end_of_line(&mut self)   -> MovementResult { self.movement(|c| c.move_to_end_of_line(),   TransitionOp::UnspecifiedRight) }
    pub fn move_left_large(&mut self)       -> MovementResult { self.movement(|c| c.move_left_large(),       TransitionOp::MoveLeftLarge) }
    pub fn move_right_large(&mut self)      -> MovementResult { self.movement(|c| c.move_right_large(),      TransitionOp::UnspecifiedRight) }

    pub fn move_up(&mut self)               -> MovementResult { self.movement(|c| c.move_up(),               TransitionOp::PreviousLine) }
    pub fn move_down(&mut self)             -> MovementResult { self.movement(|c| c.move_down(),             TransitionOp::NextLine) }
     */

    pub fn move_left(&mut self) -> MovementResult {
        let r = match self.class.move_left() {
            MovementResult::HitStart => self.try_move_prev_token(TransitionOp::Left),
            x => x,
        };

        self.vertical_transition_hint = self.class.get_vertical_transition_hint();

        r
    }

    pub fn move_right(&mut self) -> MovementResult {
        let r = match self.class.move_right() {
            MovementResult::HitEnd => self.try_move_next_token(TransitionOp::Right),
            x => x,
        };

        self.vertical_transition_hint = self.class.get_vertical_transition_hint();

        r
    }

    pub fn move_up(&mut self) -> MovementResult {
        let mut tokenizer = self.tokenizer.clone();

        let mut found_newline = false;
        
        loop {
            match tokenizer.prev() {
                None => break MovementResult::HitStart,
                Some(token) if token.newline || found_newline => {
                    found_newline = true;

                    match CursorClass::new_transition(token, TransitionOp::Up, &self.vertical_transition_hint) {
                        Ok(cc) => {
                            self.line_addr = Self::find_beginning_of_line(tokenizer.clone());
                            self.class = cc;
                            self.tokenizer = tokenizer;
                            
                            break MovementResult::Ok;
                        },
                        Err(_token) => {
                            /* Skip this token and look for one on this line that we can actually transition onto in this cursor mode. */
                        }
                    }
                },
                Some(_) => {
                    /* Skip this token and keep looking for a newline */
                }
            }
        }
    }

    pub fn move_down(&mut self) -> MovementResult {
        let mut found_newline = self.class.get_token().newline;
        let mut line_addr_opt = None;
        let mut tokenizer = self.tokenizer.clone();

        loop {
            match tokenizer.next_preincrement() {
                None => break MovementResult::HitStart,
                Some(token) => {
                    let tok_newline = token.newline;
                    
                    if found_newline {
                        /* If this is the first token on the new line, set line_addr. */
                        let line_addr = line_addr_opt.unwrap_or_else(|| tokenizer.get_addr());
                        line_addr_opt = Some(line_addr);
                        
                        match CursorClass::new_transition(token, TransitionOp::Down, &self.vertical_transition_hint) {
                            Ok(cc) => {
                                self.line_addr = line_addr;
                                self.class = cc;
                                self.tokenizer = tokenizer;

                                break MovementResult::Ok;
                            },
                            Err(_token) => {
                                /* fall-through into skip path */
                            }
                        }
                    }

                    /* Skip this token until we find one on the new line that we can transition onto. */
                    if tok_newline {
                        found_newline = true;

                        /* If we had to skip a line because we couldn't transition onto any of its tokens, make sure to reset line_addr. */
                        line_addr_opt = None;
                    }
                }
            }
        }
    }
    
    /*
    pub fn enter_standard(&mut self, document_host: &document::DocumentHost, insert: bool, key: &key::Key) -> Result<MovementResult, EntryError> {
        self.class.enter_standard(document_host, insert, key).map(|mr| self.movement(|_| mr, TransitionOp::EntryStandard))
    }

    pub fn enter_utf8(&mut self, document_host: &document::DocumentHost, insert: bool, key: &key::Key) -> Result<MovementResult, EntryError> {
        self.class.enter_utf8(document_host, insert, key).map(|mr| self.movement(|_| mr, TransitionOp::EntryUTF8))
    }
     */

    pub fn structure_path(&self) -> structure::Path {
        self.tokenizer.structure_path()
    }

    pub fn structure_child_index(&self) -> usize {
        self.tokenizer.structure_position_child()
    }
    
    pub fn structure_offset(&self) -> addr::Address {
        self.tokenizer.structure_position_offset() + self.class.get_offset()
    }
    
    pub fn document(&self) -> sync::Arc<document::Document> {
        self.document.clone()
    }
    
    pub fn insert_node(&mut self, host: &document::DocumentHost, node: sync::Arc<structure::Node>) -> Result<(), document::change::ApplyError> {
        host.change(
            self.document.insert_node(
                self.structure_path(),
                self.structure_child_index(),
                structure::Childhood::new(node, self.structure_offset()))
        ).map(|new_doc| {
            self.update_internal(&new_doc, UpdateMode::AfterNewNode);
        })
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
    fn new_placement(token: token::Token, offset: addr::Address, hint: &PlacementHint) -> Result<CursorClass, token::Token> {
        match token.class {
            token::TokenClass::Title => title::Cursor::new_placement(token, offset, hint).map(CursorClass::Title),
            token::TokenClass::Hexdump(_) => hexdump::Cursor::new_placement(token, offset, hint).map(CursorClass::Hexdump),
            _ => Err(token)
        }
    }

    /// Attempts to transition a cursor onto the token.
    #[instrument]
    fn new_transition(token: token::Token, op: TransitionOp, vth: &VerticalTransitionHint) -> Result<CursorClass, token::Token> {
        match token.class {
            token::TokenClass::Title => title::Cursor::new_transition(token, op, vth).map(CursorClass::Title),
            token::TokenClass::Hexdump(_) => hexdump::Cursor::new_transition(token, op, vth).map(CursorClass::Hexdump),
            _ => Err(token)
        }
    }
}

impl std::default::Default for PlacementHint {
    fn default() -> PlacementHint{
        PlacementHint::Unused
    }
}

impl TransitionOp {
    pub fn is_left(&self) -> bool {
        match self {
            TransitionOp::Left => true,
            TransitionOp::Right => false,
            TransitionOp::Up => false,
            TransitionOp::Down => false,
        }
    }

    pub fn is_right(&self) -> bool {
        match self {
            TransitionOp::Left => false,
            TransitionOp::Right => true,
            TransitionOp::Up => false,
            TransitionOp::Down => false,
        }
    }

    pub fn is_entry(&self) -> bool {
        match self {
            TransitionOp::Left => false,
            TransitionOp::Right => false,
            TransitionOp::Up => false,
            TransitionOp::Down => false,
        }
    }

    pub fn is_vertical(&self) -> bool {
        match self {
            TransitionOp::Left => false,
            TransitionOp::Right => false,
            TransitionOp::Up => true,
            TransitionOp::Down => true,
        }
    }
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
            node_path: structure::Path::default(),
            node_addr: addr::unit::NULL,
            depth: 1,
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

        /* make sure it winds up after the new node */
        assert_eq!(cursor.class.get_token(), &token::Token {
            class: token::TokenClass::Hexdump(addr::Extent::sized(4.into(), 12.into())),
            node: document.root.clone(),
            node_path: structure::Path::default(),
            node_addr: addr::unit::NULL,
            depth: 1,
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
        
        /* make sure it winds up after the second new node */
        assert_eq!(cursor.class.get_token(), &token::Token {
            class: token::TokenClass::Hexdump(addr::Extent::sized(8.into(), 8.into())),
            node: document.root.clone(),
            node_path: structure::Path::default(),
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
            node_path: structure::Path::default(),
            node_addr: addr::unit::NULL,
            depth: 1,
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
            node_path: structure::Path::default(),
            node_addr: addr::unit::NULL,
            depth: 1,
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
        
        /* make sure it winds up after the new node */
        assert_eq!(cursor.class.get_token(), &token::Token {
            class: token::TokenClass::Hexdump(addr::Extent::sized(8.into(), 8.into())),
            node: document.root.clone(),
            node_path: structure::Path::default(),
            node_addr: addr::unit::NULL,
            depth: 1,
            newline: true,
        });
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == addr::unit::ZERO && hxc.low_nybble == false);
    }
}
