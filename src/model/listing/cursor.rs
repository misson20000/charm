use std::sync;
use std::task;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::line;
use crate::model::listing::stream;
use crate::model::listing::token;
use crate::model::listing::token::TokenKind;
use crate::model::versioned::Versioned;

use enum_dispatch::enum_dispatch;
use tracing::instrument;

pub mod key;

pub mod title;
pub mod hexdump;
pub mod punctuation;

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

#[derive(Debug)]
pub enum EntryError {
    DataNotLoaded,
    InvalidForPosition,
    InvalidForType,
    ChangeError {
        err: document::change::ApplyError,
        document: sync::Arc<document::Document>,
    },
}

#[enum_dispatch]
pub trait CursorClassExt {
    fn update(&mut self, _document: &document::Document) {
    }

    fn is_over(&self, token: token::TokenRef<'_>) -> bool;
    fn get_addr(&self) -> addr::Address;
    fn get_offset(&self) -> addr::Size;
    fn get_token(&self) -> token::TokenRef<'_>;
    fn get_placement_hint(&self) -> PlacementHint;
    fn get_horizontal_position_in_line(&self, line: &line::Line) -> HorizontalPosition;

    fn move_left(&mut self) -> MovementResult;
    fn move_right(&mut self) -> MovementResult;
    fn move_left_large(&mut self) -> MovementResult;
    fn move_right_large(&mut self) -> MovementResult;

    fn enter_hex(&mut self, _document_host: &document::DocumentHost, _document: &document::Document, _nybble: u8) -> Result<MovementResult, EntryError> {
        Err(EntryError::InvalidForType)
    }

    fn invalidate_data(&mut self) {
    }

    fn work(&mut self, _document: &document::Document, _cx: &mut task::Context) -> bool {
        false
    }
}

#[enum_dispatch(CursorClassExt)]
#[derive(Debug)]
pub enum CursorClass {
    Title(title::Cursor),
    Hexdump(hexdump::Cursor),
    Punctuation(punctuation::Cursor),
}

#[derive(Clone, Debug)]
pub enum HorizontalPosition {
    Unspecified,
    Title,
    Hexdump(addr::Size, bool),
}

#[derive(Debug)]
pub struct Cursor {
    position: stream::Position,
    line: line::Line,
    line_begin: stream::Position,
    line_end: stream::Position,
    desired_horizontal_position: Option<HorizontalPosition>,
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

impl CursorClass {
    fn place_forward(position: &mut stream::Position, offset: addr::Address, hint: &PlacementHint) -> Result<CursorClass, PlacementFailure> {
        loop {
            match position.gen_token() {
                stream::TokenGenerationResult::Ok(token) => match CursorClass::new_placement(token, offset, hint) {
                    Ok(cursor) => return Ok(cursor),
                    /* failed to place on this token; try the next */
                    Err(_) => {},
                },
                stream::TokenGenerationResult::Skip => {},
                stream::TokenGenerationResult::Boundary => return Err(PlacementFailure::HitBottomOfAddressSpace)
            };

            if !position.move_next() {
                return Err(PlacementFailure::HitBottomOfAddressSpace)
            }
        }
    }

    fn place_backward(position: &mut stream::Position, offset: addr::Address, hint: &PlacementHint) -> Result<CursorClass, PlacementFailure> {
        loop {
            match position.prev() {
                Some(token) => match CursorClass::new_placement(token, offset, hint) {
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
    pub fn new(document: sync::Arc<document::Document>) -> Cursor {
        let root = document.root.clone();
        Self::place_position(document, stream::Position::at_beginning(root), addr::unit::NULL, &PlacementHint::default())
    }

    pub fn place(document: sync::Arc<document::Document>, path: &structure::Path, offset: addr::Address, hint: PlacementHint) -> Cursor {
        let root = document.root.clone();
        Self::place_position(document, stream::Position::at_path(root, path, offset), offset, &hint)
    }

    fn place_position(document: sync::Arc<document::Document>, origin: stream::Position, offset: addr::Address, hint: &PlacementHint) -> Self {
        let mut position = origin.clone();
        
        let class = match CursorClass::place_forward(&mut position, offset, hint) {
            Ok(cc) => cc,
            Err(PlacementFailure::HitBottomOfAddressSpace) => {
                position = origin.clone();
                match CursorClass::place_backward(&mut position, offset, hint) {
                    Ok(cc) => cc,
                    Err(PlacementFailure::HitTopOfAddressSpace) => match hint {
                        PlacementHint::LastDitch => panic!("expected to be able to place cursor somewhere"),
                        _ => return Self::place_position(document, origin, offset, &PlacementHint::LastDitch),
                    },
                    Err(_) => panic!("unexpected error from CursorClass::place_backward")
                }
            },
            Err(_) => panic!("unexpected error from CursorClass::place_forward")
        };

        let mut line_end = position.clone();
        let (line, mut line_begin, _) = line::Line::containing_position(&mut line_end);
        line_begin.canonicalize_next();
        line_end.canonicalize_next();
        
        Cursor {
            position,
            line,
            line_begin,
            line_end,
            desired_horizontal_position: None,
            class,
            document,
        }
    }

    /// Notify cursor that the underlying document has changed and it needs to renogitiate its position.
    #[instrument]
    fn update_internal(&mut self, document: &sync::Arc<document::Document>, update_mode: UpdateMode) {
        /* if we're using an outdated structure hierarchy root, make a
         * new position and try to put the cursor nearby in the new
         * hierarchy. */
        if self.document.is_outdated(document) {
            let mut options = stream::PortOptionsBuilder::new();
            options = options.additional_offset(self.class.get_offset());

            match update_mode {
                UpdateMode::AfterNewNode => {
                    options = options.prefer_after_new_node();
                },
                _ => {},
            };

            let mut options = options.build();
            let mut position = self.position.clone();
            
            document.changes_since(&self.document, &mut |document, change| {
                position.port_change(
                    &document.root,
                    change,
                    &mut options);
            });

            let offset = position.structure_position_offset() + options.additional_offset.unwrap_or(addr::unit::ZERO);
            *self = Self::place_position(document.clone(), position, offset, &self.class.get_placement_hint());
        }

        self.class.update(document);
    }

    pub fn update(&mut self, document: &sync::Arc<document::Document>) {
        self.update_internal(document, UpdateMode::Default);
    }

    pub fn goto(&mut self, document: sync::Arc<document::Document>, path: &structure::Path, offset: addr::Address, hint: PlacementHint) {
        *self = Self::place(document, path, offset, hint);
    }
    
    pub fn is_over(&self, token: token::TokenRef<'_>) -> bool {
        self.class.is_over(token)
    }

    /// Finds the next token that the supplied function returns Ok(_) for, and mutates our state to account for being on a new token. If we hit the end of the token stream without seeing Ok(_), the state is not mutated.
    fn next_token_matching<T, E, F: Fn(token::Token) -> Result<T, E>>(&mut self, acceptor: F) -> Option<T> {
        let mut position = self.position.clone();
        let mut line = self.line.clone();
        let mut line_begin = self.line_begin.clone();
        let mut line_end = self.line_end.clone();

        let obj = loop {
            match position.next_preincrement() {
                None => return None,
                Some(token) => {
                    while position >= line_end {
                        line_begin = line_end.clone();
                        line = line::Line::next_from_position(&mut line_end);
                        line_end.canonicalize_next();
                    }

                    line_begin.canonicalize_next();

                    assert!(line.iter_tokens().any(|t| t == token.as_ref()));
                    
                    match acceptor(token) {
                        Ok(obj) => break obj,
                        Err(_) => continue,
                    }
                }
            }
        };

        /* We only want to update these in the success case. */
        self.line = line;
        self.line_begin = line_begin;
        self.line_end = line_end;
        self.position = position;
        
        Some(obj)
    }

    /// See [next_token_matching].
    fn prev_token_matching<T, E, F: Fn(token::Token) -> Result<T, E>>(&mut self, acceptor: F) -> Option<T> {
        let mut position = self.position.clone();
        let mut line = self.line.clone();
        let mut line_begin = self.line_begin.clone();
        let mut line_end = self.line_end.clone();

        let obj = loop {
            match position.prev() {
                None => return None,
                Some(token) => {
                    while position < line_begin {
                        line_end = line_begin.clone();
                        line = line::Line::prev_from_position(&mut line_begin);
                        line_begin.canonicalize_next();
                    }

                    line_end.canonicalize_next();

                    assert!(line.iter_tokens().any(|t| t == token.as_ref()));
                    
                    match acceptor(token) {
                        Ok(obj) => break obj,
                        Err(_) => continue,
                    }
                }
            }
        };

        /* We only want to update these in the success case. */
        self.line = line;
        self.line_begin = line_begin;
        self.line_end = line_end;
        self.position = position;
        
        Some(obj)
    }

    fn movement<F>(&mut self, mov: F, hint: TransitionHint) -> MovementResult where F: FnOnce(&mut CursorClass) -> MovementResult {
        self.desired_horizontal_position = None;
        
        match mov(&mut self.class) {
            /* If the movement hit a token boundary, try moving the cursor to another token. */
            MovementResult::HitStart => match self.prev_token_matching(|tok| CursorClass::new_transition(tok, &hint)) {
                Some(cc) => { self.class = cc; MovementResult::Ok }
                None => MovementResult::HitStart
            },
            MovementResult::HitEnd   => match self.next_token_matching(|tok| CursorClass::new_transition(tok, &hint)) {
                Some(cc) => { self.class = cc; MovementResult::Ok }
                None => MovementResult::HitStart
            }
            /* Otherwise, it's a legitimate result. */
            x => x,
        }
    }
    
    pub fn move_left(&mut self)             -> MovementResult { self.movement(|c| c.move_left(),             TransitionHint::UnspecifiedLeft) }
    pub fn move_right(&mut self)            -> MovementResult { self.movement(|c| c.move_right(),            TransitionHint::UnspecifiedRight) }
    //pub fn move_up(&mut self)               -> MovementResult { self.movement(|c| c.move_up(),               TransitionHint::UnspecifiedLeft) }
    //pub fn move_down(&mut self)             -> MovementResult { self.movement(|c| c.move_down(),             TransitionHint::UnspecifiedRight) }
    //pub fn move_to_start_of_line(&mut self) -> MovementResult { self.movement(|c| c.move_to_start_of_line(), TransitionHint::UnspecifiedLeft) }
    //pub fn move_to_end_of_line(&mut self)   -> MovementResult { self.movement(|c| c.move_to_end_of_line(),   TransitionHint::UnspecifiedRight) }
    pub fn move_left_large(&mut self)       -> MovementResult { self.movement(|c| c.move_left_large(),       TransitionHint::MoveLeftLarge) }
    pub fn move_right_large(&mut self)      -> MovementResult { self.movement(|c| c.move_right_large(),      TransitionHint::UnspecifiedRight) }

    fn move_vertically(&mut self, line: line::Line, line_begin: stream::Position, line_end: stream::Position) -> Result<MovementResult, (stream::Position, stream::Position)> {
        let dhp = match &self.desired_horizontal_position {
            Some(x) => x.clone(),
            None => self.class.get_horizontal_position_in_line(&self.line),
        };

        let mut position = line_begin.clone();

        let hint = TransitionHint::MoveVertical {
            horizontal_position: &dhp,
            line: &line,
            line_end: &line_end
        };

        self.class = loop {
            match position.gen_token() {
                stream::TokenGenerationResult::Ok(token) => match CursorClass::new_transition(token, &hint) {
                    Ok(cc) => break cc,
                    Err(_tok) => {},
                },
                stream::TokenGenerationResult::Skip => {},
                stream::TokenGenerationResult::Boundary => return Ok(MovementResult::HitEnd),
            };

            if !position.move_next() {
                return Ok(MovementResult::HitEnd);
            }

            if position >= line_end {
                break loop {                    
                    if let Some(token) = position.prev() {
                        if position < line_begin {
                            return Err((line_begin, line_end));
                        }

                        match CursorClass::new_transition(token, &TransitionHint::EndOfLine) {
                            Ok(cc) => break cc,
                            Err(_tok) => {},
                        }
                    } else {
                        return Ok(MovementResult::HitStart);
                    }
                }
            }
        };

        assert!(line.iter_tokens().any(|t| t == self.class.get_token()));
        
        self.position = position;
        self.line = line;
        self.line_begin = line_begin;
        self.line_end = line_end;
        
        self.desired_horizontal_position = Some(dhp);

        Ok(MovementResult::Ok)
    }
    
    pub fn move_up(&mut self) -> MovementResult {
        let mut line_begin = self.line_begin.clone();
        
        loop {
            let line_end = line_begin.clone();
            let line = line::Line::prev_from_position(&mut line_begin);
            if line.is_empty() {
                return MovementResult::HitStart;
            }
            
            line_begin = match self.move_vertically(line, line_begin, line_end) {
                Ok(mr) => return mr,
                Err((line_begin, _line_end)) => line_begin,
            }
        }
    }
    
    pub fn move_down(&mut self) -> MovementResult {
        let mut line_end = self.line_end.clone();

        loop {
            let line_begin = line_end.clone();
            let line = line::Line::next_from_position(&mut line_end);
            if line.is_empty() {
                return MovementResult::HitEnd;
            }

            line_end = match self.move_vertically(line, line_begin, line_end) {
                Ok(mr) => return mr,
                Err((_line_begin, line_end)) => line_end,
            }
        }
    }
    
    /* previous node */
    pub fn move_prev(&mut self) -> MovementResult {
        todo!();
    }

    /* next node */
    pub fn move_next(&mut self) -> MovementResult {
        todo!();
    }

    pub fn enter_hex(&mut self, document_host: &document::DocumentHost, nybble: u8) -> Result<MovementResult, EntryError> {
        self.class.enter_hex(document_host, &*self.document, nybble).map(|mr| self.movement(|_| mr, TransitionHint::Entry))
    }

    pub fn addr(&self) -> addr::Address {
        self.class.get_addr()
    }
    
    pub fn structure_path(&self) -> structure::Path {
        self.position.structure_path()
    }

    pub fn structure_child_index(&self) -> usize {
        self.position.structure_position_child()
    }
    
    pub fn structure_offset(&self) -> addr::Address {
        self.position.structure_position_offset() + self.class.get_offset()
    }
    
    pub fn document(&self) -> sync::Arc<document::Document> {
        self.document.clone()
    }
    
    pub fn insert_node(&mut self, host: &document::DocumentHost, node: sync::Arc<structure::Node>) -> Result<(), (document::change::ApplyError, sync::Arc<document::Document>)> {
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
        match token {
            token::Token::Title(token) => title::Cursor::new_placement(token, hint).map(CursorClass::Title).map_err(TokenKind::into_token),
            token::Token::Hexdump(token) => hexdump::Cursor::new_placement(token, offset, hint).map(CursorClass::Hexdump).map_err(TokenKind::into_token),
            token::Token::SummaryPunctuation(token) if token.kind.accepts_cursor() => punctuation::Cursor::new_placement(token.into_token(), hint).map(CursorClass::Punctuation),
            token::Token::BlankLine(token) if token.accepts_cursor => punctuation::Cursor::new_placement(token.into_token(), hint).map(CursorClass::Punctuation),
            _ => Err(token)
        }
    }

    /// Attempts to transition a cursor onto the token.
    #[instrument]
    fn new_transition(token: token::Token, hint: &TransitionHint) -> Result<CursorClass, token::Token> {
        match token {
            token::Token::Title(token) => title::Cursor::new_transition(token, hint).map(CursorClass::Title).map_err(TokenKind::into_token),
            token::Token::Hexdump(token) => hexdump::Cursor::new_transition(token, hint).map(CursorClass::Hexdump).map_err(TokenKind::into_token),
            token::Token::SummaryPunctuation(token) if token.kind.accepts_cursor() => punctuation::Cursor::new_transition(token.into_token(), hint).map(CursorClass::Punctuation),
            token::Token::BlankLine(token) if token.accepts_cursor => punctuation::Cursor::new_transition(token.into_token(), hint).map(CursorClass::Punctuation),
            _ => Err(token)
        }
    }

    #[instrument]
    fn try_move_prev_token(&self, mut position: stream::Position, hint: TransitionHint) -> Option<(CursorClass, stream::Position)> {
        loop {
            match position.prev() {
                None => {
                    return None
                },
                Some(token) => {
                    match Self::new_transition(token, &hint) {
                        Ok(cc) => {
                            return Some((cc, position));
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
    fn try_move_next_token(&self, mut position: stream::Position, hint: TransitionHint) -> Option<(CursorClass, stream::Position)> {
        loop {
            match position.next_preincrement() {
                None => {
                    return None
                },
                Some(token) => {
                    match Self::new_transition(token, &hint) {
                        Ok(cc) => {
                            return Some((cc, position));
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
#[derive(Debug, Clone)]
pub enum PlacementHint {
    Hexdump(hexdump::HexdumpPlacementHint),
    Title,
    Punctuation,
    Unused,
    LastDitch,
}

impl std::default::Default for PlacementHint {
    fn default() -> Self {
        Self::Unused
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TransitionHint<'a> {
    MoveLeftLarge,
    Entry,
    UnspecifiedLeft,
    UnspecifiedRight,
    MoveVertical {
        horizontal_position: &'a HorizontalPosition,
        line: &'a line::Line,
        line_end: &'a stream::Position,
    },
    EndOfLine,
}

impl<'a> TransitionHint<'a> {
    pub fn is_left(&self) -> bool {
        match self {
            TransitionHint::MoveLeftLarge => true,
            TransitionHint::Entry => false,
            TransitionHint::UnspecifiedLeft => true,
            TransitionHint::UnspecifiedRight => false,
            TransitionHint::MoveVertical { .. } => false,
            TransitionHint::EndOfLine => true,
        }
    }

    pub fn is_right(&self) -> bool {
        match self {
            TransitionHint::MoveLeftLarge => false,
            TransitionHint::Entry => true,
            TransitionHint::UnspecifiedLeft => false,
            TransitionHint::UnspecifiedRight => true,
            TransitionHint::MoveVertical { .. } => false,
            TransitionHint::EndOfLine => false,
        }
    }

    pub fn is_entry(&self) -> bool {
        match self {
            TransitionHint::MoveLeftLarge => false,
            TransitionHint::Entry => true,
            TransitionHint::UnspecifiedLeft => false,
            TransitionHint::UnspecifiedRight => false,
            TransitionHint::MoveVertical { .. } => false,
            TransitionHint::EndOfLine => false,
        }
    }
}

impl From<(document::change::ApplyError, sync::Arc<document::Document>)> for EntryError {
    fn from(err: (document::change::ApplyError, sync::Arc<document::Document>)) -> EntryError {
        EntryError::ChangeError {
            err: err.0,
            document: err.1,
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
        let document_host = sync::Arc::new(document::Builder::default().host());
        let mut document = document_host.get();
        let mut cursor = Cursor::new(document.clone());

        /* when the cursor is first created, it should be placed on the first hexdump token. */
        assert_eq!(cursor.class.get_token(), token::Token::Hexdump(token::HexdumpToken {
            common: token::TokenCommon {
                node: document.root.clone(),
                node_path: structure::Path::default(),
                node_addr: addr::unit::NULL,
                depth: 1,
            },
            index: 0,
            extent: addr::Extent::sized(addr::unit::NULL, 16.into()),
            line: addr::Extent::sized(addr::unit::NULL, 16.into()),
        }).as_ref());
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == addr::unit::ZERO && hxc.low_nybble == false);

        /* insert a node */
        let child_1 = sync::Arc::new(structure::Node {
            props: structure::Properties {
                name: "child_1".to_string(),
                title_display: structure::TitleDisplay::Minor,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::Hexdump {
                    line_pitch: addr::Size::from(16),
                    gutter_pitch: addr::Size::from(8),
                },
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
        assert_eq!(cursor.class.get_token(), token::Token::Hexdump(token::HexdumpToken {
            common: token::TokenCommon {
                node: document.root.clone(),
                node_path: structure::Path::default(),
                node_addr: addr::unit::NULL,
                depth: 1,
            },
            index: 1,
            extent: addr::Extent::sized(4.into(), 12.into()),
            line: addr::Extent::sized(0.into(), 16.into()),
        }).as_ref());
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == addr::unit::ZERO && hxc.low_nybble == false);

        /* insert another node */
        let child_2 = sync::Arc::new(structure::Node {
            props: structure::Properties {
                name: "child_2".to_string(),
                title_display: structure::TitleDisplay::Minor,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::Hexdump {
                    line_pitch: addr::Size::from(16),
                    gutter_pitch: addr::Size::from(8),
                },
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
        assert_eq!(cursor.class.get_token(), token::Token::Hexdump(token::HexdumpToken {
            common: token::TokenCommon {
                node: document.root.clone(),
                node_path: structure::Path::default(),
                node_addr: addr::unit::NULL,
                depth: 1,
            },
            index: 2,
            extent: addr::Extent::sized(8.into(), 8.into()),
            line: addr::Extent::sized(0.into(), 16.into()),
        }).as_ref());
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == addr::unit::ZERO && hxc.low_nybble == false);
    }

    #[test]
    fn node_insertion_simpler() {
        let document_host = sync::Arc::new(document::Builder::default().host());
        let mut document = document_host.get();
        let mut cursor = Cursor::new(document.clone());

        /* when the cursor is first created, it should be placed on the first hexdump token. */
        assert_eq!(cursor.class.get_token(), token::Token::Hexdump(token::HexdumpToken {
            common: token::TokenCommon {
                node: document.root.clone(),
                node_path: structure::Path::default(),
                node_addr: addr::unit::NULL,
                depth: 1,
            },
            index: 0,
            extent: addr::Extent::sized(addr::unit::NULL, 16.into()),
            line: addr::Extent::sized(addr::unit::NULL, 16.into()),
        }).as_ref());
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == addr::unit::ZERO && hxc.low_nybble == false);

        /* move the cursor to offset 4 (8 nybbles) */
        for _ in 0..8 {
            cursor.move_right();
        }

        /* make sure it winds up in the correct position on the correct token */
        assert_eq!(cursor.class.get_token(), token::Token::Hexdump(token::HexdumpToken {
            common: token::TokenCommon {
                node: document.root.clone(),
                node_path: structure::Path::default(),
                node_addr: addr::unit::NULL,
                depth: 1,
            },
            index: 0,
            extent: addr::Extent::sized(addr::unit::NULL, 16.into()),
            line: addr::Extent::sized(addr::unit::NULL, 16.into()),
        }).as_ref());
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == addr::Size::from(4) && hxc.low_nybble == false);

        /* insert a node */
        let child = sync::Arc::new(structure::Node {
            props: structure::Properties {
                name: "child".to_string(),
                title_display: structure::TitleDisplay::Minor,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::Hexdump {
                    line_pitch: addr::Size::from(16),
                    gutter_pitch: addr::Size::from(8),
                },
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
        assert_eq!(cursor.class.get_token(), token::Token::Hexdump(token::HexdumpToken {
            common: token::TokenCommon {
                node: document.root.clone(),
                node_path: structure::Path::default(),
                node_addr: addr::unit::NULL,
                depth: 1,
            },
            index: 1,
            extent: addr::Extent::sized(8.into(), 8.into()),
            line: addr::Extent::sized(addr::unit::NULL, 16.into()),
        }).as_ref());
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == addr::unit::ZERO && hxc.low_nybble == false);
    }

    #[test]
    fn node_insertion_on_top_of() {
        let document_host = sync::Arc::new(document::Builder::default().host());
        let document = document_host.get();
        let mut cursor = Cursor::place(document.clone(), &vec![], 0x24.into(), PlacementHint::Unused);

        assert_eq!(cursor.class.get_token(), token::Token::Hexdump(token::HexdumpToken {
            common: token::TokenCommon {
                node: document.root.clone(),
                node_path: structure::Path::default(),
                node_addr: addr::unit::NULL,
                depth: 1,
            },
            index: 0,
            extent: addr::Extent::sized(0x20.into(), 0x10.into()),
            line: addr::Extent::sized(0x20.into(), 0x10.into()),
        }).as_ref());
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == 0x4.into() && hxc.low_nybble == false);

        let node = sync::Arc::new(structure::Node {
            props: structure::Properties {
                name: "child".to_string(),
                title_display: structure::TitleDisplay::Minor,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::Hexdump {
                    line_pitch: addr::Size::from(16),
                    gutter_pitch: addr::Size::from(8),
                },
                locked: false,
            },
            children: vec::Vec::new(),
            size: addr::Size::from(0x30),
        });
        
        let document = document_host.change(document.insert_node(
            vec![],
            0,
            structure::Childhood::new(node.clone(), 0x12.into())
        )).unwrap();
        
        /* port the cursor over */
        cursor.update(&document);
        
        /* make sure it winds up in the correct place */
        assert_eq!(cursor.class.get_token(), token::Token::Hexdump(token::HexdumpToken {
            common: token::TokenCommon {
                node: node.clone(),
                node_path: vec![0],
                node_addr: 0x12.into(),
                depth: 2,
            },
            index: 0,
            extent: addr::Extent::sized(0x10.into(), 0x10.into()),
            line: addr::Extent::sized(0x10.into(), 0x10.into()),
        }).as_ref());
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == 0x2.into() && hxc.low_nybble == false);
    }
    
    #[test]
    fn node_insertion_after() {
        let document_host = sync::Arc::new(document::Builder::default().host());
        let document = document_host.get();
        let mut cursor = Cursor::place(document.clone(), &vec![], 0x0.into(), PlacementHint::Unused);

        assert_eq!(cursor.class.get_token(), token::Token::Hexdump(token::HexdumpToken {
            common: token::TokenCommon {
                node: document.root.clone(),
                node_path: structure::Path::default(),
                node_addr: addr::unit::NULL,
                depth: 1,
            },
            index: 0,
            extent: addr::Extent::sized(0x0.into(), 0x10.into()),
            line: addr::Extent::sized(0x0.into(), 0x10.into()),
        }).as_ref());
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == 0x0.into() && hxc.low_nybble == false);

        let node = sync::Arc::new(structure::Node {
            props: structure::Properties {
                name: "child".to_string(),
                title_display: structure::TitleDisplay::Minor,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::Hexdump {
                    line_pitch: addr::Size::from(16),
                    gutter_pitch: addr::Size::from(8),
                },
                locked: false,
            },
            children: vec::Vec::new(),
            size: addr::Size::from(0x30),
        });
        
        let document = document_host.change(document.insert_node(
            vec![],
            0,
            structure::Childhood::new(node.clone(), 0x12.into())
        )).unwrap();
        
        /* port the cursor over */
        cursor.update(&document);
        
        /* make sure it winds up in the correct place */
        assert_eq!(cursor.class.get_token(), token::Token::Hexdump(token::HexdumpToken {
            common: token::TokenCommon {
                node: document.root.clone(),
                node_path: vec![],
                node_addr: 0x0.into(),
                depth: 1,
            },
            index: 0,
            extent: addr::Extent::sized(0x0.into(), 0x10.into()),
            line: addr::Extent::sized(0x0.into(), 0x10.into()),
        }).as_ref());
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == 0x0.into() && hxc.low_nybble == false);
    }

    #[test]
    fn can_place_with_no_content() {
        let root = structure::Node::builder()
            .name("root")
            .size(0x40)
            .content_display(structure::ContentDisplay::None)
            .build();
        
        let document = sync::Arc::new(document::Builder::new(root).build());

        Cursor::place(document, &vec![], 0x0.into(), PlacementHint::Unused);
    }

    #[test]
    fn vertical_and_horizontal_movement() {
        let root = structure::Node::builder()
            .name("root")
            .size(0x40)
            .child(0x0, |b| b
                   .name("child")
                   .size(0x10))
            .build();
        
        let document = sync::Arc::new(document::Builder::new(root).build());
        
        let mut cursor = Cursor::place(document, &vec![], 0x0.into(), PlacementHint::Unused);
        println!("initial:");
        println!("  line: {:?}", cursor.line);
        println!("  line positions: {:?}-{:?}", cursor.line_begin, cursor.line_end);
        cursor.move_up();
        println!("after move up 1:");
        println!("  line: {:?}", cursor.line);
        println!("  line positions: {:?}-{:?}", cursor.line_begin, cursor.line_end);
        cursor.move_up();
        println!("after move up 2:");
        println!("  line: {:?}", cursor.line);
        println!("  line positions: {:?}-{:?}", cursor.line_begin, cursor.line_end);
        cursor.move_right();
    }

    #[test]
    fn summary_swap() {
        let document_host = sync::Arc::new(document::Builder::default().host());
        let document = document_host.get();
        let mut cursor = Cursor::new(document.clone());

        /* when the cursor is first created, it should be placed on the first hexdump token. */
        assert_eq!(cursor.class.get_token(), token::Token::Hexdump(token::HexdumpToken {
            common: token::TokenCommon {
                node: document.root.clone(),
                node_path: structure::Path::default(),
                node_addr: addr::unit::NULL,
                depth: 1,
            },
            index: 0,
            extent: addr::Extent::sized(addr::unit::NULL, 16.into()),
            line: addr::Extent::sized(addr::unit::NULL, 16.into()),
        }).as_ref());
        assert_matches!(&cursor.class, CursorClass::Hexdump(hxc) if hxc.offset == addr::unit::ZERO && hxc.low_nybble == false);

        let document = document_host.change(document.alter_node(vec![], structure::Properties {
            name: "root".to_string(),
            children_display: structure::ChildrenDisplay::Summary,
            ..Default::default()
        })).unwrap();

        cursor.update(&document);

        assert_eq!(cursor.class.get_token(), token::Token::SummaryPunctuation(token::SummaryPunctuationToken {
            common: token::TokenCommon {
                node: document.root.clone(),
                node_path: structure::Path::default(),
                node_addr: addr::unit::NULL,
                depth: 0,
            },
            kind: token::PunctuationKind::CloseBracket,
            index: 0,
        }).as_ref());
    }
}
