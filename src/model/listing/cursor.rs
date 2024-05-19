use std::sync;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::token;
use crate::model::listing::token::TokenKind;
use crate::model::versioned::Versioned;
use crate::logic::tokenizer;

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

    fn is_over(&self, token: token::TokenRef<'_>) -> bool;
    fn get_addr(&self) -> addr::Address;
    fn get_offset(&self) -> addr::Size;
    fn get_token(&self) -> token::TokenRef<'_>;
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
    Punctuation(punctuation::Cursor),
}

#[derive(Debug)]
pub struct Cursor {
    tokenizer: tokenizer::Tokenizer,
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
    fn place_forward(tokenizer: &mut tokenizer::Tokenizer, offset: addr::Address, hint: &PlacementHint) -> Result<CursorClass, PlacementFailure> {
        loop {
            if !match tokenizer.gen_token() {
                tokenizer::TokenGenerationResult::Ok(token) => match CursorClass::new_placement(token, offset, hint) {
                    Ok(cursor) => return Ok(cursor),
                    /* failed to place on this token; try the next */
                    Err(_) => tokenizer.move_next(),
                },
                tokenizer::TokenGenerationResult::Skip => tokenizer.move_next(),
                tokenizer::TokenGenerationResult::Boundary => return Err(PlacementFailure::HitBottomOfAddressSpace)
            } {
                /* move_next() returned false */
                return Err(PlacementFailure::HitBottomOfAddressSpace)
            }
        }        
    }

    fn place_backward(tokenizer: &mut tokenizer::Tokenizer, offset: addr::Address, hint: &PlacementHint) -> Result<CursorClass, PlacementFailure> {
        loop {
            match tokenizer.prev() {
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
        Self::place_tokenizer(document, tokenizer::Tokenizer::at_beginning(root), addr::unit::NULL, &PlacementHint::default())
    }

    pub fn place(document: sync::Arc<document::Document>, path: &structure::Path, offset: addr::Address, hint: PlacementHint) -> Cursor {
        let root = document.root.clone();
        Self::place_tokenizer(document, tokenizer::Tokenizer::at_path(root, path, offset), offset, &hint)
    }

    fn place_tokenizer(document: sync::Arc<document::Document>, origin: tokenizer::Tokenizer, offset: addr::Address, hint: &PlacementHint) -> Self {
        let mut tokenizer = origin.clone();
        
        let class = match CursorClass::place_forward(&mut tokenizer, offset, hint) {
            Ok(cc) => cc,
            Err(PlacementFailure::HitBottomOfAddressSpace) => {
                tokenizer = origin.clone();
                match CursorClass::place_backward(&mut tokenizer, offset, hint) {
                    Ok(cc) => cc,
                    Err(PlacementFailure::HitTopOfAddressSpace) => match hint {
                        PlacementHint::LastDitch => panic!("expected to be able to place cursor somewhere"),
                        _ => return Self::place_tokenizer(document, origin, offset, &PlacementHint::LastDitch),
                    },
                    Err(_) => panic!("unexpected error from CursorClass::place_backward")
                }
            },
            Err(_) => panic!("unexpected error from CursorClass::place_forward")
        };

        Cursor {
            tokenizer,
            class,
            document,
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

            let mut options = options.build();
            let mut tokenizer = self.tokenizer.clone();
            
            document.changes_since(&self.document, &mut |document, change| {
                tokenizer.port_change(
                    &document.root,
                    change,
                    &mut options);
            });

            let offset = tokenizer.structure_position_offset() + options.additional_offset.unwrap_or(addr::unit::ZERO);
            *self = Self::place_tokenizer(document.clone(), tokenizer, offset, &self.class.get_placement_hint());
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
                Some(token) => {
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
                    return None
                },
                Some(token) => {
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
#[derive(Debug, Clone)]
pub enum PlacementHint {
    Hexdump(hexdump::HexdumpPlacementHint),
    Title,
    Punctuation,
    Unused,
    LastDitch,
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
}
