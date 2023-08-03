use crate::model::addr;
use crate::model::listing::cursor;
use crate::model::listing::token;

use tracing::instrument;

#[derive(Debug)]
pub struct Cursor {
    token: token::Token,
}

impl Cursor {
    pub fn new_transition(token: token::Token, op: cursor::TransitionOp, _vth: &cursor::VerticalTransitionHint) -> Result<Cursor, token::Token> {
        if op.is_entry() {
            /* skip over title tokens for entry */
            Err(token)
        } else {
            Ok(Cursor {
                token
            })
        }
    }
    
    pub fn new_placement(token: token::Token, _offset: addr::Address, hint: &cursor::PlacementHint) -> Result<Cursor, token::Token> {
        match hint {
            /* we only place the cursor on a break header if explicitly requested;
             * otherwise, we prefer to place it on a content token. */
            cursor::PlacementHint::Title => Ok(Cursor {
                token
            }),
            _ => Err(token)
        }
    }
}

impl cursor::CursorClassExt for Cursor {
    fn is_over(&self, token: &token::Token) -> bool {
        &self.token == token
    }
    
    fn get_addr(&self) -> addr::Address {
        self.token.node_addr
    }

    fn get_offset(&self) -> addr::Size {
        addr::unit::ZERO
    }

    fn get_token(&self) -> &token::Token {
        &self.token
    }
    
    fn get_placement_hint(&self) -> cursor::PlacementHint {
        cursor::PlacementHint::Title
    }
    
    fn get_vertical_transition_hint(&self) -> cursor::VerticalTransitionHint {
        cursor::VerticalTransitionHint::Unused
    }

    #[instrument]
    fn move_left(&mut self) -> cursor::MovementResult {
        cursor::MovementResult::HitStart
    }

    #[instrument]
    fn move_right(&mut self) -> cursor::MovementResult {
        cursor::MovementResult::HitEnd
    }

    fn move_left_large(&mut self) -> cursor::MovementResult {
        cursor::MovementResult::HitStart
    }

    fn move_right_large(&mut self) -> cursor::MovementResult {
        cursor::MovementResult::HitEnd
    }

    /*
    fn enter_standard(&mut self, _document_host: &document::DocumentHost, _insert: bool, _key: &cursor::key::Key) -> Result<cursor::MovementResult, cursor::EntryError> {
        Err(cursor::EntryError::KeyNotRecognized)
    }

    fn enter_utf8(&mut self, _document_host: &document::DocumentHost, _insert: bool, _key: &cursor::key::Key) -> Result<cursor::MovementResult, cursor::EntryError> {
        Err(cursor::EntryError::KeyNotRecognized)
}
    */
}
