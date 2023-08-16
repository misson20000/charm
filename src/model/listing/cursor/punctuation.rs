use crate::model::addr;
use crate::model::listing::cursor;
use crate::model::listing::token;

#[derive(Debug)]
pub struct Cursor {
    token: token::Token,
}

impl Cursor {
    pub fn new_transition(token: token::Token, hint: &cursor::TransitionHint) -> Result<Cursor, token::Token> {
        if hint.op.is_entry() {
            /* skip over punctuation tokens for entry */
            Err(token)
        } else {
            Ok(Cursor {
                token
            })
        }
    }
    
    pub fn new_placement(token: token::Token, _offset: addr::Address, hint: &cursor::PlacementHint) -> Result<Cursor, token::Token> {
        match hint {
            /* we only place the cursor on punctuation if explicitly requested;
             * otherwise, we prefer to place it on a content token. */
            cursor::PlacementHint::Punctuation => Ok(Cursor {
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
        cursor::PlacementHint::Punctuation
    }
    
    fn get_transition_hint(&self) -> cursor::TransitionHintClass {
        cursor::TransitionHintClass::Unused
    }

    fn move_left(&mut self) -> cursor::MovementResult {
        cursor::MovementResult::HitStart
    }

    fn move_right(&mut self) -> cursor::MovementResult {
        cursor::MovementResult::HitEnd
    }

    fn move_left_large(&mut self) -> cursor::MovementResult {
        cursor::MovementResult::HitStart
    }

    fn move_right_large(&mut self) -> cursor::MovementResult {
        cursor::MovementResult::HitEnd
    }
}