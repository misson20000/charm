use crate::model::addr;
use crate::model::listing::cursor;
use crate::model::listing::line;
use crate::model::listing::token;
use crate::model::listing::token::TokenKind;

#[derive(Debug)]
pub struct Cursor {
    token: token::Token,
}

impl Cursor {
    pub fn new_transition(token: token::Token, hint: &cursor::TransitionHint) -> Result<Cursor, token::Token> {
        if match hint {
            hint if hint.is_entry() => false,
            cursor::TransitionHint::MoveVertical { horizontal_position: cursor::HorizontalPosition::Unspecified, .. } => true,
            cursor::TransitionHint::MoveVertical { .. } => false,
            _ => true,
        } {
            Ok(Cursor { token })
        } else {
            Err(token)
        }
    }
    
    pub fn new_placement(token: token::Token, hint: &cursor::PlacementHint) -> Result<Cursor, token::Token> {
        match hint {
            /* we only place the cursor on punctuation if explicitly requested or this is a last-ditch effort;
             * otherwise, we prefer to place it on a content token. */
            cursor::PlacementHint::Punctuation | cursor::PlacementHint::LastDitch => Ok(Cursor {
                token
            }),
            _ => Err(token)
        }
    }
}

impl cursor::CursorClassExt for Cursor {
    fn is_over(&self, token: token::TokenRef<'_>) -> bool {
        self.token.as_ref() == token
    }
    
    fn get_addr(&self) -> addr::Address {
        self.token.common().node_addr
    }

    fn get_offset(&self) -> addr::Size {
        addr::unit::ZERO
    }

    fn get_token(&self) -> token::TokenRef<'_> {
        self.token.as_ref()
    }
    
    fn get_placement_hint(&self) -> cursor::PlacementHint {
        cursor::PlacementHint::Punctuation
    }
    
    fn get_horizontal_position_in_line(&self, _line: &line::Line) -> cursor::HorizontalPosition {
        cursor::HorizontalPosition::Unspecified
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
