use crate::model::listing::cursor;
use crate::model::listing::token;
use crate::model::listing::token::AsTokenRef;

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
    fn get_token(&self) -> token::TokenRef<'_> {
        self.token.as_token_ref()
    }
    
    fn get_placement_hint(&self) -> cursor::PlacementHint {
        cursor::PlacementHint::Punctuation
    }
}
