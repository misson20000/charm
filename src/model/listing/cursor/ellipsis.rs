use crate::model::listing::cursor;
use crate::model::listing::token;
use crate::model::listing::token::AsTokenRef;

#[derive(Debug)]
pub struct Cursor {
    token: token::Ellipsis,
}

impl Cursor {
    pub fn new_transition(token: token::Ellipsis, hint: &cursor::TransitionHint) -> Result<Cursor, token::Ellipsis> {
        if match hint {
            hint if hint.is_entry() => false,
            _ => true,
        } {
            Ok(Cursor { token })
        } else {
            Err(token)
        }
    }
    
    pub fn new_placement(token: token::Ellipsis, _hint: &cursor::PlacementHint) -> Result<Cursor, token::Ellipsis> {
        Ok(Cursor { token })
    }
}

impl cursor::CursorClassExt for Cursor {
    fn get_token(&self) -> token::TokenRef<'_> {
        self.token.as_token_ref()
    }
}
