use crate::model::listing::cursor;
use crate::model::listing::line;
use crate::model::listing::token;
use crate::model::listing::token::AsTokenRef;

#[derive(Debug)]
pub struct Cursor {
    token: token::CommentText,
    pub character: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct CommentPlacementHint {
    character: usize,
}

impl Cursor {
    pub fn new_transition(token: token::CommentText, hint: &cursor::TransitionHint) -> Result<Cursor, token::CommentText> {
        Ok(Cursor {
            character: match hint {
                cursor::TransitionHint::MoveVertical {
                    horizontal_position: cursor::HorizontalPosition::Comment(character),
                    ..
                } => *character,
                hint if hint.is_entry() => return Err(token),
                hint if hint.is_left() => token.text().len()-1,
                _ => 0
            },
            token,
        })
    }
    
    pub fn new_placement(token: token::CommentText, hint: &cursor::PlacementHint) -> Result<Cursor, token::CommentText> {
        Ok(Cursor {
            token,
            character: match hint {
                cursor::PlacementHint::Comment(cph) => cph.character,
                _ => 0,
            }
        })
    }
}

impl cursor::CursorClassExt for Cursor {
    fn get_token(&self) -> token::TokenRef<'_> {
        self.token.as_token_ref()
    }

    fn get_placement_hint(&self) -> cursor::PlacementHint {
        cursor::PlacementHint::Comment(CommentPlacementHint {
            character: self.character
        })
    }

    fn get_horizontal_position_in_line(&self, _line: &line::Line) -> cursor::HorizontalPosition {
        cursor::HorizontalPosition::Comment(self.character)
    }

    // TODO: figure out how to deal with grapheme clusters properly
    /*
    fn move_left(&mut self) -> cursor::MovementResult {
        if self.character == 0 {
            cursor::MovementResult::HitStart
        } else {
            self.character-= 1;
            cursor::MovementResult::Ok
        }
    }

    fn move_left_large(&mut self) -> cursor::MovementResult {
        if self.character == 0 {
            cursor::MovementResult::HitStart
        } else {
            self.character = 0;
            cursor::MovementResult::Ok
        }
    }
    
    fn move_right(&mut self) -> cursor::MovementResult {
        let next = self.character + 1;
        if next >= self.token.text().len() {
            cursor::MovementResult::HitEnd
        } else {
            self.character = next;
            cursor::MovementResult::Ok
        }
    }

    fn move_right_large(&mut self) -> cursor::MovementResult {
        let end = self.token.text().len() - 1;
        if self.character != end {
            self.character = end;
            cursor::MovementResult::Ok
        } else {
            cursor::MovementResult::HitEnd
        }
}
    */
}
