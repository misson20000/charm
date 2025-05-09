use std::task;

use crate::model::addr;
use crate::model::datapath;
use crate::model::document;
use crate::model::listing::cursor;
use crate::model::listing::line;
use crate::model::listing::token;
use crate::model::listing::token::AsTokenRef;

#[derive(Debug)]
pub struct Cursor {
    pub token: token::Bindump,

    /* Offset within token */
    offset: addr::Offset,

    data: Option<datapath::Fetcher>,
}

impl Cursor {
    pub fn new_transition(token: token::Bindump, hint: &cursor::TransitionHint) -> Result<Cursor, token::Token> {
        let extent = token.extent;

        let offset = match hint {
            cursor::TransitionHint::MoveVertical {
                horizontal_position: cursor::HorizontalPosition::Bindump(offset_in_line),
                ..
            } => {
                let offset = token.line.begin + *offset_in_line;
                if offset >= extent.end {
                    return Err(token::Token::Bindump(token));
                } else if offset < extent.begin {
                    /* the first line after a child can have a first token that
                     * starts in the middle of the line, and the horizontal
                     * position can point before that token starts. */
                    addr::Offset::ZERO
                } else {
                    offset - extent.begin
                }
            },
            
            op if op.is_left() => token.word_at(token.extent.end - addr::Offset::BIT).begin - token.extent.begin,
            op if op.is_right() => token.word_at(token.extent.begin).end - addr::Offset::BIT - token.extent.begin,
            _ => addr::Offset::ZERO
        };
        
        Ok(Cursor {
            token,
            offset,

            data: None,
        })
    }
    
    pub fn new_placement(token: token::Bindump, offset: addr::Offset, _hint: &cursor::PlacementHint) -> Result<Cursor, token::Token> {
        let extent = token.extent;

        Ok(Cursor {
            token,
            offset: match offset {
                offset if offset < extent.begin => addr::Offset::ZERO,
                offset if offset >= extent.end => extent.len() - addr::Offset::BIT,
                offset => offset - extent.begin,
            },

            data: None,
        })
    }

    pub fn extent(&self) -> addr::Extent {
        self.token.extent
    }

    fn word_size(&self) -> addr::Offset {
        self.token.word_size()
    }

    /* Node-relative */
    pub fn current_word(&self) -> addr::Extent {
        self.token.word_at(self.token.extent.begin + self.offset)
    }
}

impl cursor::CursorClassExt for Cursor {
    fn get_offset(&self) -> addr::Offset {
        self.offset
    }

    fn get_token(&self) -> token::TokenRef<'_> {
        self.token.as_token_ref()
    }
    
    fn get_placement_hint(&self) -> cursor::PlacementHint {
        cursor::PlacementHint::Bindump(BindumpPlacementHint {
        })
    }
    
    fn get_horizontal_position_in_line(&self, line: &line::Line) -> cursor::HorizontalPosition {
        if let line::LineType::Bindump { line_extent, .. } = &line.ty {
            cursor::HorizontalPosition::Bindump(self.extent().begin + self.offset - line_extent.begin)
        } else {
            panic!("attempted to get horizontal position of BindumpCursor on a non-Bindump line");
        }
    }
    
    fn move_left(&mut self) -> cursor::MovementResult {
        let current_word = self.current_word();
        if self.token.extent.begin + self.offset + addr::Offset::BIT >= current_word.end {
            /* On MSB. Try to move to LSB of previous word. */
            if current_word.begin >= self.token.extent.begin + self.word_size() {
                self.offset = current_word.begin - self.word_size() - self.token.extent.begin;
                cursor::MovementResult::Ok
            } else {
                /* We're in the first word. */
                // TODO: This is messed up for cases like this:
                //  0.                1.
                //   7 6 5 4 3 2 1 0   7 6 5 4 3 2 1 0
                //  +---------+-----+ +-----------+---+
                //  |    B    |  A  | |     C     | B |
                //  +---------+-----+ +-----------+---+
                //  The tokens will be emitted in A, B, C order,
                //  but if we're on 1.7 and move left, we need to skip over B.
                cursor::MovementResult::HitStart
            }
        } else {
            self.offset+= addr::Offset::BIT;
            cursor::MovementResult::Ok
        }
    }

    fn move_right(&mut self) -> cursor::MovementResult {
        let current_word = self.current_word();
        if self.token.extent.begin + self.offset == current_word.begin {
            if current_word.end < self.token.extent.end {
                /* There is another word to move to. Move to the MSB of it. */
                self.offset = std::cmp::min(self.token.extent.end, current_word.end + self.word_size()) - addr::Offset::BIT - self.token.extent.begin;
                cursor::MovementResult::Ok
            } else {
                cursor::MovementResult::HitEnd
            }
        } else {
            self.offset-= addr::Offset::BIT;
            cursor::MovementResult::Ok
        }
    }

    fn move_left_large(&mut self) -> cursor::MovementResult {
        let current_word = self.current_word();
        if self.token.extent.begin + self.offset + addr::Offset::BIT >= current_word.end {
            /* On MSB. Try to move to LSB of previous word. */
            if current_word.begin >= self.token.extent.begin + self.word_size() {
                self.offset = current_word.begin - self.word_size() - self.token.extent.begin;
                cursor::MovementResult::Ok
            } else {
                /* We're in the first word. */
                // TODO: This is messed up for cases like this:
                //  0.                1.
                //   7 6 5 4 3 2 1 0   7 6 5 4 3 2 1 0
                //  +---------+-----+ +-----------+---+
                //  |    B    |  A  | |     C     | B |
                //  +---------+-----+ +-----------+---+
                //  The tokens will be emitted in A, B, C order,
                //  but if we're on 1.7 and move left, we need to skip over B.
                cursor::MovementResult::HitStart
            }
        } else {
            /* Move to MSB. */
            self.offset = current_word.end - addr::Offset::BIT - self.token.extent.begin;
            cursor::MovementResult::Ok
        }
    }

    fn move_right_large(&mut self) -> cursor::MovementResult {
        let current_word = self.current_word();
        if self.token.extent.begin + self.offset == current_word.begin {
            /* On LSB. */
            if current_word.end < self.token.extent.end {
                /* There is another word to move to. Move to the MSB of it. */
                self.offset = std::cmp::min(self.token.extent.end, current_word.end + self.word_size()) - addr::Offset::BIT - self.token.extent.begin;
                cursor::MovementResult::Ok
            } else {
                cursor::MovementResult::HitEnd
            }
        } else {
            /* Move to LSB. */
            self.offset = current_word.begin - self.token.extent.begin;
            cursor::MovementResult::Ok
        }
    }

    fn enter_hex(&mut self, _document_host: &document::DocumentHost, _document: &document::Document, _nybble: u8) -> Result<cursor::MovementResult, cursor::EntryError> {
        Err(cursor::EntryError::InvalidForType)
    }

    fn invalidate_data(&mut self) {
        self.data = None;
    }
    
    fn work(&mut self, document: &document::Document, cx: &mut task::Context) -> (bool, bool) {
        let mut fetcher = match self.data.take() {
            Some(fetcher) => fetcher,
            None => {
                let (begin_byte, size) = self.token.absolute_extent().round_out();
                datapath::Fetcher::new(document.datapath.clone(), begin_byte, size as usize)
            }
        };

        let did_work = fetcher.work(cx);
        let done = fetcher.finished();
        self.data = Some(fetcher);

        (did_work, done)
    }
}

#[derive(Debug, Clone)]
pub struct BindumpPlacementHint {
}

#[derive(Debug, Clone)]
pub struct BindumpTransitionHint {
}
