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
    offset: addr::Offset,

    data: Option<datapath::Fetcher>,
}

impl Cursor {
    pub fn new_transition(token: token::Bindump, hint: &cursor::TransitionHint) -> Result<Cursor, token::Token> {
        let extent = token.extent;
        let limit = (extent.len() - addr::Offset::BIT).round_down();

        let offset = match hint {
            cursor::TransitionHint::MoveVertical {
                horizontal_position: cursor::HorizontalPosition::Bindump(offset_in_line),
                line,
                ..
            } => {
                let line_extent = match line.ty {
                    line::LineType::Bindump { line_extent, .. } => line_extent,
                    _ => return Err(token::Token::Bindump(token)),
                };
                let offset = line_extent.begin + *offset_in_line;
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
            
            op if op.is_left() => addr::Offset::from(limit.bytes()),
            op if op.is_right() => std::cmp::min(limit, addr::Offset::new(0, 7)),
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
        let limit = (extent.len() - addr::Offset::BIT).round_down();

        Ok(Cursor {
            token,
            offset: match offset {
                offset if offset < extent.begin => addr::Offset::ZERO,
                offset if offset >= extent.begin + limit => limit,
                offset => offset - extent.begin,
            },

            data: None,
        })
    }

    pub fn extent(&self) -> addr::Extent {
        self.token.extent
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
            panic!("attempted to get horizontal position of HexdumpCursor on a non-Hexdump line");
        }
    }
    
    fn move_left(&mut self) -> cursor::MovementResult {
        if self.offset.bits() == 7 || self.offset == (self.extent().len() - addr::Offset::BIT) {
            if self.offset >= addr::Offset::BYTE {
                self.offset = addr::Offset::new(self.offset.bytes() - 1, 0);
                cursor::MovementResult::Ok
            } else {
                cursor::MovementResult::HitStart
            }
        } else {
            self.offset+= addr::Offset::BIT;
            cursor::MovementResult::Ok
        }
    }

    fn move_right(&mut self) -> cursor::MovementResult {
        if self.offset.bits() > 0 {
            self.offset-= addr::Offset::BIT;
            cursor::MovementResult::Ok
        } else if self.extent().len().bits() == 0 && self.offset.bytes() + 1 == self.extent().len().bytes() {
            cursor::MovementResult::HitEnd
        } else if self.offset.bytes() == self.extent().len().bytes() {
            cursor::MovementResult::HitEnd
        } else {
            self.offset = std::cmp::min(addr::Offset::new(self.offset.bytes() + 1, 7), self.extent().len() - addr::Offset::BIT);
            cursor::MovementResult::Ok
        }
    }

    fn move_left_large(&mut self) -> cursor::MovementResult {
        if self.offset.bits() == 7 || (self.offset.bytes() == self.extent().len().bytes() && self.offset.bits() + 1 == self.extent().len().bits()) {
            if self.offset.bytes() > 0 {
                self.offset-= addr::Offset::BYTE;
                cursor::MovementResult::Ok
            } else {
                cursor::MovementResult::HitStart
            }
        } else {
            if self.offset.bytes() == self.extent().len().bytes() {
                self.offset = addr::Offset::new(self.offset.bytes(), self.extent().len().bits() - 1);
            } else {
                self.offset = addr::Offset::new(self.offset.bytes(), 7);
            }
            cursor::MovementResult::Ok
        }
    }

    fn move_right_large(&mut self) -> cursor::MovementResult {
        if self.offset.bits() == 0 {
            if self.offset.bytes() + 1 < self.extent().len().bytes() {
                self.offset+= addr::Offset::BYTE;
                cursor::MovementResult::Ok
            } else {
                cursor::MovementResult::HitEnd
            }
        } else {
            self.offset = addr::Offset::new(self.offset.bytes(), 0);
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
