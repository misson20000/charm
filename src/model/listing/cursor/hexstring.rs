use std::task;

use crate::model::addr;
use crate::model::datapath;
use crate::model::document;
use crate::model::listing::cursor;
use crate::model::listing::line;
use crate::model::listing::token;
use crate::model::listing::token::TokenKind;

#[derive(Debug)]
pub struct Cursor {
    pub token: token::HexstringToken,
    pub offset: addr::Size,
    pub low_nybble: bool,

    data: Option<datapath::Fetcher>,
}

impl Cursor {
    pub fn new_transition(token: token::HexstringToken, hint: &cursor::TransitionHint) -> Result<Cursor, token::Token> {
        let extent = token.extent;
        let limit = (extent.length() - addr::unit::BIT).floor();

        let (offset, low_nybble) = match hint {
            cursor::TransitionHint::MoveLeftLarge => (addr::Size::from(limit.bytes & !7), false),

            /*
            cursor::TransitionHint::MoveVertical {
                horizontal_position: cursor::HorizontalPosition::Hexdump(offset_in_line, low_nybble),
                line,
                ..
            } => {
                let line_extent = match line.ty {
                    line::LineType::Hexdump { line_extent, .. } => line_extent,
                    _ => return Err(token::Token::Hexdump(token)),
                };
                let offset = line_extent.begin + *offset_in_line;
                if offset >= extent.end {
                    return Err(token::Token::Hexdump(token));
                } else if offset < extent.begin {
                    /* the first line after a child can have a first token that
                     * starts in the middle of the line, and the horizontal
                     * position can point before that token starts. */
                    (addr::unit::ZERO, false)
                } else {
                    (offset - extent.begin, *low_nybble)
                }
        },
            */
            
            op if op.is_left() => (limit, true),
            op if op.is_right() => (addr::unit::ZERO, false),
            _ => (addr::unit::ZERO, false)
        };
        
        Ok(Cursor {
            token,
            offset,
            low_nybble,

            data: None,
        })
    }
    
    pub fn new_placement(token: token::HexstringToken, offset: addr::Address, hint: &cursor::PlacementHint) -> Result<Cursor, token::Token> {
        let extent = token.extent;
        let limit = (extent.length() - addr::unit::BIT).floor();

        Ok(Cursor {
            token,
            offset: match offset {
                offset if offset < extent.begin => addr::unit::ZERO,
                offset if offset >= extent.begin + limit => limit,
                offset => offset - extent.begin,
            },
            low_nybble: match &hint {
                cursor::PlacementHint::Hexdump(hph) => hph.low_nybble,
                cursor::PlacementHint::Hexstring(hph) => hph.low_nybble,
                _ => false,
            },

            data: None,
        })
    }

    pub fn extent(&self) -> addr::Extent {
        self.token.extent
    }
}

impl cursor::CursorClassExt for Cursor {
    fn is_over(&self, token: token::TokenRef<'_>) -> bool {
        self.token.as_ref() == token
    }

    fn get_addr(&self) -> addr::Address {
        self.token.absolute_extent().begin + self.offset
    }

    fn get_offset(&self) -> addr::Size {
        self.offset
    }

    fn get_token(&self) -> token::TokenRef<'_> {
        self.token.as_ref()
    }
    
    fn get_placement_hint(&self) -> cursor::PlacementHint {
        cursor::PlacementHint::Hexstring(HexstringPlacementHint {
            low_nybble: self.low_nybble
        })
    }
    
    fn get_horizontal_position_in_line(&self, _line: &line::Line) -> cursor::HorizontalPosition {
        cursor::HorizontalPosition::Unspecified
    }
    
    fn move_left(&mut self) -> cursor::MovementResult {
        if self.low_nybble {
            self.low_nybble = false;
            cursor::MovementResult::Ok
        } else if self.offset >= addr::unit::BYTE {
            self.offset-= addr::unit::BYTE;
            self.low_nybble = true;
            cursor::MovementResult::Ok
        } else {
            cursor::MovementResult::HitStart
        }
    }

    fn move_right(&mut self) -> cursor::MovementResult {
        if self.low_nybble {
            let offset = self.offset + addr::unit::BYTE;
            if offset >= self.extent().length() {
                cursor::MovementResult::HitEnd
            } else {
                self.offset = offset;
                self.low_nybble = false;
                cursor::MovementResult::Ok
            }
        } else {
            self.low_nybble = true;
            cursor::MovementResult::Ok
        }
    }

    fn move_left_large(&mut self) -> cursor::MovementResult {
        if self.offset == addr::unit::ZERO && !self.low_nybble {
            cursor::MovementResult::HitStart
        } else if self.low_nybble {
            self.offset = addr::Size::from(self.offset.bytes & !7);
            self.low_nybble = false;
            cursor::MovementResult::Ok
        } else {
            self.offset-= addr::unit::BIT;
            self.offset = addr::Size::from(self.offset.bytes & !7);
            cursor::MovementResult::Ok
        }
    }

    fn move_right_large(&mut self) -> cursor::MovementResult {
        let offset = addr::Size::from(self.offset.bytes & !7);
        let length = self.extent().length();

        if offset + addr::unit::QWORD >= length {
            cursor::MovementResult::HitEnd
        } else {
            self.low_nybble = false;
            self.offset = offset + addr::unit::QWORD;
            cursor::MovementResult::Ok
        }
    }

    fn enter_hex(&mut self, document_host: &document::DocumentHost, document: &document::Document, nybble: u8) -> Result<cursor::MovementResult, cursor::EntryError> {
        let i = self.offset.bytes as usize;
        let loc = self.token.absolute_extent().begin.byte + self.offset.bytes;
        let shift = self.token.absolute_extent().begin.bit;
        let insert = false; // TODO
        
        /*
         * +-----------------+-----------------+
         * | 0 1 2 3 4 5 6 7 | 0 1 2 3 4 5 6 7 |
         * +-----------------+-----------------+
         * 
         */

        let raw = match self.data.as_ref().and_then(|fetcher| fetcher.byte(i)) {
            Some(b) => b,
            None => return Err(cursor::EntryError::DataPending)
        };
        
        let change = if self.low_nybble && shift <= 4 {
            let mask = 0xF << shift;
            
            document.patch_byte(loc, (raw & !mask) | (nybble << shift))
        } else if !self.low_nybble && shift == 0 {
            let mask = 0xF << 4;
            
            if insert {
                document.insert_byte(loc, (raw & !mask) | (nybble << 4))
            } else {
                document.patch_byte(loc, (raw & !mask) | (nybble << 4))
            }
        } else {
            todo!(); // TODO
        };

        document_host.change(change)?;

        Ok(self.move_right())
    }

    fn invalidate_data(&mut self) {
        self.data = None;
    }
    
    fn work(&mut self, document: &document::Document, cx: &mut task::Context) -> (bool, bool) {
        let mut fetcher = match self.data.take() {
            Some(fetcher) => fetcher,
            None => {
                let (begin_byte, size) = self.token.absolute_extent().round_out();
                datapath::Fetcher::new(&document.datapath, begin_byte, size as usize)
            }
        };

        let did_work = fetcher.work(cx);
        let done = fetcher.finished();
        self.data = Some(fetcher);

        (did_work, done)
    }
}

#[derive(Debug, Clone)]
pub struct HexstringPlacementHint {
    pub low_nybble: bool,
}

#[derive(Debug, Clone)]
pub struct HexstringTransitionHint {
    offset: addr::Size,
    low_nybble: bool,
}
