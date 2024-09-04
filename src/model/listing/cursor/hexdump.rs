use crate::model::addr;
use crate::model::listing::cursor;
use crate::model::listing::line;
use crate::model::listing::token;
use crate::model::listing::token::TokenKind;

#[derive(Debug)]
pub struct Cursor {
    pub token: token::HexdumpToken,
    pub offset: addr::Size,
    pub low_nybble: bool,
}

impl Cursor {
    pub fn new_transition(token: token::HexdumpToken, hint: &cursor::TransitionHint) -> Result<Cursor, token::Token> {
        let extent = token.extent;
        let limit = (extent.length() - addr::unit::BIT).floor();

        let (offset, low_nybble) = match hint {
            cursor::TransitionHint::MoveLeftLarge => (addr::Size::from(limit.bytes & !7), false),
            
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
            
            op if op.is_left() => (limit, true),
            op if op.is_right() => (addr::unit::ZERO, false),
            _ => (addr::unit::ZERO, false)
        };
        
        Ok(Cursor {
            token,
            offset,
            low_nybble,
        })
    }
    
    pub fn new_placement(token: token::HexdumpToken, offset: addr::Address, hint: &cursor::PlacementHint) -> Result<Cursor, token::Token> {
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
                _ => false,
            },
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
        cursor::PlacementHint::Hexdump(HexdumpPlacementHint {
            low_nybble: self.low_nybble
        })
    }
    
    fn get_horizontal_position_in_line(&self, line: &line::Line) -> cursor::HorizontalPosition {
        if let line::LineType::Hexdump { line_extent, .. } = &line.ty {
            cursor::HorizontalPosition::Hexdump(self.extent().begin.to_size() + self.offset - line_extent.begin.to_size(), self.low_nybble)
        } else {
            panic!("attempted to get horizontal position of HexdumpCursor on a non-Hexdump line");
        }
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

    /*
    fn enter_standard(&mut self, document_host: &document::DocumentHost, insert: bool, key: &cursor::key::Key) -> Result<cursor::MovementResult, cursor::EntryError> {
        let nybble = match key {
            cursor::key::Key::_0 => 0,
            cursor::key::Key::_1 => 1,
            cursor::key::Key::_2 => 2,
            cursor::key::Key::_3 => 3,
            cursor::key::Key::_4 => 4,
            cursor::key::Key::_5 => 5,
            cursor::key::Key::_6 => 6,
            cursor::key::Key::_7 => 7,
            cursor::key::Key::_8 => 8,
            cursor::key::Key::_9 => 9,
            cursor::key::Key::a => 0xa,
            cursor::key::Key::b => 0xb,
            cursor::key::Key::c => 0xc,
            cursor::key::Key::d => 0xd,
            cursor::key::Key::e => 0xe,
            cursor::key::Key::f => 0xf,
            _ => return Err(cursor::EntryError::KeyNotRecognized)
        };

        let i = self.offset.bytes as usize;
        let extent = self.lg.as_hex_line().extent;
        let loc = extent.begin.byte + self.offset.bytes;
        let shift = extent.begin.bit;

        /*
         * +-----------------+-----------------+
         * | 0 1 2 3 4 5 6 7 | 0 1 2 3 4 5 6 7 |
         * +-----------------+-----------------+
         * 
         */

        if self.low_nybble && shift <= 4 {
            let raw = self.lg.as_hex_line().bytes[i].get_loaded().ok_or(cursor::EntryError::DataNotLoaded)?;
            let mask = 0xF << shift;
            
            document_host.patch_byte(loc, (raw & !mask) | (nybble << shift));
        } else if !self.low_nybble && shift == 0 {
            let raw = self.lg.as_hex_line().bytes[i].get_loaded().ok_or(cursor::EntryError::DataNotLoaded)?;
            let mask = 0xF << 4;
            
            if insert {
                document_host.insert_byte(loc, (raw & !mask) | (nybble << 4));
            } else {
                document_host.patch_byte(loc, (raw & !mask) | (nybble << 4));
            }
        } else {
            todo!(); // TODO
        }

        Ok(self.move_right())
    }

    fn enter_utf8(&mut self, _document_host: &document::DocumentHost, _insert: bool, _key: &cursor::key::Key) -> Result<cursor::MovementResult, cursor::EntryError> {
        Err(cursor::EntryError::KeyNotRecognized) // TODO
}
    */
}

#[derive(Debug, Clone)]
pub struct HexdumpPlacementHint {
    pub low_nybble: bool,
}

#[derive(Debug, Clone)]
pub struct HexdumpTransitionHint {
    offset: addr::Size,
    low_nybble: bool,
}
