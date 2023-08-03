use crate::model::addr;
use crate::model::listing::cursor;
use crate::model::listing::token;

#[derive(Debug)]
pub struct Cursor {
    pub token: token::Token,
    pub extent: addr::Extent,
    pub offset: addr::Size,
    pub low_nybble: bool,
}

trait TokenExt {
    fn hexdump_extent(&self) -> addr::Extent;
    fn hexdump_absolute_extent(&self) -> addr::Extent;
}

impl TokenExt for token::Token {
    fn hexdump_extent(&self) -> addr::Extent {
        match self.class {
            token::TokenClass::Hexdump(e) => e,
            _ => panic!("expected hexdump token")
        }
    }

    fn hexdump_absolute_extent(&self) -> addr::Extent {
        self.hexdump_extent().rebase(self.node_addr)
    }
}

impl Cursor {
    pub fn new_transition(token: token::Token, hint: &cursor::TransitionHint) -> Result<Cursor, token::Token> {
        let extent = token.hexdump_extent();
        let limit = (extent.length() - addr::unit::BIT).floor();
        
        Ok(Cursor {
            token,
            extent,
            offset: match hint.op {
                cursor::TransitionOp::MoveLeftLarge => addr::Size::from(limit.bytes & !7),
                op if op.is_left() => limit,
                op if op.is_right() => addr::unit::ZERO,
                _ => match &hint.class {
                    cursor::TransitionHintClass::Hexdump(hth) => std::cmp::min(hth.offset, limit),
                    _ => addr::unit::ZERO,
                },
            },
            low_nybble: match (&hint.class, hint.op) {
                /* decide from op */
                (_, cursor::TransitionOp::MoveLeftLarge) => false,
                (_, op) if op.is_left() => true,
                (_, op) if op.is_right() => false,
                /* if we have an intended offset and had to truncate it, we should place at the end of the line */
                (cursor::TransitionHintClass::Hexdump(hth), _) if hth.offset > limit => true,
                /* if we have an intended offset and didn't have to truncate it, try to carry the low_nybble flag over from a previous HexCursor */
                (cursor::TransitionHintClass::Hexdump(hth), _) => hth.low_nybble,
                /* last resort, if the op is seriously misbehaving and is neither left nor right */
                _ => false,
            },
        })
    }
    
    pub fn new_placement(token: token::Token, offset: addr::Address, hint: &cursor::PlacementHint) -> Result<Cursor, token::Token> {
        let extent = token.hexdump_extent();
        let limit = (extent.length() - addr::unit::BIT).floor();
        
        Ok(Cursor {
            token,
            extent,
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
}

impl cursor::CursorClassExt for Cursor {
    fn is_over(&self, token: &token::Token) -> bool {
        &self.token == token
    }

    fn get_addr(&self) -> addr::Address {
        self.token.hexdump_absolute_extent().begin + self.offset
    }

    fn get_offset(&self) -> addr::Size {
        self.offset
    }

    fn get_token(&self) -> &token::Token {
        &self.token
    }
    
    fn get_placement_hint(&self) -> cursor::PlacementHint {
        cursor::PlacementHint::Hexdump(HexdumpPlacementHint {
            offset: self.offset,
            low_nybble: self.low_nybble
        })
    }
    
    fn get_transition_hint(&self) -> cursor::TransitionHintClass {
        cursor::TransitionHintClass::Hexdump(HexdumpTransitionHint {
            offset: self.offset,
            low_nybble: self.low_nybble
        })
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
            if offset >= self.extent.length() {
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
        let length = self.extent.length();

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

#[derive(Debug)]
pub struct HexdumpPlacementHint {
    offset: addr::Size,
    low_nybble: bool,
}

#[derive(Debug, Clone)]
pub struct HexdumpTransitionHint {
    offset: addr::Size,
    low_nybble: bool,
}
