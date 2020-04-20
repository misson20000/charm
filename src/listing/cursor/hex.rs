use crate::addr;
use crate::listing::brk;
use crate::listing::cursor;
use crate::listing::line_group;

#[derive(Debug)]
pub struct HexCursor {
    lg: line_group::LineGroup,
    pub offset: addr::Size,
    intended_offset: Option<addr::Size>,
    intended_nybble: Option<bool>,
    pub low_nybble: bool,
}

trait LineGroupExt {
    fn as_hex_line(&self) -> &brk::hex::HexLineGroup;
}

impl LineGroupExt for line_group::LineGroup {
    fn as_hex_line(&self) -> &brk::hex::HexLineGroup {
        match self {
            line_group::LineGroup::Hex(hlg) => hlg,
            _ => panic!("expected hex line group"),
        }
    }
}

impl HexCursor {
    pub fn new_transition(lg: line_group::LineGroup, hint: &cursor::TransitionHint) -> Result<HexCursor, line_group::LineGroup> {
        let hlg = lg.as_hex_line();
        let max = (hlg.extent.length() - addr::unit::BIT).floor();

        let intended_nybble = match &hint.class {
            cursor::TransitionHintClass::Hex(hth) => Some(hth.low_nybble),
            _ => None,
        };
        
        Ok(HexCursor {
            offset: match hint.intended_offset {
                Some(io) if io > max => max,
                Some(io) => io,
                None => match hint.op {
                    cursor::TransitionOp::MoveLeftLarge => addr::Size::from(max.bytes & !7),
                    op if op.is_left() => max,
                    op if op.is_right() => addr::unit::ZERO,
                    _ => addr::unit::ZERO,
                },
            },
            low_nybble: match (hint.intended_offset, intended_nybble, hint.op) {
                /* if we have an intended offset and had to truncate it, we should place at the end of the line */
                (Some(io), _, _) if io > max => true,
                /* if we have an intended offset and didn't have to truncate it, try to carry the low_nybble flag over from a previous HexCursor */
                (Some(_), Some(inb), _) => inb,
                /* decide from op */
                (_, _, cursor::TransitionOp::MoveLeftLarge) => false,
                (_, _, op) if op.is_left() => true,
                (_, _, op) if op.is_right() => false,
                /* last resort, if the op is seriously misbehaving and is neither left nor right */
                _ => false,
            },
            intended_offset: hint.intended_offset,
            intended_nybble: intended_nybble,
            lg,
        })
    }
    
    pub fn new_placement(lg: line_group::LineGroup, hint: &cursor::PlacementHint) -> Result<HexCursor, line_group::LineGroup> {
        let hlg = lg.as_hex_line();
        let max = (hlg.extent.length() - addr::unit::BIT).floor();
        
        Ok(HexCursor {
            offset: match hint.addr {
                addr if addr < hlg.extent.begin => addr::unit::ZERO,
                addr if addr >= hlg.extent.begin + max => max,
                addr => addr - hlg.extent.begin,
            },
            low_nybble: match &hint.class {
                cursor::PlacementHintClass::Hex(hph) => hph.low_nybble,
                _ => false,
            },
            intended_offset: hint.intended_offset,
            intended_nybble: None,
            lg
        })
    }
}

impl cursor::CursorClassExt for HexCursor {
    fn get_line_group(&self) -> &line_group::LineGroup {
        &self.lg
    }

    fn take_line_group(self) -> line_group::LineGroup {
        self.lg
    }
    
    fn get_addr(&self) -> addr::Address {
        self.lg.as_hex_line().extent.begin + self.offset
    }

    fn get_intended_offset(&self) -> Option<addr::Size> {
        self.intended_offset
    }
    
    fn get_placement_hint(&self) -> cursor::PlacementHintClass {
        cursor::PlacementHintClass::Hex(HexPlacementHint {
            low_nybble: self.intended_nybble.unwrap_or(self.low_nybble)
        })
    }

    fn get_transition_hint(&self) -> cursor::TransitionHintClass {
        cursor::TransitionHintClass::Hex(HexTransitionHint {
            low_nybble: self.intended_nybble.unwrap_or(self.low_nybble)
        })
    }

    fn move_left(&mut self) -> cursor::MovementResult {
        self.intended_offset = None;
        self.intended_nybble = None;
        
        if self.low_nybble {
            self.low_nybble = false;
            cursor::MovementResult::Ok
        } else {
            if self.offset >= addr::unit::BYTE {
                self.offset-= addr::unit::BYTE;
                self.low_nybble = true;
                cursor::MovementResult::Ok
            } else {
                cursor::MovementResult::HitStart
            }
        }
    }

    fn move_right(&mut self) -> cursor::MovementResult {
        self.intended_offset = None;
        self.intended_nybble = None;
        
        if self.low_nybble {
            let offset = self.offset + addr::unit::BYTE;
            if offset >= self.lg.as_hex_line().extent.length() {
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

    fn move_up(&mut self) -> cursor::MovementResult {
        self.intended_offset = self.intended_offset.or(Some(self.offset));
        
        cursor::MovementResult::HitStart
    }

    fn move_down(&mut self) -> cursor::MovementResult {
        self.intended_offset = self.intended_offset.or(Some(self.offset));
        
        cursor::MovementResult::HitEnd
    }

    fn move_to_start_of_line(&mut self) -> cursor::MovementResult {
        self.intended_offset = None;
        self.intended_nybble = None;
        
        if self.offset == addr::unit::ZERO && !self.low_nybble {
            cursor::MovementResult::NotApplicableForPosition
        } else {
            self.offset = addr::unit::ZERO;
            self.low_nybble = false;
            cursor::MovementResult::Ok
        }
    }

    fn move_to_end_of_line(&mut self) -> cursor::MovementResult {
        self.intended_offset = None;
        self.intended_nybble = None;

        let max = (self.lg.as_hex_line().extent.length() - addr::unit::BIT).floor();
        if self.offset >= max && self.low_nybble {
            self.offset = max; // silently repair some invalid state if it happens
            cursor::MovementResult::NotApplicableForPosition
        } else {
            self.offset = max;
            self.low_nybble = true;
            cursor::MovementResult::Ok
        }
    }

    fn move_left_large(&mut self) -> cursor::MovementResult {
        self.intended_offset = None;
        self.intended_nybble = None;

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
        self.intended_offset = None;
        self.intended_nybble = None;

        let offset = addr::Size::from(self.offset.bytes & !7);
        let length = self.lg.as_hex_line().extent.length();

        if offset + addr::unit::QWORD >= length {
            cursor::MovementResult::HitEnd
        } else {
            self.low_nybble = false;
            self.offset = offset + addr::unit::QWORD;
            cursor::MovementResult::Ok
        }
    }
}

#[derive(Debug)]
pub struct HexPlacementHint {
    low_nybble: bool,
}

#[derive(Debug)]
pub struct HexTransitionHint {
    low_nybble: bool,
}
