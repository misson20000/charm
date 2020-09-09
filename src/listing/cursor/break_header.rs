use crate::addr;
use crate::listing;
use crate::listing::brk;
use crate::listing::cursor;
use crate::listing::line_group;

#[derive(Debug)]
pub struct BreakHeaderCursor {
    lg: line_group::LineGroup,
    transition_hint: Option<cursor::TransitionHint>,
}

trait LineGroupExt {
    fn as_break_header_line(&self) -> &brk::BreakHeaderLineGroup;
}

impl LineGroupExt for line_group::LineGroup {
    fn as_break_header_line(&self) -> &brk::BreakHeaderLineGroup {
        match self {
            line_group::LineGroup::BreakHeader(bhlg) => bhlg,
            _ => panic!("expected break header line group"),
        }
    }
}

impl BreakHeaderCursor {
    pub fn new_transition(lg: line_group::LineGroup, hint: &cursor::TransitionHint) -> Result<BreakHeaderCursor, line_group::LineGroup> {
        if !hint.op.is_entry() {
            Ok(BreakHeaderCursor {
                lg,
                transition_hint: Some(hint.clone()),
            })
        } else {
            // when entering bytes, we should skip over break headers
            Err(lg)
        }
    }
    
    pub fn new_placement(lg: line_group::LineGroup, hint: &cursor::PlacementHint) -> Result<BreakHeaderCursor, line_group::LineGroup> {
        match hint.class {
            // we only place the cursor on a break header if explicitly requested;
            // otherwise, we prefer to place it on a content line.
            cursor::PlacementHintClass::BreakHeader => Ok(BreakHeaderCursor {
                lg,
                transition_hint: None
            }),
            _ => Err(lg)
        }
    }
}

impl cursor::CursorClassExt for BreakHeaderCursor {
    fn get_line_group(&self) -> &line_group::LineGroup {
        &self.lg
    }

    fn get_line_group_mut(&mut self) -> &mut line_group::LineGroup {
        &mut self.lg
    }
    
    fn take_line_group(self) -> line_group::LineGroup {
        self.lg
    }
    
    fn get_addr(&self) -> addr::Address {
        self.lg.as_break_header_line().brk.addr
    }

    fn get_intended_offset(&self) -> Option<addr::Size> {
        self.transition_hint.as_ref().and_then(|th| th.intended_offset)
    }
    
    fn get_placement_hint(&self) -> cursor::PlacementHintClass {
        cursor::PlacementHintClass::BreakHeader
    }
    
    fn get_transition_hint(&self) -> cursor::TransitionHintClass {
        self.transition_hint.as_ref().map(|th| th.class.clone()).unwrap_or(cursor::TransitionHintClass::Unused)
    }

    fn move_left(&mut self) -> cursor::MovementResult {
        cursor::MovementResult::HitStart
    }

    fn move_right(&mut self) -> cursor::MovementResult {
        cursor::MovementResult::HitEnd
    }

    fn move_up(&mut self) -> cursor::MovementResult {
        cursor::MovementResult::HitStart
    }

    fn move_down(&mut self) -> cursor::MovementResult {
        cursor::MovementResult::HitEnd
    }

    fn move_to_start_of_line(&mut self) -> cursor::MovementResult {
        cursor::MovementResult::NotApplicableForPosition
    }

    fn move_to_end_of_line(&mut self) -> cursor::MovementResult {
        cursor::MovementResult::NotApplicableForPosition
    }

    fn move_left_large(&mut self) -> cursor::MovementResult {
        cursor::MovementResult::HitStart
    }

    fn move_right_large(&mut self) -> cursor::MovementResult {
        cursor::MovementResult::HitEnd
    }

    fn enter_standard(&mut self, _listing: &listing::ListingWatch, _key: &gdk::EventKey) -> Result<cursor::MovementResult, cursor::EntryError> {
        Err(cursor::EntryError::KeyNotRecognized)
    }

    fn enter_utf8(&mut self, _listing: &listing::ListingWatch, _key: &gdk::EventKey) -> Result<cursor::MovementResult, cursor::EntryError> {
        Err(cursor::EntryError::KeyNotRecognized)
    }
}
