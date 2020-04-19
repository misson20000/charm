use std::task;

use crate::listing;

pub enum LineGroup {
    Hex(listing::brk::hex::HexLineGroup),
    BreakHeader(listing::brk::BreakHeaderLineGroup)
}

impl LineGroup {
    pub fn num_lines(&self) -> usize {
        match self {
            LineGroup::Hex(hex) => hex.num_lines(),
            LineGroup::BreakHeader(bhdr) => bhdr.num_lines(),
        }
    }

    pub fn progress(&mut self, cx: &mut task::Context) -> bool {
        match self {
            LineGroup::Hex(hex) => hex.progress(cx),
            LineGroup::BreakHeader(_) => false,
        }
    }    
}
