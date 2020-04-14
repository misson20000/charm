use std::sync;
use std::task;

use crate::space;
use crate::listing;
use crate::addr;

pub enum LineGroup {
    Hex(listing::hex_line::HexLine),
    Break(listing::break_line::BreakLine) // index
}

impl LineGroup {
    pub fn make_break(i: usize, brk: &sync::Arc<space::edit::Break>) -> LineGroup {
        LineGroup::Break(listing::break_line::BreakLine {
            address: brk.address,
            brk: brk.clone(),
            i
        })
    }
    
    pub fn make_line(space: sync::Arc<dyn space::AddressSpace + Send + Sync>, extent: addr::Extent, distance_from_break: u64) -> LineGroup {
        LineGroup::Hex(listing::hex_line::HexLine::new(space, extent, distance_from_break))
    }

    pub fn get_addr(&self) -> addr::Address {
        match self {
            LineGroup::Hex(hex) => hex.extent.addr,
            LineGroup::Break(brk) => brk.address,
        }
    }
    
    pub fn num_lines(&self) -> usize {
        match self {
            LineGroup::Hex(hex) => hex.num_lines(),
            LineGroup::Break(brk) => brk.num_lines(),
        }
    }

    pub fn progress(&self, cx: &mut task::Context) -> bool {
        match self {
            LineGroup::Hex(hex) => hex.progress(cx),
            LineGroup::Break(_) => false,
        }
    }    
}
