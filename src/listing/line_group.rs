use std::task;

use crate::listing;

pub enum LineGroup {
    Hex(listing::brk::hex::HexLineGroup),
    BreakHeader(listing::brk::BreakHeaderLineGroup)
}

#[derive(Hash, PartialEq, Eq)]
pub enum CacheId {
    Hex(u64),
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

    pub fn get_cache_id(&self) -> Option<CacheId> {
        match self {
            LineGroup::Hex(hex) => hex.get_cache_id().map(|cid| CacheId::Hex(cid)),
            LineGroup::BreakHeader(_) => None,
        }
    }
}
