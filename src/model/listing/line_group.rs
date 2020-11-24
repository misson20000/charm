use std::task;

use crate::model::document;

#[derive(PartialEq, Eq, Debug)]
pub enum LineGroup {
    Hex(document::brk::hex::HexLineGroup),
    BreakHeader(document::brk::BreakHeaderLineGroup)
}

#[derive(Hash, PartialEq, Eq, Debug)]
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

    pub fn update(&mut self, document: &document::Document, cx: &mut task::Context) -> bool {
        match self {
            LineGroup::Hex(hex) => hex.update(document, cx),
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
