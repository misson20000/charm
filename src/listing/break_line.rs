use std::sync;

use crate::addr;
use crate::space::edit;

pub struct BreakLine {
    pub address: addr::Address,
    pub brk: sync::Arc<edit::Break>,
    pub i: usize,
}

impl BreakLine {
    pub fn num_lines(&self) -> usize {
        self.brk.num_lines()
    }
}
