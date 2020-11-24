use std::sync;
use std::string;

use crate::model::addr;

pub mod hex;

#[derive(Debug)]
pub struct Break {
    pub addr: addr::Address,
    pub label: Option<string::String>,
    pub class: BreakClass
}

#[derive(Debug)]
pub enum BreakClass {
    Hex(hex::HexBreak),
    //Binary,
    //Structure
}

impl Break {
    pub fn new<T>(addr: T, label: Option<&str>, class: BreakClass) -> Break
    where addr::Address: From<T> {
        Break {
            addr: addr::Address::from(addr),
            label: label.map(|l| l.to_string()),
            class
        }
    }
}

pub struct BreakHeaderLineGroup {
    pub brk: sync::Arc<Break>,
}

impl BreakHeaderLineGroup {
    pub fn new(brk: &sync::Arc<Break>) -> BreakHeaderLineGroup {
        BreakHeaderLineGroup {
            brk: brk.clone(),
        }
    }

    pub fn num_lines(&self) -> usize {
        2
    }
}

impl Ord for Break {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.addr.cmp(&other.addr)
    }
}

impl PartialOrd for Break {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Break {
    fn eq(&self, other: &Self) -> bool {
        self.addr == other.addr
    }
}

impl Eq for Break {
}

impl PartialEq for BreakHeaderLineGroup {
    fn eq(&self, other: &Self) -> bool {
        sync::Arc::ptr_eq(&self.brk, &other.brk)
    }
}

impl Eq for BreakHeaderLineGroup {
}

impl std::fmt::Debug for BreakHeaderLineGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BreakHeaderLineGroup")
            .finish()
    }
}
