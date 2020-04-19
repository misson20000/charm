use std::sync;
use std::string;

use crate::addr;

pub mod hex;

pub struct Break {
    pub addr: addr::Address,
    pub label: Option<string::String>,
    pub class: BreakClass
}

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

impl Eq for Break {}
