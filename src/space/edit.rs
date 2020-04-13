use std::sync;
use std::vec;
use std::string;

use crate::addr;
use crate::space::AddressSpace;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct Break {
    pub address: addr::Address,
    pub label: string::String,
}

impl Break {
    pub fn new<T>(addr: T, label: &str) -> sync::Arc<Break>
    where addr::Address: From<T> {
        sync::Arc::new(Break {
            address: addr::Address::from(addr),
            label: label.to_string()
        })
    }

    pub fn num_lines(&self) -> usize {
        2
    }
}

pub trait BreakListObserver : Send + Sync {
    fn break_list_changed(&self);
}

pub struct SpaceEditor {
    pub space: sync::Arc<dyn AddressSpace + Send + Sync>,
    break_list: sync::RwLock<vec::Vec<sync::Arc<Break>>>,
    break_list_observers: sync::Mutex<vec::Vec<sync::Weak<dyn BreakListObserver>>>,
}

impl SpaceEditor {
    pub fn new(space: sync::Arc<dyn AddressSpace + Send + Sync>) -> sync::Arc<SpaceEditor> {
        sync::Arc::new(SpaceEditor {
            space,
            break_list: sync::RwLock::new(vec![
                Break::new(addr::Address { byte: 0x00000000, bit: 0}, "zero break"),
                Break::new(addr::Address { byte: 0x00000000, bit: 1}, "pathological 1"),
                Break::new(addr::Address { byte: 0x00000000, bit: 2}, "pathological 2"),
                Break::new(addr::Address { byte: 0x00000001, bit: 0}, "pathological 3"),
                Break::new(addr::Address { byte: 0x00000030, bit: 2}, "pathological 4"),
                Break::new(addr::Address { byte: 0x00000100, bit: 0}, "a bit of sanity"),
                Break::new(addr::Address { byte: 0xfffffffffffffff0, bit: 0}, "near the end"),
                Break::new(addr::Address { byte: 0xffffffffffffffff, bit: 0}, "pathological -3"),
                Break::new(addr::Address { byte: 0xffffffffffffffff, bit: 6}, "pathological -2"),
                Break::new(addr::Address { byte: 0xffffffffffffffff, bit: 7}, "pathological -1"),
            ]),
            break_list_observers: sync::Mutex::new(vec::Vec::new()),
        })
    }

    /* I would just make break_list pub, but I want to control write access
     * to enforce invariants like "break list must be sorted" and "break
     * list must notify observers on change" */
    pub fn get_breaks(&self)-> sync::RwLockReadGuard<vec::Vec<sync::Arc<Break>>> {
        self.break_list.read().unwrap()
    }
    
    pub fn add_break_list_observer(&self, observer: &sync::Arc<dyn BreakListObserver>) {
        self.break_list_observers.lock().unwrap().push(sync::Arc::downgrade(observer));
    }
    pub fn remove_break_list_observer(&self, observer: &sync::Weak<dyn BreakListObserver>) {
        // TODO: the pointer comparison here is bogus: https://github.com/rust-lang/rust/issues/46139
        // but it doesn't really matter because we run after Drop for ListingEngineBreakObsever
        self.break_list_observers.lock().unwrap().retain(|e| !e.ptr_eq(observer) && e.upgrade().is_some());
    }

    /// Gets the extents of the break containing the target address.
    pub fn break_extents_at(&self, target: addr::Address) -> addr::InfiniteExtent {
        let breaks = self.get_breaks();
        
        let containing_break_index = match &breaks.binary_search_by(|b| b.address.cmp(&target)) {
            Result::Ok(idx) => *idx,
            Result::Err(idx) if *idx == 0 => panic!("somehow we're above the zero break"),
            Result::Err(idx) => idx-1
        };

        let b = &breaks[containing_break_index];
        match breaks.get(containing_break_index + 1) {
            Some(next_break) => addr::InfiniteExtent::between(b.address, next_break.address),
            None => addr::InfiniteExtent::infinite(b.address)
        }
    }

    /// Gets the extents of the break containing the target address, and if they
    /// exist, the extents of the breaks before and after.
    pub fn break_extents_near(&self, target: addr::Address) -> addr::Triplet<addr::InfiniteExtent> {
        let breaks = self.get_breaks();
        
        let containing_break_index = match &breaks.binary_search_by(|b| b.address.cmp(&target)) {
            Result::Ok(idx) => *idx,
            Result::Err(idx) if *idx == 0 => panic!("somehow we're above the zero break"),
            Result::Err(idx) => idx-1
        };

        let b = &breaks[containing_break_index];

        let before = if containing_break_index == 0 { None } else {
            let bb = &breaks[containing_break_index - 1];
            Some(addr::InfiniteExtent::between(bb.address, b.address))
        };
        
        match breaks.get(containing_break_index + 1) {
            Some(after_break) => addr::Triplet::<addr::InfiniteExtent> {
                before,
                at: addr::InfiniteExtent::between(b.address, after_break.address),
                after: match breaks.get(containing_break_index + 2) {
                    Some(after_after_break) => Some(addr::InfiniteExtent::between(after_break.address, after_after_break.address)),
                    None => Some(addr::InfiniteExtent::infinite(after_break.address))
                }
            },
            None => addr::Triplet::<addr::InfiniteExtent> {
                before,
                at: addr::InfiniteExtent::infinite(b.address),
                after: None
            }
        }
    }
}
