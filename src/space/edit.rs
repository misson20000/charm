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
    fn break_list_changed(&self, breaks: &vec::Vec<sync::Arc<Break>>);
}

pub enum BreakEditError {
    NotFound,
}

pub enum BreakDeletionError {
    IsZeroBreak,
    NotFound,
}
pub enum BreakInsertionError {
    ExistingBreak(sync::Arc<Break>)
}

pub struct SpaceEditor {
    pub space: sync::Arc<dyn AddressSpace + Send + Sync>,
    break_list: sync::RwLock<vec::Vec<sync::Arc<Break>>>,
    break_list_observers: sync::Mutex<vec::Vec<sync::Weak<dyn BreakListObserver>>>,
}

impl SpaceEditor {
    pub fn new(space: sync::Arc<dyn AddressSpace + Send + Sync>) -> sync::Arc<SpaceEditor> {
        sync::Arc::new(SpaceEditor {
            break_list: sync::RwLock::new(vec![
                Break::new(addr::Address { byte: 0x00000000, bit: 0}, space.get_label()),
            ]),
            break_list_observers: sync::Mutex::new(vec::Vec::new()),
            space,
        })
    }

    /* I would just make break_list pub, but I want to control write access
     * to enforce invariants like "break list must be sorted" and "break
     * list must notify observers on change" */
    pub fn get_breaks(&self)-> sync::RwLockReadGuard<vec::Vec<sync::Arc<Break>>> {
        self.break_list.read().unwrap()
    }

    pub fn edit_break(&self, brk: &sync::Arc<Break>, label: &str) -> Result<(), BreakEditError> {
        let mut breaks = self.break_list.write().unwrap();

        match breaks.binary_search_by(|b| b.address.cmp(&brk.address)) {
            Result::Ok(i) => {
                std::mem::replace(&mut breaks[i], Break::new(brk.address, label));
                self.notify_observers(&breaks);
                Ok(())
            },
            Result::Err(_) => { Err(BreakEditError::NotFound) }
        }
    }

    pub fn delete_break(&self, brk: &sync::Arc<Break>) -> Result<(), BreakDeletionError> {
        let mut breaks = self.break_list.write().unwrap();

        match breaks.binary_search_by(|b| b.address.cmp(&brk.address)) {
            Result::Ok(i) if i == 0 => Err(BreakDeletionError::IsZeroBreak),
            Result::Ok(i) => {
                breaks.remove(i);
                self.notify_observers(&breaks);
                Ok(())
            },
            Result::Err(_) => { Err(BreakDeletionError::NotFound) }
        }
    }
    
    pub fn insert_break(&self, addr: addr::Address, label: &str) -> Result<(), BreakInsertionError> {
        let mut breaks = self.break_list.write().unwrap();

        match breaks.binary_search_by(|b| b.address.cmp(&addr)) {
            Result::Ok(idx) => Err(BreakInsertionError::ExistingBreak(breaks[idx].clone())),
            Result::Err(idx) => {
                breaks.insert(idx, Break::new(addr, label));
                self.notify_observers(&breaks);
                Ok(())
            }
        }
    }

    /// Gets the break at an address.
    pub fn break_at(&self, target: addr::Address) -> Result<sync::Arc<Break>, sync::Arc<Break>> {
        let breaks = self.get_breaks();

        match breaks.binary_search_by(|b| b.address.cmp(&target)) {
            Result::Ok(i) => Ok(breaks[i].clone()),
            Result::Err(i) if i == 0 => panic!("somehow we're above the zero break"),
            Result::Err(i) => Err(breaks[i-1].clone()),
        }
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

    pub fn add_break_list_observer(&self, observer: &sync::Arc<dyn BreakListObserver>) {
        self.break_list_observers.lock().unwrap().push(sync::Arc::downgrade(observer));
    }
    
    pub fn remove_break_list_observer(&self, observer: &sync::Weak<dyn BreakListObserver>) {
        // TODO: the pointer comparison here is bogus: https://github.com/rust-lang/rust/issues/46139
        // but it doesn't really matter because we run after Drop for ListingEngineBreakObsever
        self.break_list_observers.lock().unwrap().retain(|e| !e.ptr_eq(observer) && e.upgrade().is_some());
    }

    fn notify_observers(&self, breaks: &vec::Vec<sync::Arc<Break>>) {
        self.break_list_observers.lock().unwrap().retain(|observer| {
            match observer.upgrade() {
                Some(observer) => { observer.break_list_changed(breaks); true },
                None => false
            }
        });
    }
}
