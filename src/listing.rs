pub mod view;
pub mod brk;
pub mod line_group;

use std::sync;
use std::vec;

use crate::addr;
use crate::space::AddressSpace;

extern crate im;
use im::ordmap;

type BreakMap = ordmap::OrdMap<addr::Address, sync::Arc<brk::Break>>;

pub struct Listing {
    pub space: sync::Arc<dyn AddressSpace + Send + Sync>,
    break_map: parking_lot::RwLock<BreakMap>,
    break_list_observers: sync::Mutex<vec::Vec<sync::Weak<dyn BreakListObserver>>>,
}

impl Listing {
    pub fn new(space: sync::Arc<dyn AddressSpace + Send + Sync>) -> Listing {
        Listing {
            break_map: parking_lot::RwLock::new(ordmap!{
                addr::unit::NULL => sync::Arc::new(brk::Break::new(
                    addr::unit::NULL,
                    Some(space.get_label()),
                    brk::BreakClass::Hex(brk::hex::HexBreak {
                        line_size: addr::Size::from(16)
                    })))
            }),
            break_list_observers: sync::Mutex::new(vec::Vec::new()),
            space,
        }
    }

    /* I would just make break_map pub, but I want to control write access
     * to enforce invariants like "break list must be sorted" and "break
     * list must notify observers on change" */
    pub fn get_break_map(&self) -> parking_lot::RwLockReadGuard<BreakMap> {
        self.break_map.read_recursive()
    }

    /// Inserts a break or replaces an existing one.
    pub fn insert_break(&self, brk: brk::Break) {
        let mut breaks = self.break_map.write();
        breaks.insert(brk.addr, sync::Arc::new(brk));
        self.invalidate_break_list(breaks);
    }
    
    /// Removes a break.
    pub fn delete_break(&self, addr: &addr::Address) -> Result<(), BreakDeletionError> {
        let mut breaks = self.break_map.write();

        if *addr == addr::unit::NULL {
            return Err(BreakDeletionError::IsZeroBreak);
        }
        
        match breaks.remove(addr) {
            Some(_) => {
                self.invalidate_break_list(breaks);
                Ok(())
            },
            None => Err(BreakDeletionError::NotFound)
        }
    }

    /// Subscribes an object to be notified every time the break list changes.
    pub fn add_break_list_observer(&self, observer: &sync::Arc<dyn BreakListObserver>) {
        self.break_list_observers.lock().unwrap().push(sync::Arc::downgrade(observer));
    }

    /// Removes a break list observer.
    pub fn remove_break_list_observer(&self, observer: &sync::Weak<dyn BreakListObserver>) {
        // TODO: the pointer comparison here is bogus: https://github.com/rust-lang/rust/issues/46139
        // but it doesn't really matter because we run after Drop for ListingEngineBreakObsever
        self.break_list_observers.lock().unwrap().retain(|e| !e.ptr_eq(observer) && e.upgrade().is_some());
    }

    /// Signals all of the break list observers that the list has been invalidated.
    fn invalidate_break_list(&self, guard: parking_lot::RwLockWriteGuard<BreakMap>) {
        std::mem::drop(guard);
        
        self.break_list_observers.lock().unwrap().retain(|observer| {
            match observer.upgrade() {
                Some(observer) => { observer.break_list_changed(); true },
                None => false
            }
        });
    }
}

pub trait BreakMapExt {
    fn break_at(&self, addr: addr::Address) -> &sync::Arc<brk::Break>;
    fn break_before<'a, 'b>(&'a self, brk: &'b sync::Arc<brk::Break>) -> Option<&'a sync::Arc<brk::Break>>;
    fn break_after<'a, 'b>(&'a self, brk: &'b sync::Arc<brk::Break>) -> Option<&'a sync::Arc<brk::Break>>;
    fn extents_at(&self, target: addr::Address) -> addr::Extent;
    fn extents_near(&self, target: addr::Address) -> addr::ExtentTriplet;
}

impl BreakMapExt for BreakMap {
    /// Gets the break at an address.
    fn break_at(&self, addr: addr::Address) -> &sync::Arc<brk::Break> {
        self.get_prev(&addr).expect("zero break should always exist").1
    }

    fn break_before<'a, 'b>(&'a self, brk: &'b sync::Arc<brk::Break>) -> Option<&'a sync::Arc<brk::Break>> {
        if brk.addr == addr::unit::NULL {
            None
        } else {
            Some(self.get_prev(&(brk.addr - addr::unit::BIT)).expect("zero break should always exist").1)
        }
    }

    fn break_after<'a, 'b>(&'a self, brk: &'b sync::Arc<brk::Break>) -> Option<&'a sync::Arc<brk::Break>> {
        if brk.addr == addr::unit::REAL_END {
            None
        } else {
            self.get_next(&(brk.addr + addr::unit::BIT)).map(|tuple| tuple.1)
        }
    }

    /// Gets the extents of the break containing the target address.
    fn extents_at(&self, target: addr::Address) -> addr::Extent {
        let at = self.get_prev(&target).expect("zero break should always exist").0;
        let next = self.get_next(&(target + addr::unit::BIT));
        
        match next {
            Some(next) => addr::Extent::between(*at, *next.0),
            None => addr::Extent::unbounded(*at)
        }
    }

    /// Gets the extents of the break containing the target address, and if they
    /// exist, the extents of the breaks before and after.
    fn extents_near(&self, target: addr::Address) -> addr::ExtentTriplet {
        let at = self.get_prev(&target).expect("zero break should always exist").0;
        let next = self.get_next(&(target + addr::unit::BIT)).map(|tuple| tuple.0);

        addr::ExtentTriplet {
            before: if *at == addr::unit::NULL {
                None
            } else {
                self.get_prev(&(*at - addr::unit::BIT)).map(|tuple| *tuple.0)
            },
            at: *at,
            after: match next {
                Some(next) => match self.get_next(&(*next + addr::unit::BIT)).map(|tuple| tuple.0) {
                    Some(next_next) => addr::ExtentTripletEnding::AfterBounded(*next, *next_next),
                    None => addr::ExtentTripletEnding::AfterUnbounded(*next),
                },
                None => addr::ExtentTripletEnding::AtUnbounded,
            }
        }
    }
}

pub trait BreakListObserver : Send + Sync {
    fn break_list_changed(&self);
}

pub enum BreakDeletionError {
    IsZeroBreak,
    NotFound,
}
