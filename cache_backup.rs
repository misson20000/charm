use std::future::Future;
use std::sync;
use std::task::Context;
use std::task::Poll;

use crate::model::datapath;

use parking_lot::Mutex;
use tokio::sync::Notify;

/// A slot in the cache that has been reserved for a specific block address and does not depend on the type of the underlying Space.
struct Entry {
    /// The address of the block that this entry contains or is fetching. Acts as cache tag, but technically contains more information than it needs to.
    address: u64, // cache tag

    /// How many FetchFutures are waiting on this specific memory block. We can't turn this slot over to another memory block until all the FetchFutures that wanted data from it are satisfied.
    pins: u64,
    
    data: Box<[AtomicByteRecord]>,

    completed: bool,
}

struct Slot<Space: space::AddressSpace> {
    /* Unfortunately, a tokio::sync::RwLock is not suitable here. We want a read-favoring priority policy to make sure that various L1 cache lines can slurp all the data they need out of a given L2 line before we turn it over to a different block. */
    entry: Mutex<Option<Entry>>,
    
    future: Mutex<Option<(u64, Space::Future)>>,

    /* Signaled when the entry has been evicted and the backend needs to start a new fetch to populate the new entry. */
    reset: Notify,

    /* Signaled when the last pin has been released and the entry can be turned over. */
    evictable: Notify,

    /* Signaled when the future has finished loading data. */
    completed: Notify,
}

struct EntryPin<'cache> {
    address: u64,
    mutex: &'cache Mutex<Option<Entry>>,
    evictable_notify: &'cache Notify,
}

/// One of the design goals of the Cache is to erase the type of the underlying address space without making access synchronous. The most convenient way to do that is to make the Cache implementation generic over address spaces, and have them all implement a common base trait. The dynamic dispatch happens on cache access, but this is an L2 cache so this is a warm-temperature path at worst. The returned future type is the same regardless of the type of the underlying Space.
pub trait BaseCache {
    fn fetch_range<'cache, 'output>(&'cache self, range: datapath::ByteRecordRange<'output>) -> FetchFuture<'cache, 'output>;
}

/// Implementation of a direct-mapped cache. I may change this to set or full associativity later if there turns out to be a use-case that
/// causes a lot of cache conflicts which wind up meaningfully affecting performance. I think this is unlikely.
pub struct Cache<Space: space::AddressSpace> {
    block_size: u64,
    block_count: u64,
    entries: Pin<Box<[Slot<Space>]>>,
    space: Space,
}

enum FetchFutureState<'cache> {
    Invalid,
    WaitingForEntryPin(tokio::sync::futures::Notfied<'cache>),
    AcquiredPin(EntryPin<'cache>),
    WaitingForEntryCompletion(tokio::sync::futures::Notified<'cache>, EntryPin<'cache>),
    Finished,
}

/// This needs to erase the type of the underlying Space.
pub struct FetchFuture<'cache, 'output> {
    range: datapath::ByteRecordRange<'output>,
    block_addr: u64,

    state: FetchFutureState<'cache>,
    
    mutex: &'cache Mutex<Option<Entry>>,
    reset_notify: &'cache Notify,
    evictable_notify: &'cache Notify,
    completed_notify: &'cache Notify,
}

impl<Space: space::AddressSpace> Cache<Space> {
    fn new(space: Space, block_size_power: u8, block_count_power: u8) -> SpaceCache {
        let block_size = 1 << block_size_power;
        let block_count = 1 << block_count_power;
        
        let entries = Box::into_pin(std::iter::repeat_with(Slot::new).take(block_count).collect());
        
        SpaceCache {
            block_size,
            block_count,
            entries,
            space,
        }
    }

    fn get_pinned_entry(&self, index: usize) -> Pin<&Slot<Space>> {
        /* SAFETY: the slice is in a box. it won't move. therefore the slice's items won't move either. */
        unsafe { self.entries.map_unchecked(|entries| &entries[index]) }
    }    

    fn fetch_range<'cache, 'output>(&'cache self, range: datapath::ByteRecordRange<'output>) -> FetchFuture<'cache, 'output> {
        let block_addr = output.address & ~(self.block_size - 1);
        let slot = self.get_pinned_entry(block_addr & ~(self.block_count - 1));

        loop {
            let guard = slot.entry.lock();

            match guard {
                None => *guard = /* TODO: is type erasure a design goal of this cache? */
            }
        }
    }    
}

impl<Space: space::AddressSpace> Slot<Space> {
    fn new() -> Self {
        Slot {
            entry: Mutex::new(None),
	    future: Mutex::new(None),
	    reset: tokio::Notify::new(),
	    evictable: tokio::Notfy::new(),
	    completed: tokio::Notifiy::new(),
        }
    }
}

fn try_pin_entry<'cache>(mutex: &'cache Mutex<Option<Entry>>, address: u64, block_size: usize, evictable_notify: &'cache Notify, reset_notifiy: &'cache Notify) -> Option<EntryPin<'cache>> {
    let guard = mutex.lock();

    match guard.as_mut() {
	/* Cold start miss; the slot was uninitialized. Fill it with a fresh entry with one pin. */
	None => *guard = Some(Entry::new(address, 1, block_size, reset_notify)),

	/* This slot already had that block in it! Add a pin. */
	Some(e) if e.address == address {
	    e.pins+= 1;
	},

	/* We have a different block loaded, but nobody has it pinned. Go ahead and turn it over. */
	Some(e) if e.pins == 0 {
	    e.evict(address, 1, reset_notify);
	},

	/* We have a different block loaded and other users have it pinned. Caller has to wait until it's evictable. */
	_ => return None
    }
    
    Some(EntryPin {
	address,
	mutex,
	evictable_notify,
    })
}

impl Entry {
    fn new(address: u64, pins: usize, block_size: usize, reset: &tokio::Notify) -> Entry {
	reset.notify_one();
	
	Entry {
	    address,
	    pins,
	    data: std::iter::repeat_with(ByteRecord::new(0, DataFlags::PENDING)).take(block_size).collect().into_boxed_slice(),
	    completed: false,
	}
    }

    fn evict(&mut self, new_address: u64, new_pins: u64, reset: &tokio::Notify) {
	assert_eq!(self.pins, 0);
	
	self.address = new_address;
	self.pins = new_pins;
	self.completed = false;

	/* Reuse the allocated buffer */
	for record in &mut self.data {
	    *record = ByteRecord::new(0, DataFlags::PENDING);
	}

	reset.notify_one();
    }
}

impl<'cache> EntryPin<'cache> {
    fn get_entry(&self) -> parking_lot::MappedMutexGuard<'cache, Entry> {
	let guard = self.mutex.lock().map(|x| x.expect("having been able to pin a cache entry means it should've been initialized"));

	/* slot shouldn't have been able to turn over while we had it pinned */
	assert_eq!(guard.address, self.address);
	
	guard
    }
}

impl<'cache> Drop for EntryPin<'cache> {
    fn drop(&mut self) {
	let mut e = self.get_entry();
	e.pins-= 1;

	if e.pins == 0 {
	    /* I would love to only wake up one waiter, but if there are two waiters, we wake up one of them, and then that one gets dropped, the other might be stuck waiting forever. */
	    self.evictable_notify.notify_waiters();
	}
    }
}

impl<'cache, 'output> Future for FetchFuture<'cache, 'output> {
    type Output = ();

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
	loop {
	    match std::mem::replace(&mut self.state, FetchFutureState::Invalid) {
		FetchFutureState::Invalid => panic!(),
		FetchFutureState::WaitingForEntryPin(evictable) => {
		    match evictable.poll(cx) {
			Poll::Pending => {
			    self.state = FetchFutureState::WaitingForEntryPin(evictable);
			    return Poll::Pending;
			},
			Poll::Finished(_) => _,
		    };

		    self.state = match try_pin_entry(self.mutex, self.block_addr, self.block_size, self.evictable_notify, self.reset_notify) {
			/* Pin succeeded! Move on to the next state. */
			Some(pin) => FetchFutureState::AcquiredPin(pin),

			/* Couldn't pin. We were probably woken up by notify_all and someone else got to it first. Keep waiting... */
			None => FetchFutureState::WaitingForEntryPin(self.evictable_notify.notified()),
		    };
		},
		FetchFutureState::AcquiredPin(pin) => {
		    let notified = self.completed_notify.notified();
		    if entry.completed {
			/* Copy data to output */
			let offset = self.range.addr - self.block_addr;
			for (i, record) in self.range.out {
			    record.store(entry.data[i - offset], sync::atomic::Ordering::Relaxed);
			}

			self.state = FetchFutureState::Finished;
			return Poll::Finished(_);
		    } else {
			self.state = FetchFutureState::WaitingForEntryCompletion(pin, notified)
		    }
		},
		FetchFutureState::WaitingForEntryCompletion(pin, notified) => {
		    match notified.poll() {
			Poll::Pending => {
			    self.state = FetchFutureState::WaitingForEntryCompletion(pin, notified);
			    return Poll::Pending;
			},
			Poll::Finished(_) => {
			    self.state = FetchFutureState::AcquiredPin(pin);
			},
		    }
		},
		FetchFutureState::Finished => {
		    self.state = FetchFutureState::Finished;
		    return Poll::Finished(_);
		},
	    };
	};
    }
}

impl std::fmt::Debug for SpaceCache {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        Ok(())
    }
}
