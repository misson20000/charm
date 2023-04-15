use std::sync;
use std::sync::Arc;

use crate::datapath::ByteRecord;
use crate::datapath::ByteRecordRange;
use crate::datapath::DataFlags;
use crate::datapath::space::AddressSpace;

use parking_lot::Mutex;
use tokio::sync::Notify;

/// A slot in the cache that has been reserved for a specific block address and does not depend on the type of the underlying Space.
#[derive(Clone)]
struct Entry {
    /// The address of the block that this entry contains or is fetching. Acts as cache tag, but technically contains more information than it needs to.
    address: u64, // cache tag

    /// How many FetchFutures are waiting on this specific memory block. We can't turn this slot over to another memory block until all the FetchFutures that wanted data from it are satisfied.
    pins: usize,
    
    data: Box<[ByteRecord]>,

    completed: bool,
}

struct Slot {
    /* Unfortunately, a tokio::sync::RwLock is not suitable here. We want a read-favoring priority policy to make sure that various L1 cache lines can slurp all the data they need out of a given L2 line before we turn it over to a different block. */
    entry: Mutex<Option<Entry>>,

    /* Signaled when the entry has been evicted and the backend needs to start a new fetch to populate the new entry. */
    reset: Notify,

    /* Signaled when the last pin has been released and the entry can be turned over. */
    evictable: Notify,

    /* Signaled when the future has finished loading data. */
    completed: Notify,
}

impl Clone for Slot {
    fn clone(&self) -> Slot {
	Slot {
	    entry: Mutex::new(self.entry.lock().clone()),
	    reset: tokio::sync::Notify::new(),
	    evictable: tokio::sync::Notify::new(),
	    completed: tokio::sync::Notify::new(),
	}
    }
}

struct SlotPin<'cache> {
    slot: &'cache Slot,
    block_addr: u64,
}

/// Implementation of a direct-mapped cache. I may change this to set or full associativity later if there turns out to
/// be a use-case that causes a lot of cache conflicts which wind up meaningfully affecting performance. I think this is
/// unlikely.
///
/// One of the design goals of the Cache is to erase the type of the underlying address space without making access
/// synchronous. Each cache slot stores data, state, and some signaling mechanisms while the actual fetching that
/// depends on the underlying address space is done on a separate task.
pub struct Cache {
    block_size: usize,
    slots: Arc<[Slot]>,
    fetch_task: tokio::task::JoinHandle<()>,
}

impl Cache {
    pub fn new<Space: AddressSpace>(space: Space, block_size_power: u8, block_count_power: u8) -> Cache {
        let block_size = 1 << block_size_power;
        let block_count = 1 << block_count_power;

	// TODO: come up with a much better way to initialize this
        let slots: Arc<[Slot]> = <Arc<[Slot]> as From<&[Slot]>>::from(&std::iter::repeat_with(Slot::new).take(block_count).collect::<Vec<Slot>>()[..]);
	let slots_clone = Arc::clone(&slots);

	let fetch_task = tokio::spawn(CacheBackend::new(space, slots, block_size));
	
        Cache {
            block_size,
            slots: slots_clone,
	    fetch_task,
        }
    }

    fn get_block_addr(&self, addr: u64) -> u64 {
	addr & !(self.block_size as u64 - 1)
    }
    
    fn get_slot_for_block_addr(&self, block_addr: u64) -> &Slot {
	let slot_index = (block_addr / self.block_size as u64) as usize;
	&self.slots[slot_index]
    }

    /// Fetches the bytes specified by the given range. If the range spans multiple cache blocks, they won't be fetched in parallel, but if other parallel fetches cause data to get loaded that this fetch is also interested in, it may opportunistically snoop the data.
    pub async fn fetch_range<'cache, 'output>(&'cache self, range: ByteRecordRange<'output>)  {
	/* Clear the output range. */
	for record in range.out {
	    record.store(ByteRecord {
		value: 0,
		flags: DataFlags::PENDING
	    }, sync::atomic::Ordering::Relaxed);
	}

	let begin_block_addr = self.get_block_addr(range.addr);
	let end_block_addr = self.get_block_addr(range.addr + range.out.len() as u64 + self.block_size as u64 - 1);
	let blocks_iter = (begin_block_addr..end_block_addr).step_by(self.block_size as usize);

	let mut held_pin = None;
	
	loop {
	    let mut eviction_future = None;
	    let mut completion_future = None;
	    let mut finished = true;
	    
	    /* Load whatever data happens to be in the cache right now. */
	    for block_addr in blocks_iter.clone() {
		/* Get the index of the first record in the block we're considering. */
		let first_record_index = if block_addr < range.addr {
		    0
		} else {
		    (block_addr - range.addr) as usize
		};

		/* Use it to figure out whether we've already loaded the data for this block. */
		if range.out[first_record_index].load(sync::atomic::Ordering::Relaxed).flags.contains(DataFlags::PENDING) {
		    continue;
		}

		/* We haven't loaded the data for this block yet, so try to. */
		let slot = self.get_slot_for_block_addr(block_addr);

		/* Try to pin the slot. We may already be holding a pin for it from a previous iteration of the outer loop. That's ok. */
		let pin = match slot.try_pin(block_addr, self.block_size) {
		    Some(p) => p,
		    None => {
			if eviction_future.is_none() {
			    eviction_future = Some(slot.evictable.notified());
			} else {
			    /* Tough cookies. We'll catch it later. */
			}
			
			/* Opportunistically process the remaining blocks. We can't do anything more for this one. */
			finished = false;
			continue;
		    },
		};

		/* Check if the data has been loaded. */
		let entry = pin.get_entry();
		if !entry.completed {
		    if completion_future.is_none() {
			/* Hold the pin across the await. */
			held_pin = Some(pin);

			completion_future = Some(slot.completed.notified());
		    } else {
			/* Tough cookies. Maybe we'll catch it later. */
		    }

		    /* Opportunistically process the remaining blocks. We can't do anything more for this one. */
		    finished = false;
		    continue;
		}

		/* Copy data to output */
		let offset = (range.addr - block_addr) as usize;
		for (i, record_out) in range.out.iter().enumerate() {
		    let record = entry.data[i - offset];

		    /* If the completed variable was set, none of the records should still be pending. */
		    assert!(!record.flags.contains(DataFlags::PENDING));
		    
		    record_out.store(record, sync::atomic::Ordering::Relaxed);
		}
	    }
	    
	    if finished {
		assert!(eviction_future.is_none());
		assert!(completion_future.is_none());
		std::mem::drop(held_pin);
		return;
	    }
	    
	    let eviction_future = futures::future::OptionFuture::from(eviction_future);
	    let completion_future = futures::future::OptionFuture::from(completion_future);

	    futures::pin_mut!(eviction_future);
	    futures::pin_mut!(completion_future);
	    
	    futures::future::select(completion_future, eviction_future).await;
	};
    }
}

impl Slot {
    fn new() -> Self {
        Slot {
            entry: Mutex::new(None),
	    reset: tokio::sync::Notify::new(),
	    evictable: tokio::sync::Notify::new(),
	    completed: tokio::sync::Notify::new(),
        }
    }

    fn try_pin(&self, block_addr: u64, block_size: usize) -> Option<SlotPin<'_>> {
	let mut guard = self.entry.lock();

	match guard.as_mut() {
	    /* Cold start miss; the slot was uninitialized. Fill it with a fresh entry with one pin. */
	    None => *guard = Some(Entry::new(block_addr, 1, block_size, &self.reset)),

	    /* This slot already had that block in it! Add a pin. */
	    Some(e) if e.address == block_addr => {
		e.pins+= 1;
	    },

	    /* We have a different block loaded, but nobody has it pinned. Go ahead and turn it over. */
	    Some(e) if e.pins == 0 => {
		e.turnover(block_addr, 1, &self.reset);
	    },

	    /* We have a different block loaded and other users have it pinned. Caller has to wait until it's evictable. */
	    _ => return None
	}
	
	Some(SlotPin {
	    slot: self,
	    block_addr,
	})	
    }
}

impl Entry {
    fn new(address: u64, pins: usize, block_size: usize, reset: &tokio::sync::Notify) -> Entry {
	reset.notify_one();
	
	Entry {
	    address,
	    pins,
	    // TODO: find a better way to initialize this
	    data: std::iter::repeat(ByteRecord::new(0, DataFlags::PENDING)).take(block_size as usize).collect::<Vec<ByteRecord>>().into_boxed_slice(),
	    completed: false,
	}
    }

    fn turnover(&mut self, new_address: u64, new_pins: usize, reset: &tokio::sync::Notify) {
	assert_eq!(self.pins, 0);
	
	self.address = new_address;
	self.pins = new_pins;
	self.completed = false;

	/* Reuse the allocated buffer */
	for record in self.data.iter_mut() {
	    *record = ByteRecord::new(0, DataFlags::PENDING);
	}

	reset.notify_one();
    }
}

impl<'cache> SlotPin<'cache> {
    fn get_entry(&self) -> parking_lot::MappedMutexGuard<'cache, Entry> {
	let guard = self.slot.entry.lock();
	let entry = parking_lot::MutexGuard::map(guard, |option| option.as_mut().expect("having been able to pin a cache entry means it should've been initialized"));

	/* slot shouldn't have been able to turn over while we had it pinned */
	assert_eq!(entry.address, self.block_addr);
	
	entry
    }
}

impl<'cache> Drop for SlotPin<'cache> {
    fn drop(&mut self) {
	let mut e = self.get_entry();
	e.pins-= 1;

	if e.pins == 0 {
	    /* I would love to only wake up one waiter, but if there are two waiters, we wake up one of them, and then that one gets dropped, the other might be stuck waiting forever. */
	    self.slot.evictable.notify_waiters();
	}
    }
}

impl Drop for Cache {
    fn drop(&mut self) {
	self.fetch_task.abort();
    }
}

impl std::fmt::Debug for Cache {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        Ok(())
    }
}

//type SlotBackend<Space> = impl Future<Output = ()>;

struct CacheBackend<Space: AddressSpace> {
    space: Space,
    block_size: usize,
    fe_slots: Arc<[Slot]>,
    //be_slots: Box<[SlotBackend<Space>]>
}

impl<Space: AddressSpace> CacheBackend<Space> {
    fn new(space: Space, slots: Arc<[Slot]>, block_size: usize) -> Self {
	CacheBackend {
	    space,
	    block_size,
	    fe_slots: slots,
	}
    }
}

impl<Space: AddressSpace> std::future::Future for CacheBackend<Space> {
    type Output = ();
    
    fn poll(self: std::pin::Pin<&mut Self>, _cx: &mut std::task::Context<'_>) -> std::task::Poll<()> {
	/*
	for (be_slot, fe_slot) in self.be_slots.zip(self.fe_slots) {
	    
    }
	*/
	
	std::task::Poll::Pending
    }
}
