use std::string;
use std::sync;

use bitflags::bitflags;

pub mod cache;
pub mod filter;
pub mod space;

bitflags! {
    pub struct DataFlags: u8 {
        /// The [cache::Cache] is in the process of fetching this byte.
        const PENDING = 0b00000001;

        /// Touched by a [filter::load::LoadSpaceFilter]
        const LOADED = 0b00000010;

        /// Touched by an [filter::overwrite::OverwriteFilter]
        const OVERWRITTEN = 0b00000100;

        /// Touched by an [filter::insert::InsertFilter]
        const INSERTED = 0b00001000;

        /// Touched by a [filter::shift::ShiftFilter] or occurs after an [filter::insert::InsertFilter]
        const SHIFTED = 0b00010000;

        /// Datapath encountered an error while fetching this byte.
        const ERROR = 0b10000000;
    }
}

#[derive(Debug, Clone, Copy)]
/// A byte value that is either currently being asynchronously loaded from a datapath, or has already been loaded, along with some metadata about how it was fetched.
pub struct ByteRecord {
    value: u8,
    flags: DataFlags,
}

impl ByteRecord {
    pub const PENDING: ByteRecord = Self::new(0, DataFlags::PENDING);
    
    pub fn new(value: u8, flags: DataFlags) -> ByteRecord {
	ByteRecord {
	    value,
	    flags
	}
    }

    pub fn value(&self) -> Option<u8> {
        if self.flags.contains(DataFlags::LOADED) {
            Some(self.value)
        } else {
            None
        }
    }

    pub fn flags(&self) -> DataFlags {
	self.flags
    }
}

/// Atomic version of [ByteRecord].
pub struct AtomicByteRecord {
    /* Value stored in upper 8 bits, flags stored in lower 8 bits. */
    value_and_flags: sync::atomic::AtomicU16,
}

impl AtomicByteRecord {
    pub fn load(&self, ordering: sync::atomic::Ordering) -> ByteRecord {
	let value_and_flags = self.value_and_flags.load(ordering);
	Self::unpack(value_and_flags)
    }
    
    pub fn store(&self, value: ByteRecord, ordering: sync::atomic::Ordering) {
	self.value_and_flags.store(Self::pack(value), ordering);
    }

    fn pack(br: ByteRecord) -> u16 {
	((br.value as u16) << 8) | (br.flags.bits() as u16)
    }

    fn unpack(packed: u16) -> ByteRecord {
	let value = (packed >> 8) as u8;
	let flags = DataFlags::from_bits_truncate(((packed) & 0xff) as u8);
	
	ByteRecord { value, flags }
    }
}

impl From<ByteRecord> for AtomicByteRecord {
    fn from(br: ByteRecord) -> AtomicByteRecord {
	AtomicByteRecord {
	    value_and_flags: Self::pack(br).into()
	}
    }
}

#[derive(Default, Clone, Copy)]
/// Attaches an address to a slice of [AtomicByteRecord]s. Used when fetching bytes from datapath to keep track of what address bytes need to be fetched from and where they need to be stored when they're fetched.
pub struct ByteRecordRange<'a> {
    addr: u64,
    out: &'a [AtomicByteRecord]
}

#[derive(Clone, Debug)]
pub enum Filter {
    LoadSpace(filter::load_space::LoadSpaceFilter),
    //Overwrite(filter::load_space::OverwriteFilter),
    //Move(filter::load_space::MoveFilter),
    //Insert(filter::load_space::InsertFilter),
}

#[derive(Clone)]
pub struct DataPath {
    filters: imbl::Vector<Filter>,
}

pub enum FetchResult {
    Done,
    DidWork,
    DidntDoWork,
}

impl Filter {
    /// If the two provided filters can be combined, return a single filter that has the same effect as `a` followed by `b`.
    pub fn stack(a: &Filter, b: &Filter) -> Option<Filter> {
        match (a, b) {
            /*
            (Filter::LoadSpace(_), Filter::LoadSpace(_)) => None,
            (Filter::Overwrite(ai), Filter::Overwrite(bi)) => OverwriteFilter::stack(ai, bi).map(Filter::Overwrite),
            (Filter::Move(ai), Filter::Move(bi)) => MoveFilter::stack(ai, bi).map(Filter::Move),
            (Filter::Insert(ai), Filter::Insert(bi)) => InsertFilter::stack(ai, bi).map(Filter::Insert),
            (Filter::Insert(ai), Filter::Overwrite(bi)) => InsertFilter::stack_overwrite(ai, bi).map(Filter::Insert),
            */
            _ => None,
        }
    }

    /// Loads bytes into the ByteRecordRange, or mutates it to affect how it is viewed by other filters.
    async fn fetch<'out>(&self, range: &mut ByteRecordRange<'out>) -> FetchResult {
        match self {
            Filter::LoadSpace(f) => f.fetch(range).await,
            /*
            Filter::Overwrite(f) => f.fetch(iter, range, cx),
            Filter::Move(f) => f.fetch(iter, range, cx),
            Filter::Insert(f) => f.fetch(iter, range, cx),
            */
        }
    }

    pub fn human_details(&self) -> string::String {
        match self {
            Filter::LoadSpace(f) => f.human_details(),
            /*
            Filter::Overwrite(f) => f.human_details(),
            Filter::Move(f) => f.human_details(),
            Filter::Insert(f) => f.human_details(),
            */
        }
    }

    pub fn human_affects_addr(&self) -> u64 {
        match self {
            Filter::LoadSpace(f) => f.human_affects_addr(),
            /*
            Filter::Overwrite(f) => f.human_affects_addr(),
            Filter::Move(f) => f.human_affects_addr(),
            Filter::Insert(f) => f.human_affects_addr(),
            */
        }
    }

    pub fn human_affects_size(&self) -> Option<u64> {
        match self {
            Filter::LoadSpace(f) => f.human_affects_size(),
	    /*
            Filter::Overwrite(f) => f.human_affects_size(),
            Filter::Move(f) => f.human_affects_size(),
            Filter::Insert(f) => f.human_affects_size(),
            */
        }
    }
}

impl DataPath {
    pub fn new() -> DataPath {
	DataPath {
	    filters: imbl::Vector::new(),
	}
    }

    pub fn push_back(&mut self, filter: Filter) {
	self.filters.push_back(filter)
    }

    pub fn iter(&self) -> imbl::vector::Iter<'_, Filter> {
	self.filters.iter()
    }
    
    pub async fn fetch(self, original_range: ByteRecordRange<'_>) {
	/* This future needs to have static lifetime, not the lifetime of our self reference. */
	//let filters = self.filters.clone();

	for record in original_range.out {
	    record.store(ByteRecord::new(0, DataFlags::PENDING), std::sync::atomic::Ordering::Relaxed);
	}
	
	/* Some filters "split" a ByteRecordRange and need to be run multiple times. Each time they're run, they use the PENDING flags to figure out which branch they should take. */
        while original_range.out.iter().any(|r| r.load(std::sync::atomic::Ordering::Relaxed).flags.contains(DataFlags::PENDING)) {
	    let mut range = original_range.clone();
	    let mut has_done_work = false;
	    
            for filter in self.filters.iter().rev() {
                match filter.fetch(&mut range).await {
		    FetchResult::Done => {
			/* If the filter said its range was done, it should have cleared the PENDING flags. */
			assert!(!range.out.iter().any(|r| r.load(std::sync::atomic::Ordering::Relaxed).flags.contains(DataFlags::PENDING)));
			has_done_work = true;
		    },
		    FetchResult::DidWork => has_done_work = true,
		    FetchResult::DidntDoWork => {},
		};
            }

	    assert!(has_done_work);
        }
    }
}

impl<'a> ByteRecordRange<'a> {
    pub fn new(addr: u64, records: &'a [AtomicByteRecord]) -> ByteRecordRange {
        ByteRecordRange {
            addr,
            out: records,
        }
    }
    
    fn is_empty(&self) -> bool {
        self.out.is_empty()
    }

    fn split2(self, addr: u64) -> (ByteRecordRange<'a>, ByteRecordRange<'a>) {
        if self.addr >= addr {
            /* if we are entirely after the split point */
            (ByteRecordRange::default(), ByteRecordRange {
                addr: self.addr,
                out: self.out,
            })
        } else if self.addr < addr && self.addr + self.out.len() as u64 > addr {
            /* if we overlap the split point */
            let (a, b) = self.out.split_at((addr - self.addr) as usize);
            
            (ByteRecordRange {
                addr: self.addr,
                out: a,
            }, ByteRecordRange {
                addr,
                out: b
            })
        } else if self.addr < addr && self.addr + self.out.len() as u64 <= addr {
            /* if we are entirely before the split point */
            (ByteRecordRange {
                addr: self.addr,
                out: self.out,
            }, ByteRecordRange::default())
        } else {
            panic!("unreachable")
        }
    }
    
    fn split3(self, (addr, size): (u64, Option<u64>)) -> (ByteRecordRange<'a>, ByteRecordRange<'a>, ByteRecordRange<'a>) {
        let (before, b) = self.split2(addr);
        let (overlap, after) = match size {
            Some(size) => b.split2(addr + size),
            None => (b, ByteRecordRange::default())
        };

        (before, overlap, after)
    }
}
