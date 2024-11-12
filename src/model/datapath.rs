use std::string;
use std::sync;
use std::vec;

use crate::model::space;
use crate::model::space::AddressSpaceExt;
use crate::util;

use atomig::Atom;
use atomig::Atomic;
use atomig::AtomLogic;
use atomig::Ordering;
use bitflags::bitflags;

mod fetcher;

pub use fetcher::Fetcher;

#[derive(Default, Copy, Clone, Atom, AtomLogic, Debug)]
pub struct FetchFlags(u8);

bitflags! {
    impl FetchFlags: u8 {
        /* Touched by a LoadSpaceEdit */
        const LOADED = 1;

        /* Touched by an OverwriteEdit */
        const OVERWRITTEN = 2;

        /* Touched by an InsertEdit */
        const INSERTED = 4;

        /* Touched by a MoveEdit or the trailing end of an InsertEdit */
        const MOVED = 8;

        /* Encountered an I/O error while loading. */
        const IO_ERROR = 16;

        /* Hit the end of the file trying to load the requested data. */
        const EOF = 32;
        
        const HAS_ANY_DATA = 1 | 2 | 4;
        const HAS_DIRECT_EDIT = 2 | 4;
        const ERROR = 16 | 32;
    }
}

#[derive(Default)]
pub struct FetchRequest<'a> {
    addr: u64,
    data: &'a [Atomic<u8>],
    flags: Option<&'a [Atomic<FetchFlags>]>,
    ignore_edits: bool,
}

pub struct FetchResult {
    /// The bitwise-OR accumulation of all the flags from the request
    pub flags: FetchFlags,
    /// How many bytes were processed. Advance your request by this much and try again if you didn't get everything.
    pub loaded: usize,
}

type LoadFuture = std::pin::Pin<Box<dyn std::future::Future<Output = FetchResult>>>;

enum FilterFetchResult<'a> {
    Pass(FetchRequest<'a>),
    Done(FetchResult),
}

enum RequestSlice<'a> {
    NonOverlapping(FetchRequest<'a>),
    OverlapsStart {
        non_overlap: FetchRequest<'a>,
        overlap: FetchRequest<'a>,
    },
    Enclosed(FetchRequest<'a>),
    OverlapsEnd {
        non_overlap: FetchRequest<'a>,
        overlap: FetchRequest<'a>,
    },    
}

#[derive(Clone, Debug)]
pub enum Filter {
    LoadSpace(LoadSpaceFilter),
    Overwrite(OverwriteFilter),
    Move(MoveFilter),
    Insert(InsertFilter),
}

#[derive(Clone, Debug)]
pub struct DataPath {
    filters: imbl::Vector<Filter>,
}

impl Filter {
    pub fn stack(a: &Filter, b: &Filter) -> Option<Filter> {
        match (a, b) {
            (Filter::LoadSpace(_), Filter::LoadSpace(_)) => None,
            (Filter::Overwrite(ai), Filter::Overwrite(bi)) => OverwriteFilter::stack(ai, bi).map(Filter::Overwrite),
            (Filter::Move(ai), Filter::Move(bi)) => MoveFilter::stack(ai, bi).map(Filter::Move),
            (Filter::Insert(ai), Filter::Insert(bi)) => InsertFilter::stack(ai, bi).map(Filter::Insert),
            (Filter::Insert(ai), Filter::Overwrite(bi)) => InsertFilter::stack_overwrite(ai, bi).map(Filter::Insert),
            _ => None,
        }
    }
    
    async fn load<'a>(&self, rq: FetchRequest<'a>) -> FilterFetchResult<'a> {
        match self {
            Filter::LoadSpace(f) => f.load(rq).await,
            Filter::Overwrite(f) => f.load(rq),
            Filter::Move(_f) => todo!(),//f.load(iter, rq),
            Filter::Insert(_f) => todo!(),//f.load(iter, rq),
        }
    }
    
    pub fn human_details(&self) -> string::String {
        match self {
            Filter::LoadSpace(f) => f.human_details(),
            Filter::Overwrite(f) => f.human_details(),
            Filter::Move(f) => f.human_details(),
            Filter::Insert(f) => f.human_details(),
        }
    }

    pub fn human_affects_addr(&self) -> u64 {
        match self {
            Filter::LoadSpace(f) => f.human_affects_addr(),
            Filter::Overwrite(f) => f.human_affects_addr(),
            Filter::Move(f) => f.human_affects_addr(),
            Filter::Insert(f) => f.human_affects_addr(),
        }
    }

    pub fn human_affects_size(&self) -> Option<u64> {
        match self {
            Filter::LoadSpace(f) => f.human_affects_size(),
            Filter::Overwrite(f) => f.human_affects_size(),
            Filter::Move(f) => f.human_affects_size(),
            Filter::Insert(f) => f.human_affects_size(),
        }
    }
}

impl DataPath {
    pub fn new() -> Self {
        DataPath {
            filters: imbl::Vector::new(),
        }
    }

    pub fn from_filters(filters: imbl::Vector<Filter>) -> Self {
        DataPath {
            filters,
        }
    }

    pub fn stack(&mut self, filter: &Filter) {
        if let Some(top_filter) = self.filters.last() {
            if let Some(stacked_filter) = Filter::stack(top_filter, filter) {
                self.filters.set(self.filters.len() - 1, stacked_filter);
                
                return;
            }
        }
        
        self.filters.push_back(filter.clone());
    }

    pub fn push(&mut self, filter: Filter) {
        self.filters.push_back(filter);
    }

    pub fn iter_filters(&self) -> impl std::iter::DoubleEndedIterator<Item = &Filter> {
        self.filters.iter()
    }

    pub async fn fetch(&self, mut rq: FetchRequest<'_>) -> FetchResult {
        let filters = self.filters.clone();
        
        for filter in filters.iter().rev() {
            rq = match filter.load(std::mem::replace(&mut rq, FetchRequest::default())).await {
                FilterFetchResult::Pass(rq) => rq,
                FilterFetchResult::Done(rs) => return rs,
            }
        }
        
        FetchResult {
            flags: FetchFlags::default(),
            loaded: rq.len() as usize,
        }
    }

}
    
impl<'a> FetchRequest<'a> {
    pub fn new(addr: u64, data: &'a [Atomic<u8>], flags: Option<&'a [Atomic<FetchFlags>]>, ignore_edits: bool) -> Self {
        if let Some(flags) = flags.as_ref() {
            assert_eq!(flags.len(), data.len());
        }
        
        Self {
            addr,
            data,
            flags,
            ignore_edits,
        }
    }

    fn end(&self) -> u64 {
        self.addr + self.data.len() as u64
    }
    
    fn len(&self) -> usize {
        self.data.len()
    }
    
    fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    fn split2(self, addr: u64) -> (Self, Self) {
        if self.addr >= addr {
            /* if we are entirely after the split point */
            (Self::default(), Self {
                addr: self.addr,
                data: self.data,
                flags: self.flags,
                ignore_edits: self.ignore_edits,
            })
        } else if self.addr < addr && self.addr + self.len() as u64 > addr {
            /* if we overlap the split point */
            let (ad, bd) = self.data.split_at((addr - self.addr) as usize);
            let (af, bf) = match self.flags.map(|f| f.split_at((addr - self.addr) as usize)) {
                Some((af, bf)) => (Some(af), Some(bf)),
                None => (None, None),
            };
            
            (Self {
                addr: self.addr,
                data: ad,
                flags: af,
                ignore_edits: self.ignore_edits,
            }, Self {
                addr,
                data: bd,
                flags: bf,
                ignore_edits: self.ignore_edits,
            })
        } else if self.addr < addr && self.addr + self.len() as u64 <= addr {
            /* if we are entirely before the split point */
            (Self {
                addr: self.addr,
                data: self.data,
                flags: self.flags,
                ignore_edits: self.ignore_edits,
            }, Self::default())
        } else {
            panic!("unreachable")
        }
    }
    
    fn split3(self, addr: u64, size: Option<u64>) -> (FetchRequest<'a>, FetchRequest<'a>, FetchRequest<'a>) {
        let (before, b) = self.split2(addr);
        let (overlap, after) = match size {
            Some(size) => b.split2(addr + size),
            None => (b, Self::default())
        };

        (before, overlap, after)
    }

}

#[derive(Clone, Debug)]
pub struct LoadSpaceFilter {
    pub load_offset: u64,
    pub space_offset: u64,
    pub size: Option<u64>, /* None means unbounded */
    cache: sync::Arc<space::cache::SpaceCache>,
}

#[derive(Clone, Debug)]
pub struct OverwriteFilter {
    pub offset: u64,
    pub bytes: vec::Vec<u8>,
}

#[derive(Clone, Debug)]
pub struct MoveFilter {
    pub from: u64,
    pub to: u64,
    pub size: u64,
}

#[derive(Clone, Debug)]
pub struct InsertFilter {
    pub offset: u64,
    pub bytes: vec::Vec<u8>,
}

impl LoadSpaceFilter {
    pub fn new_defaults(space: sync::Arc<space::AddressSpace>, load_offset: u64, space_offset: u64) -> LoadSpaceFilter {
        // TODO: make this const when const unwrap gets stabilized
        //       https://github.com/rust-lang/rust/issues/67441
        //const block_count: std::num::NonZeroUsize = std::num::NonZeroUsize::new(1024).unwrap();
        let block_count = std::num::NonZeroUsize::new(1024).unwrap();
        
        LoadSpaceFilter {
            load_offset,
            space_offset,
            size: None,
            cache: sync::Arc::new(space::cache::SpaceCache::new(space, 0x1000, block_count)), // 4 MiB cache is plenty
        }
    }

    pub fn new_complete(space: sync::Arc<space::AddressSpace>, load_offset: u64, space_offset: u64, size: Option<u64>, cache_block_size: u64, cache_block_count: std::num::NonZeroUsize) -> LoadSpaceFilter {
        LoadSpaceFilter {
            load_offset,
            space_offset,
            size,
            cache: sync::Arc::new(space::cache::SpaceCache::new(space, cache_block_size, cache_block_count)),
        }
    }
    
    fn convert_to_space(&self, addr: u64) -> u64 {
        addr - self.load_offset + self.space_offset
    }

    fn convert_to_addr(&self, space: u64) -> u64 {
        space - self.space_offset + self.load_offset
    }

    async fn load<'a>(&self, rq: FetchRequest<'a>) -> FilterFetchResult<'a> {
        let (before, overlap, after) = rq.split3(self.load_offset, self.size);
        
        if !before.is_empty() {
            return FilterFetchResult::Pass(before);
        }

        if !overlap.is_empty() {
            let block_addr = (overlap.addr / self.cache.block_size) * self.cache.block_size;
            
            return self.cache.fetch_block(block_addr, |fr| match fr {
                space::FetchResult::Ok(v) | space::FetchResult::Partial(v) => {
                    let begin_index = (overlap.addr - block_addr) as usize;

                    if begin_index >= v.len() {
                        /* hit EOF */
                        return FilterFetchResult::Done(FetchResult {
                            flags: FetchFlags::EOF,
                            loaded: overlap.len()
                        });
                    }
                    
                    let loaded_count = std::cmp::min((overlap.end() - block_addr) as usize, v.len()) - begin_index;

                    for i in 0..loaded_count {
                        overlap.data[i].store(v[begin_index+i], Ordering::Relaxed);
                    }

                    if let Some(flags) = overlap.flags.as_ref() {
                        /* This guarantees that if another thread observes a LOADED flag, it will also observe our earlier write to the corresponding data field. */
                        sync::atomic::fence(Ordering::Release);

                        for f in &flags[0..loaded_count] {
                            f.fetch_or(FetchFlags::LOADED, Ordering::Acquire);
                        }
                    }

                    FilterFetchResult::Done(FetchResult {
                        flags: FetchFlags::LOADED,
                        loaded: loaded_count,
                    })
                },
                space::FetchResult::Unreadable | space::FetchResult::IoError(_) => {
                    if let Some(flags) = overlap.flags.as_ref() {
                        for f in flags.iter() {
                            f.fetch_or(FetchFlags::LOADED | FetchFlags::ERROR, Ordering::Acquire);
                        }
                    }

                    FilterFetchResult::Done(FetchResult {
                        flags: FetchFlags::LOADED | FetchFlags::ERROR,
                        loaded: overlap.len() as usize,
                    })
                },
            }).await;
        }

        FilterFetchResult::Pass(after)
    }
    
    fn human_details(&self) -> string::String {
        self.cache.space.get_label().to_string()
    }

    fn human_affects_addr(&self) -> u64 {
        self.load_offset
    }

    fn human_affects_size(&self) -> Option<u64> {
        self.size
    }
    
    pub fn to_filter(self) -> Filter {
        Filter::LoadSpace(self)
    }

    pub fn space(&self) -> &sync::Arc<space::AddressSpace> {
        &self.cache.space
    }

    pub fn cache_block_size(&self) -> u64 {
        self.cache.block_size
    }

    pub fn cache_block_count(&self) -> std::num::NonZeroUsize {
        self.cache.block_count
    }
}

impl OverwriteFilter {
    fn load<'a>(&self, rq: FetchRequest<'a>) -> FilterFetchResult<'a> {
        if rq.ignore_edits {
            return FilterFetchResult::Pass(rq);
        }
        
        let (before, overlap, after) = rq.split3(self.offset, Some(self.bytes.len() as u64));

        /* Process this immediately since callers can observe that this data loads instantly even if data before it takes a while to load asynchronously. */
        if !overlap.is_empty() {
            let offset = (overlap.addr - self.offset) as usize;
            
            for i in 0..(overlap.len() as usize) {
                overlap.data[i].store(self.bytes[i + offset], Ordering::Relaxed);
            }

            if let Some(flags) = overlap.flags.as_ref() {
                /* This guarantees that if another thread observes a LOADED flag, it will also observe our earlier write to the corresponding data field. */
                sync::atomic::fence(Ordering::Release);
                
                for f in flags.iter() {
                    f.fetch_or(FetchFlags::OVERWRITTEN, Ordering::Acquire);
                }
            }
        }

        if !before.is_empty() {
            return FilterFetchResult::Pass(before);
        }

        if !overlap.is_empty() {
            return FilterFetchResult::Done(FetchResult {
                flags: FetchFlags::OVERWRITTEN,
                loaded: overlap.len(),
            });
        }

        FilterFetchResult::Pass(after)
    }
        
    fn stack(a: &OverwriteFilter, b: &OverwriteFilter) -> Option<OverwriteFilter> {
        if b.offset == a.offset + a.bytes.len() as u64 {
            /* Stack if b immediately follows a */
            let mut bytes = vec::Vec::new();
            bytes.extend(a.bytes.iter());
            bytes.extend(b.bytes.iter());
            
            Some(OverwriteFilter {
                offset: a.offset,
                bytes,
            })
        } else if a.offset == b.offset + b.bytes.len() as u64 {
            /* Stack if a immediately follows b */
            let mut bytes = vec::Vec::new();
            bytes.extend(b.bytes.iter());
            bytes.extend(a.bytes.iter());

            Some(OverwriteFilter {
                offset: b.offset,
                bytes,
            })
        } else if b.offset >= a.offset && b.offset + b.bytes.len() as u64 <= a.offset + a.bytes.len() as u64 {
            /* Stack if b is contained entirely within a */
            let mut bytes = a.bytes.clone();

            for (i, byte) in bytes.iter_mut().enumerate() {
                if a.offset + i as u64 >= b.offset && a.offset + (i as u64) < b.offset + b.bytes.len() as u64 {
                    *byte = b.bytes[(a.offset + i as u64 - b.offset) as usize];
                }
            }

            Some(OverwriteFilter {
                offset: a.offset,
                bytes
            })
        } else {
            // TODO: stack some more complicated overlapping cases?
            None
        }
    }

    fn human_details(&self) -> string::String {
        util::fmt_hex_slice(&self.bytes).unwrap_or_else(|_| "error".to_string())
    }

    fn human_affects_addr(&self) -> u64 {
        self.offset
    }

    fn human_affects_size(&self) -> Option<u64> {
        Some(self.bytes.len() as u64)
    }
    
    pub fn to_filter(self) -> Filter {
        Filter::Overwrite(self)
    }
}

impl MoveFilter {
    fn stack(a: &MoveFilter, b: &MoveFilter) -> Option<MoveFilter> {
        if a.to == b.from && a.size == b.size {
            /* Stack if b's source is the same as a's destination */
            Some(MoveFilter {
                from: a.from,
                to: b.to,
                size: a.size,
            })
        } else {
            None
        }
    }


    fn human_details(&self) -> string::String {
        "todo".to_string()
    }

    fn human_affects_addr(&self) -> u64 {
        self.to
    }

    fn human_affects_size(&self) -> Option<u64> {
        Some(self.size)
    }
}

impl InsertFilter {
    /*
    fn fetch<'a, 'b, 'c>(&self, iter: impl iter::Iterator<Item = &'a Filter> + Clone, range: &'c mut ByteRecordRange<'b>, cx: &mut task::Context) {
        let (mut before, mut overlap, mut after) = range.split3((self.offset, Some(self.bytes.len() as u64)));

        if !before.is_empty() {
            Filter::fetch_next(iter.clone(), &mut before, cx);
        }

        if !after.is_empty() {
            after.addr-= self.bytes.len() as u64;
            Filter::fetch_next(iter.clone(), &mut after, cx);

            for br in after.out.iter_mut() {
                br.moved = true;
            }
        }

        if !overlap.is_empty() {
            /* so we set load flags properly */
            Filter::fetch_next(iter, &mut overlap, cx);
            
            // TODO: optimize this
            let addr = range.addr;
            
            for (i, br) in range.out.iter_mut().enumerate() {
                if addr + i as u64 >= self.offset && addr + i as u64 - self.offset < self.bytes.len() as u64 {
                    br.inserted = true;
                    br.value = self.bytes[(addr + i as u64 - self.offset) as usize];
                }
            }            
        }
}
    */

    fn stack(a: &InsertFilter, b: &InsertFilter) -> Option<InsertFilter> {
        if b.offset == a.offset + a.bytes.len() as u64 {
            /* Stack if b immediately follows a */
            let mut bytes = vec::Vec::new();
            bytes.extend(a.bytes.iter());
            bytes.extend(b.bytes.iter());

            Some(InsertFilter {
                offset: a.offset,
                bytes,
            })
        } else {
            None
        }
    }

    fn stack_overwrite(a: &InsertFilter, b: &OverwriteFilter) -> Option<InsertFilter> {
        if b.offset == a.offset + a.bytes.len() as u64 - 1 && b.bytes.len() == 1 {
            /* Stack if b overwrites the last byte of a */
            let mut filter = a.clone();
            filter.bytes[a.bytes.len() - 1] = b.bytes[0];
            Some(filter)
        } else {
            None
        }
    }

    fn human_details(&self) -> string::String {
        util::fmt_hex_slice(&self.bytes).unwrap_or_else(|_| "error".to_string())
    }

    fn human_affects_addr(&self) -> u64 {
        self.offset
    }

    fn human_affects_size(&self) -> Option<u64> {
        Some(self.bytes.len() as u64)
    }
    
    pub fn to_filter(self) -> Filter {
        Filter::Insert(self)
    }
}
