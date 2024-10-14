use std::iter;
use std::string;
use std::sync;
use std::vec;

use crate::model::space;
use crate::model::space::AddressSpaceExt;
use crate::util;

use bitflags::bitflags;

#[derive(Default, Copy, Clone)]
pub struct FetchFlags(u8);

bitflags! {
    impl FetchFlags: u8 {
        /* Somewhere along the line, data wasn't yet, so try again later and if you're lucky it will be in the cache next time. */
        const PENDING = 1;

        /* Touched by a LoadSpaceEdit */
        const LOADED = 2;

        /* Touched by an OverwriteEdit */
        const OVERWRITTEN = 4;

        /* Touched by an InsertEdit */
        const INSERTED = 8;

        /* Touched by a MoveEdit or the trailing end of an InsertEdit */
        const MOVED = 16;

        /* Some kind of error was encountered, probably an I/O error. */
        const ERROR = 32;
    }
}

#[derive(Default)]
pub struct FetchRequest<'a> {
    addr: u64,
    data: &'a mut [u8],
    flags: Option<&'a mut [FetchFlags]>,
}

pub struct FetchResult {
    /// The bitwise-OR accumulation of all the flags from the request
    flags: FetchFlags,
    /// How many bytes were processed. Advance your request by this much and try again if you didn't get everything.
    loaded: u64,
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

pub type DataPath = imbl::Vector<Filter>;

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
    
    async fn load_next<'a>(mut iter: impl iter::Iterator<Item = &'a Filter> + Clone, rq: FetchRequest<'_>) -> FetchResult {
        if let Some(filter) = iter.next() {
            filter.load(iter, rq).await
        } else {
            FetchResult {
                flags: FetchFlags::default(),
                loaded: rq.len(),
            }
        }
    }

    async fn load<'a>(&self, iter: impl iter::Iterator<Item = &'a Filter> + Clone, rq: FetchRequest<'_>) -> FetchResult {
        match self {
            Filter::LoadSpace(f) => f.load(iter, rq),
            Filter::Overwrite(_f) => todo!(),//f.load(iter, rq),
            Filter::Move(_f) => todo!(),//f.load(iter, rq),
            Filter::Insert(_f) => todo!(),//f.load(iter, rq),
        }.await
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

pub trait DataPathExt {
    async fn fetch(&self, rq: FetchRequest<'_>) -> FetchResult;
}

impl DataPathExt for DataPath {
    async fn fetch(&self, mut rq: FetchRequest<'_>) -> FetchResult {
        let sc = self.clone();
        std::mem::drop(self);
        
        rq.data.fill(0);
        if let Some(flags) = &mut rq.flags {
            flags.fill(FetchFlags::default());
        }

        Filter::load_next(sc.iter().rev(), rq).await
    }
}

impl<'a> FetchRequest<'a> {
    pub fn new(addr: u64, data: &'a mut [u8], flags: Option<&'a mut [FetchFlags]>) -> Self {
        if let Some(flags) = flags {
            assert_eq!(flags.len(), data.len());
        }
        
        Self {
            addr,
            data,
            flags,
        }
    }

    fn end(&self) -> u64 {
        self.addr + self.data.len() as u64
    }
    
    fn len(&self) -> u64 {
        self.data.len() as u64
    }
    
    fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    fn split2(&mut self, addr: u64) -> (FetchRequest<'_>, FetchRequest<'_>) {
        if self.addr >= addr {
            /* if we are entirely after the split point */
            (FetchRequest::default(), FetchRequest {
                addr: self.addr,
                data: self.data,
                flags: self.flags,
            })
        } else if self.addr < addr && self.addr + self.len() > addr {
            /* if we overlap the split point */
            let (ad, bd) = self.data.split_at_mut((addr - self.addr) as usize);
            let (af, bf) = match self.flags.map(|f| f.split_at_mut((addr - self.addr) as usize)) {
                Some((af, bf)) => (Some(af), Some(bf)),
                None => (None, None),
            };
            
            (Self {
                addr: self.addr,
                data: ad,
                flags: af,
            }, Self {
                addr,
                data: bd,
                flags: bf,
            })
        } else if self.addr < addr && self.addr + self.len() <= addr {
            /* if we are entirely before the split point */
            (Self {
                addr: self.addr,
                data: self.data,
                flags: self.flags,
            }, Self::default())
        } else {
            panic!("unreachable")
        }
    }

    fn split2_owning(self, addr: u64) -> (Self, Self) {
        if self.addr >= addr {
            /* if we are entirely after the split point */
            (Self::default(), Self {
                addr: self.addr,
                data: self.data,
                flags: self.flags,
            })
        } else if self.addr < addr && self.addr + self.len() > addr {
            /* if we overlap the split point */
            let (ad, bd) = self.data.split_at_mut((addr - self.addr) as usize);
            let (af, bf) = match self.flags.map(|f| f.split_at_mut((addr - self.addr) as usize)) {
                Some((af, bf)) => (Some(af), Some(bf)),
                None => (None, None),
            };
            
            (Self {
                addr: self.addr,
                data: ad,
                flags: af,
            }, Self {
                addr,
                data: bd,
                flags: bf,
            })
        } else if self.addr < addr && self.addr + self.len() <= addr {
            /* if we are entirely before the split point */
            (Self {
                addr: self.addr,
                data: self.data,
                flags: self.flags,
            }, Self::default())
        } else {
            panic!("unreachable")
        }
    }
    
    fn split3(self, addr: u64, size: Option<u64>) -> (FetchRequest<'a>, FetchRequest<'a>, FetchRequest<'a>) {
        let (before, b) = self.split2_owning(addr);
        let (overlap, after) = match size {
            Some(size) => b.split2_owning(addr + size),
            None => (b, Self::default())
        };

        (before, overlap, after)
    }

    /*
    fn slice(self, addr: u64, size: u64) -> RequestSlice {
        if self.addr < addr {
            if self.end() <= addr {
                RequestSlice::NonOverlapping(self)
            } else {
                let (ad, bd) = self.data.split_at_mut((addr - self.addr) as usize);
                let (af, bf) = match self.flags {
                    Some(flags) => flags.split_at_mut((addr - self.addr) as usize),
                    None => Default::default()
                };
                
                RequestSlice::OverlapsStart {
                    non_overlap: FetchRequest {
                        addr: addr,
                        data: ad,
                        flags: af,
                        flags_accumulated: self.flags_accumulated,
                    },
                    overlap: FetchRequest {
                        addr: addr,
                        data: bd,
                        flags: bf,
                        flags_accumulated: self.flags_accumulated,
                    }
                }
            }
        } else {
            if self.addr >= addr + size {
                RequestSlice::NonOverlapping(self)
            } else if self.end() <= addr + size {
                RequestSlice::Enclosed(self)
            } else {
                let (ad, bd) = self.data.split_at_mut((addr + end - self.addr) as usize);
                let (af, bf) = match self.flags {
                    Some(flags) => flags.split_at_mut((addr + end - self.addr) as usize),
                    None => Default::default()
                };
                
                RequestSlice::OverlapsEnd {
                    overlap: FetchRequest {
                        addr: addr,
                        data: ad,
                        flags: af,
                        flags_accumulated: self.flags_accumulated,
                    },
                    non_overlap: FetchRequest {
                        addr: addr,
                        data: bd,
                        flags: bf,
                        flags_accumulated: self.flags_accumulated,
                    }
                }
            }
        }
}
    */
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

    /*
    fn fetch<'a>(&self, iter: impl iter::Iterator<Item = &'a Filter> + Clone, range: &'_ mut ByteRecordRange<'_>, cx: &mut task::Context) {
        let (mut before, overlap, mut after) = range.split3((self.load_offset, self.size));

        if !before.is_empty() {
            Filter::fetch_next(iter.clone(), &mut before, cx);
        }

        if !after.is_empty() {
            Filter::fetch_next(iter, &mut after, cx);
        }

        if !overlap.is_empty() {
            let mut lru_guard = self.cache.lock();
            
            let mut current_block = None;

            // TODO: optimize this
            for (i, br) in overlap.out.iter_mut().enumerate() {
                let required_block_addr = ((overlap.addr + i as u64 - self.load_offset + self.space_offset) / self.cache.block_size) * self.cache.block_size;

                let block = match current_block {
                    Some((addr, block)) if addr == required_block_addr => block,
                    _ => self.cache.fetch_block_with_lock(required_block_addr, &mut lru_guard, cx),
                };
                
                br.loaded = true;
                
                match &*block {
                    space::cache::SpaceCacheEntry::Pending(_) => br.pending = true,
                    space::cache::SpaceCacheEntry::Finished(space::FetchResult::Ok(bytes)) => br.value = bytes[(self.convert_to_space(overlap.addr + i as u64) - required_block_addr) as usize],
                    space::cache::SpaceCacheEntry::Finished(space::FetchResult::Partial(bytes)) => match bytes.get((self.convert_to_space(overlap.addr + i as u64) - required_block_addr) as usize) {
                        Some(b) => br.value = *b,
                        None => br.error = true,
                    },
                    space::cache::SpaceCacheEntry::Finished(_) => br.error = true,
                }
                
                current_block = Some((required_block_addr, block));
            }
        }
}
    */

    async fn load<'a>(&self, iter: impl iter::Iterator<Item = &'a Filter> + Clone, rq: FetchRequest<'_>) -> FetchResult {
        let (before, overlap, after) = rq.split3(self.load_offset, self.size);
        if !before.is_empty() {
            return Filter::load_next(iter, before).await;
        }

        if !overlap.is_empty() {
            
        }

        return Filter::load_next(iter, after).await;
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
    /*
    fn fetch<'a, 'b, 'c>(&self, iter: impl iter::Iterator<Item = &'a Filter> + Clone, range: &'c mut ByteRecordRange<'b>, cx: &mut task::Context) {
        /* so we set load flags properly */
        Filter::fetch_next(iter, range, cx);

        // TODO: optimize this
        let addr = range.addr;
        
        for (i, br) in range.out.iter_mut().enumerate() {
            if addr + i as u64 >= self.offset && addr + i as u64 - self.offset < self.bytes.len() as u64 {
                br.overwritten = true;
                br.value = self.bytes[(addr + i as u64 - self.offset) as usize];
            }
        }
}
    */
    
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
    /*
    fn fetch<'a, 'b, 'c>(&self, _iter: impl iter::Iterator<Item = &'a Filter>, _range: &'c mut ByteRecordRange<'b>, _cx: &mut task::Context) {
        todo!("implement MoveFilter::fetch");
}
    */
    
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
