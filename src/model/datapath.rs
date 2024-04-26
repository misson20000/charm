use std::iter;
use std::string;
use std::sync;
use std::task;
use std::vec;

use crate::model::space;
use crate::model::space::AddressSpaceExt;
use crate::util;

extern crate imbl;
extern crate lru;
extern crate take_mut;

#[derive(Default, Debug, Clone, Copy)]
pub struct ByteRecord {
    pub value: u8,
    
    pub pending: bool, /* Somewhere along the line, data wasn't yet, so try again later and if you're lucky it will be in the cache next time. */
    pub loaded: bool, /* Touched by a LoadSpaceEdit */
    pub overwritten: bool, /* Touched by an OverwriteEdit */
    pub inserted: bool, /* Touched by an InsertEdit */
    pub moved: bool, /* Touched by a MoveEdit or trailing end of an InsertEdit */
    pub error: bool, /* Some kind of error was encountered on the datapath. Probably an I/O error. */
}

impl ByteRecord {
    pub fn get_loaded(&self) -> Option<u8> {
        if self.loaded {
            Some(self.value)
        } else {
            None
        }
    }

    pub fn has_any_value(&self) -> bool {
        self.loaded || self.overwritten || self.inserted
    }

    pub fn has_direct_edit(&self) -> bool {
        self.overwritten || self.inserted
    }
}

#[derive(Default)]
pub struct ByteRecordRange<'a> {
    addr: u64,
    out: &'a mut [ByteRecord]
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
    
    fn fetch_next<'a, 'b, 'c>(mut iter: impl iter::Iterator<Item = &'a Filter> + Clone, range: &'c mut ByteRecordRange<'b>, cx: &mut task::Context) {
        if let Some(filter) = iter.next() {
            filter.fetch(iter, range, cx)
        }
    }

    /* Before you enter a fetch chain, you should clear the result array. */
    fn fetch<'a, 'b, 'c>(&self, iter: impl iter::Iterator<Item = &'a Filter> + Clone, range: &'c mut ByteRecordRange<'b>, cx: &mut task::Context) {
        match self {
            Filter::LoadSpace(f) => f.fetch(iter, range, cx),
            Filter::Overwrite(f) => f.fetch(iter, range, cx),
            Filter::Move(f) => f.fetch(iter, range, cx),
            Filter::Insert(f) => f.fetch(iter, range, cx),
        }
    }

    pub fn fetch_chain<'a>(iter: impl iter::Iterator<Item = &'a Filter> + Clone, mut range: ByteRecordRange, cx: &mut task::Context) {
        range.out.fill(ByteRecord::default());
        Filter::fetch_next(iter, &mut range, cx)
    }

    fn poll(&self, cx: &mut task::Context) {
        match self {
            Filter::LoadSpace(f) => f.poll(cx),
            _ => ()
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

pub trait DataPathExt {
    fn poll(&self, cx: &mut task::Context);
    fn fetch(&self, range: ByteRecordRange, cx: &mut task::Context);
}

impl DataPathExt for DataPath {
    fn poll(&self, cx: &mut task::Context) {
        for filter in self.iter() {
            filter.poll(cx);
        }
    }
    
    fn fetch(&self, range: ByteRecordRange, cx: &mut task::Context) {
        Filter::fetch_chain(self.iter().rev(), range, cx)
    }
}

impl<'a> ByteRecordRange<'a> {
    pub fn new(addr: u64, records: &'a mut [ByteRecord]) -> ByteRecordRange {
        ByteRecordRange {
            addr,
            out: records,
        }
    }
    
    fn is_empty(&self) -> bool {
        self.out.is_empty()
    }

    fn split2(&mut self, addr: u64) -> (ByteRecordRange<'_>, ByteRecordRange<'_>) {
        if self.addr >= addr {
            /* if we are entirely after the split point */
            (ByteRecordRange::default(), ByteRecordRange {
                addr: self.addr,
                out: self.out,
            })
        } else if self.addr < addr && self.addr + self.out.len() as u64 > addr {
            /* if we overlap the split point */
            let (a, b) = self.out.split_at_mut((addr - self.addr) as usize);
            
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

    fn split2_owning(self, addr: u64) -> (ByteRecordRange<'a>, ByteRecordRange<'a>) {
        if self.addr >= addr {
            /* if we are entirely after the split point */
            (ByteRecordRange::default(), ByteRecordRange {
                addr: self.addr,
                out: self.out,
            })
        } else if self.addr < addr && self.addr + self.out.len() as u64 > addr {
            /* if we overlap the split point */
            let (a, b) = self.out.split_at_mut((addr - self.addr) as usize);
            
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
    
    fn split3(&mut self, (addr, size): (u64, Option<u64>)) -> (ByteRecordRange<'_>, ByteRecordRange<'_>, ByteRecordRange<'_>) {
        let (before, b) = self.split2(addr);
        let (overlap, after) = match size {
            Some(size) => b.split2_owning(addr + size),
            None => (b, ByteRecordRange::default())
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
    
    fn fetch<'a, 'b, 'c>(&self, iter: impl iter::Iterator<Item = &'a Filter> + Clone, range: &'c mut ByteRecordRange<'b>, cx: &mut task::Context) {
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

    fn human_details(&self) -> string::String {
        self.cache.space.get_label().to_string()
    }

    fn human_affects_addr(&self) -> u64 {
        self.load_offset
    }

    fn human_affects_size(&self) -> Option<u64> {
        self.size
    }
    
    fn poll(&self, cx: &mut task::Context) {
        self.cache.poll_blocks(cx);
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
    fn fetch<'a, 'b, 'c>(&self, _iter: impl iter::Iterator<Item = &'a Filter>, _range: &'c mut ByteRecordRange<'b>, _cx: &mut task::Context) {
        todo!("implement MoveFilter::fetch");
    }
    
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
