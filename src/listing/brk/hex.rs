use std::pin;
use std::sync;
use std::task;
use std::vec;
use std::future::Future;

use crate::addr;
use crate::space;
use crate::listing;
use crate::listing::BreakMapExt;
use crate::listing::line_group::LineGroup;
use crate::listing::window;
use crate::listing::brk;

#[derive(Debug)]
pub struct HexBreak {
    pub line_size: addr::Size,
}

pub struct HexBreakView<T: window::BreakViewDir> {
    header: bool,
    current_addr: addr::Address,
    extent: addr::Extent,
    hbrk: owning_ref::ArcRef<brk::Break, HexBreak>,
    _marker: std::marker::PhantomData<T>,
}

enum AsyncResult {
    Ok,
    Partial,
    Unreadable,
    IoError
}

enum AsyncState {
    Pending(pin::Pin<Box<dyn Future<Output = space::FetchResult> + Send + Sync>>),
    Finished(AsyncResult)
}

static NEXT_CACHE_ID: sync::atomic::AtomicU64 = sync::atomic::AtomicU64::new(0);

#[derive(Default, Debug, Clone, Copy)]
pub struct PatchByteRecord {
    pub value: u8,
    pub loaded: bool,
    pub patched: bool,
}

pub struct HexLineGroup {
    pub hbrk: owning_ref::ArcRef<brk::Break, HexBreak>,
    pub extent: addr::Extent,
    async_state: AsyncState,
    raw_bytes: vec::Vec<u8>,
    patched_bytes: vec::Vec<PatchByteRecord>,
    patches: Option<listing::PatchMap>,
    cache_id: u64,
}

impl<T: window::BreakViewDir> HexBreakView<T> {
    #[allow(unreachable_patterns)] // TODO: remove when we get more break types
    pub fn new_from_top(breaks: &listing::BreakMap, brk: sync::Arc<brk::Break>) -> HexBreakView<T> {
        HexBreakView {
            header: false,
            current_addr: brk.addr,
            extent: breaks.extents_at(brk.addr),
            hbrk: owning_ref::OwningRef::new(brk).map(|brk| match &brk.class {
                brk::BreakClass::Hex(hb) => hb,
                _ => panic!("constructing HexBreakView from non-Hex break"),
            }),
            _marker: std::marker::PhantomData
        }
    }

    #[allow(unreachable_patterns)] // TODO: remove when we get more break types
    pub fn new_from_middle(breaks: &listing::BreakMap, brk: sync::Arc<brk::Break>, current_addr: addr::Address) -> HexBreakView<T> {
        let extent = breaks.extents_at(brk.addr);
        let offset = current_addr - brk.addr;
        
        let hb = owning_ref::OwningRef::new(brk).map(|brk| match &brk.class {
            brk::BreakClass::Hex(hb) => hb,
            _ => panic!("constructing HexBreakView from non-Hex break"),
        });
                                                  
        HexBreakView {
            header: true,
            current_addr: extent.begin + hb.line_size * (offset / hb.line_size), /* round */
            extent,
            hbrk: hb,
            _marker: std::marker::PhantomData
        }
    }

    #[allow(unreachable_patterns)] // TODO: remove when we get more break types
    pub fn new_from_bottom(breaks: &listing::BreakMap, brk: sync::Arc<brk::Break>) -> HexBreakView<T> {
        let extent = breaks.extents_at(brk.addr);
        
        let hb = owning_ref::OwningRef::new(brk).map(|brk| match &brk.class {
            brk::BreakClass::Hex(hb) => hb,
            _ => panic!("constructing HexBreakView from non-Hex break"),
        });
                                                  
        HexBreakView {
            header: true,
            current_addr: extent.end,
            extent,
            hbrk: hb,
            _marker: std::marker::PhantomData
        }
    }

    fn offset(&self) -> addr::Size {
        self.current_addr - self.extent.begin
    }
}

impl window::BreakView for HexBreakView<window::UpDir> {
    /* current_addr is the address of the line we just generated */
    
    fn produce(&mut self, space: &sync::Arc<dyn space::AddressSpace>) -> Option<LineGroup> {
        if self.current_addr <= self.extent.begin {
            if !self.header {
                None
            } else {
                self.header = false;
                Some(LineGroup::BreakHeader(brk::BreakHeaderLineGroup::new(self.get_break())))
            }
        } else {
            let line_start_offset = self.hbrk.line_size * ((self.current_addr - addr::unit::BIT - self.extent.begin) / self.hbrk.line_size);
            let extent = addr::Extent::between(self.extent.begin + line_start_offset, self.current_addr);
            self.current_addr = self.extent.begin + line_start_offset;

            Some(LineGroup::Hex(HexLineGroup::new(&self.hbrk, space, extent)))
        }
    }
    
    fn trim(&mut self, lg: &LineGroup) -> bool {
        if self.current_addr >= self.extent.end {
            false
        } else {
            match lg {
                LineGroup::BreakHeader(bhdr) if !self.header => {
                    if !sync::Arc::ptr_eq(&bhdr.brk, self.get_break()) {
                        panic!("tried to trim break header for wrong break");
                    } else {
                        self.header = true;
                    }
                },
                LineGroup::Hex(hlg) => {
                    self.current_addr = hlg.extent.end;
                },
                _ => {
                    panic!("tried to trim invalid line group");
                }
            };
            true
        }
    }
    
    fn hit_boundary(&self) -> bool {
        self.current_addr <= self.extent.begin && self.header
    }
    
    fn get_break(&self) -> &sync::Arc<brk::Break> {
        self.hbrk.as_owner()
    }

    fn get_addr(&self) -> addr::Address {
        self.current_addr
    }
}

impl window::BreakView for HexBreakView<window::DownDir> {
    /* current_addr is the address of the line we're about to generate */
    
    fn produce(&mut self, space: &sync::Arc<dyn space::AddressSpace>) -> Option<LineGroup> {
        if self.current_addr <= self.extent.begin && !self.header {
            self.header = true;
            Some(LineGroup::BreakHeader(brk::BreakHeaderLineGroup::new(self.get_break())))
        } else if self.current_addr < self.extent.end {
            let line_size = std::cmp::min(self.hbrk.line_size, self.extent.end - self.current_addr);
            let extent = addr::Extent::between(self.current_addr, self.current_addr + line_size);
            self.current_addr+= line_size;

            Some(LineGroup::Hex(HexLineGroup::new(&self.hbrk, space, extent)))
        } else {
            None
        }
    }
    
    fn trim(&mut self, lg: &LineGroup) -> bool {
        if self.current_addr <= self.extent.begin && !self.header {
            false
        } else {
            match lg {
                LineGroup::BreakHeader(bhdr) if self.header => {
                    if !sync::Arc::ptr_eq(&bhdr.brk, self.get_break()) {
                        panic!("tried to trim break header for wrong break");
                    } else {
                        self.header = false;
                    }
                },
                LineGroup::Hex(hlg) => {
                    self.current_addr = hlg.extent.begin;
                },
                _ => {
                    panic!("tried to trim unexpected line group");
                }
            };
            true
        }
    }
    
    fn hit_boundary(&self) -> bool {
        self.current_addr >= self.extent.end
    }
    
    fn get_break(&self) -> &sync::Arc<brk::Break> {
        self.hbrk.as_owner()
    }

    fn get_addr(&self) -> addr::Address {
        self.current_addr
    }
}

impl HexLineGroup {
    fn new(hbrk: &owning_ref::ArcRef<brk::Break, HexBreak>, space: &sync::Arc<dyn space::AddressSpace>, extent: addr::Extent) -> HexLineGroup {
        HexLineGroup {
            hbrk: hbrk.clone(),
            async_state: AsyncState::Pending(
                Box::pin(space.clone().fetch(extent.round_out()))),
            extent,
            raw_bytes: vec::Vec::new(),
            patched_bytes: vec![PatchByteRecord::default(); extent.round_out().1 as usize],
            patches: None,
            cache_id: NEXT_CACHE_ID.fetch_add(1, sync::atomic::Ordering::SeqCst),
        }
    }
    
    pub fn num_lines(&self) -> usize {
        1
    }

    pub fn patch(&mut self, patches: &listing::PatchMap) -> bool {
        if self.patches.as_ref().map(|p| !p.ptr_eq(patches)).unwrap_or(true) {
            self.patches = Some(patches.clone());
            self.try_patch()
        } else {
            false
        }
    }

    fn try_patch(&mut self) -> bool {
        let (begin, length) = self.extent.round_out();

        if if let Some(patches) = &self.patches {
            if match patches.get_prev(&(begin + length)) {
                Some((&l, _)) => {
                    let mut patched = false;
                    let mut i = patches.range(l..).peekable();

                    loop {
                        if let Some((l, p)) = i.next() {
                            if l < &(begin + length) {
                                patched = true;
                                for i in 0..(length as usize) {
                                    let mut pbr = PatchByteRecord::default();
                                    if i < self.raw_bytes.len() {
                                        pbr.value = self.raw_bytes[i];
                                        pbr.loaded = true;
                                    }
                                    if &(begin + i as u64) >= l && ((begin + i as u64 - l) as usize) < p.bytes.len() {
                                        pbr.value = p.bytes[(begin + i as u64 - l) as usize];
                                        pbr.patched = true;
                                    }
                                    self.patched_bytes[i] = pbr;
                                }
                            } else { break }
                        } else { break }
                    }

                    patched
                },
                None => {
                    false
                }
            } {
                self.invalidate_cache();
                true
            } else {
                false
            }
        } else { false } {
            true
        } else {
            for i in 0..(length as usize) {
                self.patched_bytes[i] = if i < self.raw_bytes.len() {
                    PatchByteRecord {
                        value: self.raw_bytes[i],
                        loaded: true,
                        patched: false,
                    }
                } else {
                    PatchByteRecord {
                        value: 0,
                        loaded: false,
                        patched: false,
                    }
                }
            }
            false
        }
    }
    
    pub fn progress(&mut self, cx: &mut task::Context) -> bool {
        match &mut self.async_state {
            AsyncState::Pending(future) => {
                match future.as_mut().poll(cx) {
                    task::Poll::Ready(fetch_result) => {
                        let (data, async_result) = match fetch_result {
                            space::FetchResult::Ok(data) => (data, AsyncResult::Ok),
                            space::FetchResult::Partial(data) => (data, AsyncResult::Partial),
                            space::FetchResult::Unreadable => (vec![], AsyncResult::Unreadable),
                            space::FetchResult::IoError(_) => (vec![], AsyncResult::IoError),
                        };
                        self.async_state = AsyncState::Finished(async_result);
                        self.raw_bytes = data;
                        self.try_patch();
                        self.invalidate_cache();
                        true
                    },
                    task::Poll::Pending => false
                }
            },
            AsyncState::Finished(_) => false
        }
    }

    pub fn iter_bytes<'a>(&'a self) -> impl std::iter::Iterator<Item = u8> + 'a {
        HexLineIterator::new(self.raw_bytes.iter(), self.extent.begin.bit)
    }

    pub fn get_patched_bytes(&self, cache: &mut vec::Vec<PatchByteRecord>) {
        let shift = self.extent.begin.bit;
        let bytelen = self.extent.length().bytes as usize;

        cache.resize(self.patched_bytes.len(), PatchByteRecord::default());
        
        if shift == 0 {
            /* fast path */
            cache.copy_from_slice(&self.patched_bytes);

            if cache.len() > bytelen {
                /* need to mask off high bits that are outside our extent */
                cache[bytelen].value&= (1 << self.extent.length().bits) - 1;
            }
        } else {
            let mut last_loaded:bool = true;
            let mut last_patched:bool = false;
            let mut acc:u16 = 0;
        
            for (i, b) in self.patched_bytes.iter().enumerate().rev() {
                acc<<= 8;
                acc|= (b.value as u16) << 8 >> shift;
                
                if i < cache.len() {
                    if i >= self.extent.length().bytes as usize {
                        /* need to mask out high bits of last byte that are technically outside our extent */
                        cache[i].value = (acc >> 8) as u8 & ((1 << self.extent.length().bits) - 1);
                    } else {
                        cache[i].value = (acc >> 8) as u8;
                    }
                    cache[i].loaded = b.loaded && last_loaded;
                    cache[i].patched = b.patched || last_patched;
                }

                last_loaded = b.loaded;
                last_patched = b.patched;
            }
        }
    }

    pub fn get_break(&self) -> &sync::Arc<brk::Break> {
        self.hbrk.as_owner()
    }

    pub fn describe_state(&self) -> &'static str {
        match &self.async_state {
            AsyncState::Pending(_) => "Pending",
            AsyncState::Finished(ar) => match ar {
                AsyncResult::Ok => "Ok",
                AsyncResult::Partial => "Partial",
                AsyncResult::Unreadable => "Unreadable",
                AsyncResult::IoError => "IO Error",
            },
        }
    }

    pub fn get_cache_id(&self) -> Option<u64> {
        Some(self.cache_id)
    }

    fn invalidate_cache(&mut self) {
        self.cache_id = NEXT_CACHE_ID.fetch_add(1, sync::atomic::Ordering::SeqCst);
    }
}

struct HexLineIterator<'a> {
    iter: std::slice::Iter<'a, u8>,
    slip: Option<u8>,
    shift: u8,
}

impl<'a> HexLineIterator<'a> {    
    fn new(mut i: std::slice::Iter<'a, u8>, shift: u8) -> HexLineIterator<'a> {
        HexLineIterator {
            slip: i.next().map(|b| *b),
            iter: i,
            shift,
        }
    }
}

impl<'a> std::iter::Iterator for HexLineIterator<'a> {
    type Item = u8;

    /*   slip              next
     * +-----------------+-----------------+
     * | 0 1 2 3 4 5 6 7 | 0 1 2 3 4 5 6 7 |
     * +-----------------+-----------------+
     * | shift  |                  |
     */
    
    fn next(&mut self) -> Option<u8> {
        if self.shift == 0 {
            /* fast path */
            std::mem::replace(&mut self.slip, self.iter.next().map(|b| *b))
        } else {
            let next = self.iter.next().map(|b| *b);
            let slip = std::mem::replace(&mut self.slip, next);

            match (next, slip) {
                (Some(next), Some(slip)) => {
                    Some(slip >> self.shift | next << (8 - self.shift))
                },
                (None, Some(slip)) => {
                    Some(slip >> self.shift)
                },
                (None, None) => None,
                _ => panic!("invalid iterator state"),
            }
        }
    }
}

impl PartialEq for HexLineGroup {
    fn eq(&self, rhs: &Self) -> bool {
        std::ptr::eq(self.hbrk.as_ref(), rhs.hbrk.as_ref()) && self.extent == rhs.extent
    }
}

impl Eq for HexLineGroup {
}

impl std::fmt::Debug for HexLineGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HexLineGroup")
            .field("extent", &self.extent)
            .field("raw_bytes", &self.raw_bytes)
            .field("patched_bytes", &self.patched_bytes)
            .field("cache_id", &self.cache_id)
            .finish()
    }
}

impl<T: window::BreakViewDir> std::fmt::Debug for HexBreakView<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HexBreakView")
            .field("header", &self.header)
            .field("current_addr", &self.current_addr)
            .field("extent", &self.extent)
            .finish()
    }
}
