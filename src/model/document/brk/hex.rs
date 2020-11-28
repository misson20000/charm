use std::sync;
use std::task;
use std::vec;

use crate::model::addr;
use crate::model::datapath;
use crate::model::datapath::DataPathExt;
use crate::model::document;
use crate::model::document::brk;
use crate::model::document::BreakMapExt;
use crate::model::listing::window;
use crate::model::listing::line_group::LineGroup;

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

static NEXT_CACHE_ID: sync::atomic::AtomicU64 = sync::atomic::AtomicU64::new(0);

pub struct HexLineGroup {
    pub hbrk: owning_ref::ArcRef<brk::Break, HexBreak>,
    pub extent: addr::Extent,
    pub bytes: vec::Vec<datapath::ByteRecord>,
    document: document::Document,
    pending: bool,
    cache_id: u64,
}

impl<T: window::BreakViewDir> HexBreakView<T> {
    #[allow(unreachable_patterns)] // TODO: remove when we get more break types
    pub fn new_from_top(breaks: &document::BreakMap, brk: sync::Arc<brk::Break>) -> HexBreakView<T> {
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
    pub fn new_from_middle(breaks: &document::BreakMap, brk: sync::Arc<brk::Break>, current_addr: addr::Address) -> HexBreakView<T> {
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
    pub fn new_from_bottom(breaks: &document::BreakMap, brk: sync::Arc<brk::Break>) -> HexBreakView<T> {
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
    
    fn produce(&mut self) -> Option<LineGroup> {
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

            Some(LineGroup::Hex(HexLineGroup::new(&self.hbrk, extent)))
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
    
    fn produce(&mut self) -> Option<LineGroup> {
        if self.current_addr <= self.extent.begin && !self.header {
            self.header = true;
            Some(LineGroup::BreakHeader(brk::BreakHeaderLineGroup::new(self.get_break())))
        } else if self.current_addr < self.extent.end {
            let line_size = std::cmp::min(self.hbrk.line_size, self.extent.end - self.current_addr);
            let extent = addr::Extent::between(self.current_addr, self.current_addr + line_size);
            self.current_addr+= line_size;

            Some(LineGroup::Hex(HexLineGroup::new(&self.hbrk, extent)))
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
    fn new(hbrk: &owning_ref::ArcRef<brk::Break, HexBreak>, extent: addr::Extent) -> HexLineGroup {
        HexLineGroup {
            hbrk: hbrk.clone(),
            extent,
            bytes: vec![datapath::ByteRecord::default(); extent.round_out().1 as usize],
            document: document::Document::invalid(),
            pending: true,
            cache_id: NEXT_CACHE_ID.fetch_add(1, sync::atomic::Ordering::SeqCst),
        }
    }
    
    pub fn num_lines(&self) -> usize {
        1
    }

    pub fn update(&mut self, document: &document::Document, cx: &mut task::Context) -> bool {
        if self.document.is_datapath_outdated(document) {
            self.document = document.clone();
            self.pending = true;
        }

        if self.pending {
            self.document.datapath.fetch(datapath::ByteRecordRange::new(self.extent.round_out().0, &mut self.bytes), cx);

            self.pending = self.bytes.iter().any(|b| b.pending);

            self.invalidate_cache();
            
            true
        } else {
            false
        }
    }

    pub fn get_patched_bytes(&self, cache: &mut vec::Vec<datapath::ByteRecord>) {
        let shift = self.extent.begin.bit;
        let bytelen = self.extent.length().bytes as usize;

        cache.resize(self.bytes.len(), datapath::ByteRecord::default());
        
        if shift == 0 {
            /* fast path */
            cache.copy_from_slice(&self.bytes);

            if cache.len() > bytelen {
                /* need to mask off high bits that are outside our extent */
                cache[bytelen].value&= (1 << self.extent.length().bits) - 1;
            }
        } else {
            let mut last_br = datapath::ByteRecord {
                value: 0,
                
                pending: false,
                loaded: true,
                overwritten: false,
                inserted: false,
                moved: false,
                error: false
            };
            let mut acc:u16 = 0;
        
            for (i, br) in self.bytes.iter().enumerate().rev() {
                acc<<= 8;
                acc|= (br.value as u16) << 8 >> shift;
                
                if i < cache.len() {
                    if i >= self.extent.length().bytes as usize {
                        /* need to mask out high bits of last byte that are technically outside our extent */
                        cache[i].value = (acc >> 8) as u8 & ((1 << self.extent.length().bits) - 1);
                    } else {
                        cache[i].value = (acc >> 8) as u8;
                    }
                    cache[i].pending = br.pending || last_br.pending;
                    cache[i].loaded = br.loaded && last_br.loaded;
                    cache[i].overwritten = br.overwritten || last_br.overwritten;
                    cache[i].inserted = br.inserted || last_br.inserted;
                    cache[i].moved = br.moved || last_br.moved;
                    cache[i].error = br.error || last_br.error;
                }

                last_br = *br;
            }
        }
    }

    pub fn get_break(&self) -> &sync::Arc<brk::Break> {
        self.hbrk.as_owner()
    }

    pub fn describe_state(&self) -> &'static str {
        if self.pending {
            "Pending"
        } else {
            "Ok"
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
            .field("bytes", &self.bytes)
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
