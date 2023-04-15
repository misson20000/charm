use std::sync;

use crate::datapath::ByteRecordRange;
use crate::datapath::FetchResult;
use crate::datapath::cache::Cache;
use crate::datapath::space::AddressSpace;
use crate::datapath::Filter;

#[derive(Clone, Debug)]
pub struct LoadSpaceFilter {
    pub load_offset: u64,
    pub space_offset: u64,
    pub size: Option<u64>, /* None means unbounded */
    cache: sync::Arc<Cache>,
    label: String,
}

impl LoadSpaceFilter {
    pub fn new<Space: AddressSpace>(space: Space, load_offset: u64, space_offset: u64) -> LoadSpaceFilter {
        LoadSpaceFilter {
            load_offset,
            space_offset,
            size: None,
	    label: space.get_label().to_string(),
            cache: sync::Arc::new(Cache::new(space, 10, 10)),
        }
    }
    
    fn convert_to_space(&self, addr: u64) -> u64 {
        addr - self.load_offset + self.space_offset
    }

    fn convert_to_addr(&self, space: u64) -> u64 {
        space - self.space_offset + self.load_offset
    }

    pub async fn fetch(&self, _range: &mut ByteRecordRange<'_>) -> FetchResult {
	todo!();

	/*
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
                    _ => self.cache.fetch_block_with_guard(required_block_addr, &mut lru_guard, cx),
                };
                
                br.loaded = true;
                
                match &*block {
                    SpaceCacheEntry::Pending(_) => br.pending = true,
                    SpaceCacheEntry::Finished(space::FetchResult::Ok(bytes)) => br.value = bytes[(self.convert_to_space(overlap.addr + i as u64) - required_block_addr) as usize],
                    SpaceCacheEntry::Finished(space::FetchResult::Partial(bytes)) => match bytes.get((self.convert_to_space(overlap.addr + i as u64) - required_block_addr) as usize) {
                        Some(b) => br.value = *b,
                        None => br.error = true,
                    },
                    SpaceCacheEntry::Finished(_) => br.error = true,
                }
                
                current_block = Some((required_block_addr, block));
            }
    }
	 */
    }

    pub fn human_details(&self) -> String {
	self.label.clone()
    }

    pub fn human_affects_addr(&self) -> u64 {
        self.load_offset
    }

    pub fn human_affects_size(&self) -> Option<u64> {
        self.size
    }

    pub fn to_filter(self) -> Filter {
        Filter::LoadSpace(self)
    }
}
