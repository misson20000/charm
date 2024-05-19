use std::future::Future;
use std::pin::Pin;
use std::sync;
use std::task;

use rental::rental;

use crate::model::space;
use crate::model::space::AddressSpaceExt;

rental! {
    mod rentals {
        use super::*;

        #[rental]
        pub struct SpaceCacheFuture {
            space: sync::Arc<space::AddressSpace>,
            /* Ugh. Some day we will have a way to name futures. Until then... */
            future: Pin<Box<dyn Future<Output = space::FetchResult> + Send + Sync + 'space>>,
        }
    }
}

use rentals::SpaceCacheFuture;

pub enum SpaceCacheEntry {
    Pending(SpaceCacheFuture),
    Finished(space::FetchResult),
}

pub struct SpaceCache {
    pub block_size: u64,
    pub block_count: std::num::NonZeroUsize,
    pub space: sync::Arc<space::AddressSpace>,
    /* can't use RwLock here; LruCache mutates on read */
    lru: parking_lot::Mutex<lru::LruCache<u64, SpaceCacheEntry>>,
}

pub struct SpaceCacheLock<'a>(parking_lot::MutexGuard<'a, lru::LruCache<u64, SpaceCacheEntry>>);

impl SpaceCache {
    pub fn new(space: sync::Arc<space::AddressSpace>, block_size: u64, block_count: std::num::NonZeroUsize) -> SpaceCache {
        SpaceCache {
            block_size,
            block_count: block_count,
            space,
            lru: parking_lot::Mutex::new(lru::LruCache::new(block_count)),
        }
    }
    
    pub fn fetch_block<'a>(&'a self, addr: u64, cx: &mut task::Context) -> parking_lot::MappedMutexGuard<'a, SpaceCacheEntry> {
        parking_lot::MutexGuard::map(self.lock().0, |lru| self.fetch_block_impl(addr, lru, cx))
    }

    pub fn lock(&self) -> SpaceCacheLock {
        SpaceCacheLock(self.lru.lock())
    }
    
    pub fn fetch_block_with_lock<'a, 'b>(&'a self, addr: u64, lru_guard: &'b mut SpaceCacheLock, cx: &mut task::Context) -> &'b mut SpaceCacheEntry {
        self.fetch_block_impl(addr, &mut *lru_guard.0, cx)
    }

    pub fn fetch_block_impl<'a, 'b>(&'a self, addr: u64, lru: &'b mut lru::LruCache<u64, SpaceCacheEntry>, cx: &mut task::Context) -> &'b mut SpaceCacheEntry {

        assert!(addr % self.block_size == 0, "misaligned address");

        if !lru.contains(&addr) {
            let future = SpaceCacheFuture::new(self.space.clone(), |space| Box::pin(space.fetch((addr, self.block_size))));
            let mut entry = SpaceCacheEntry::Pending(future);
            entry.poll(cx);
            
            lru.put(addr, entry);
        }
                
        lru.get_mut(&addr).unwrap()
    }

    pub fn poll_blocks(&self, cx: &mut task::Context) {
        let mut lru_guard = self.lru.lock();
        
        for (_, entry) in lru_guard.iter_mut() {
            entry.poll(cx);
        }
    }
}

impl SpaceCacheEntry {
    fn poll(&mut self, cx: &mut task::Context) {
        take_mut::take(self, |entry| match entry {
            SpaceCacheEntry::Pending(mut future) => match future.rent_mut(|f| f.as_mut().poll(cx)) {
                task::Poll::Ready(result) => SpaceCacheEntry::Finished(result),
                task::Poll::Pending => SpaceCacheEntry::Pending(future),
            },
            SpaceCacheEntry::Finished(result) => SpaceCacheEntry::Finished(result),
        });
    }

    fn is_finished(&self) -> bool {
        match self {
            SpaceCacheEntry::Pending(_) => false,
            SpaceCacheEntry::Finished(_) => true,
        }
    }
}

impl std::fmt::Debug for SpaceCache {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        Ok(())
    }
}
