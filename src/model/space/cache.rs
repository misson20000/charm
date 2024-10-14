use std::future::Future;
use std::pin::Pin;
use std::sync;

use rental::rental;

use crate::model::space;
use crate::model::space::AddressSpaceExt;
use crate::util::PreemptableFuture;

rental! {
    mod rentals {
        use super::*;

        #[rental]
        pub struct SpaceCacheFutureHolder {
            space: sync::Arc<space::AddressSpace>,
            /* Ugh. Some day we will have a way to name futures. Until then... */
            future: Pin<Box<dyn Future<Output = space::FetchResult> + Send + Sync + 'space>>,
        }
    }
}

use rentals::SpaceCacheFutureHolder;

enum SpaceCacheEntryState {
    Pending(SpaceCacheFutureHolder),
    Finished(space::FetchResult),
}

struct SpaceCacheEntry {
    future: tokio::sync::Mutex<SpaceCacheFutureHolder>,

    /* If users is > 0, there must be a permit. */
    permit: parking_lot::Mutex<(usize, Option<tokio::sync::OwnedSemaphorePermit>)>,
    permit_notify: tokio::sync::Notify,
}

struct SpaceCacheEntryReference(sync::Arc<SpaceCacheEntry>);

pub struct SpaceCache {
    pub block_size: u64,
    pub block_count: std::num::NonZeroUsize,
    pub space: sync::Arc<space::AddressSpace>,
    
    lru: parking_lot::Mutex<lru::LruCache<u64, sync::Arc<SpaceCacheEntry>>>,
    /* Notifies whenever a new entry is added to lru so that other tasks can check if it's the one they wanted. */
    lru_update_notifier: sync::Arc<tokio::sync::Notify>,

    /* This semaphore keeps track of how many cache slots are either empty or hold entries that don't have any users and
     * can be kicked out. */
    availability: sync::Arc<tokio::sync::Semaphore>,
}

//pub struct SpaceCacheLock<'a>(parking_lot::MutexGuard<'a, lru::LruCache<u64, SpaceCacheEntry>>);

impl SpaceCache {
    pub fn new(space: sync::Arc<space::AddressSpace>, block_size: u64, block_count: std::num::NonZeroUsize) -> SpaceCache {
        SpaceCache {
            block_size,
            block_count: block_count,
            space,
            lru: parking_lot::Mutex::new(lru::LruCache::new(block_count)),
            lru_update_notifier: sync::Arc::new(tokio::sync::Notify::new()),
            availability: sync::Arc::new(tokio::sync::Semaphore::new(block_count.get())),
        }
    }

    async fn get_block(&self, addr: u64, permit_future: impl Future<Output = Result<tokio::sync::OwnedSemaphorePermit, tokio::sync::AcquireError>>) -> SpaceCacheEntryReference {
        assert!(addr % self.block_size == 0, "misaligned address");

        let mut lru_lock = self.lru.lock();
        match lru_lock.get(&addr) {
            /* This address was in the cache. If it has been forgotten about, it will need a new permit to be remembered. */
            Some(entry) => {
                return SpaceCacheEntryReference::new(entry.clone(), permit_future).await;
            },

            /* This address wasn't in the cache. */
            None => {
                std::mem::drop(lru_lock);

                match PreemptableFuture(permit_future, self.lru_update_notifier.notified()).await {
                    Ok((permit, _)) => {
                        let permit = permit.unwrap();
                        
                        /* Got a permit to make a new cache entry. */
                        let mut lru_lock = self.lru.lock();

                        /* Double check that the entry still isn't there. */
                        match lru_lock.get(&addr) {
                            Some(entry) => return SpaceCacheEntryReference::new_with_permit(entry.clone(), permit),
                            None => {},
                        }
                        
                        /* Make sure there's room. */
                        while lru_lock.len() == lru_lock.cap().get() {
                            /* Need to try to kick someone out. */
                            let (key, entry) = lru_lock.peek_lru().unwrap();

                            let has_users = entry.permit.lock().0 > 0;
                            std::mem::drop(entry);
                            let key = *key;
                            
                            if has_users {
                                /* Don't kick out entries that are still being used. */
                                lru_lock.promote(&key);

                                /* We know there has to be at least one entry that isn't being used because we
                                 * obtained a semaphore permit, so we can keep looping on this. */
                                continue;
                            }

                            lru_lock.pop(&key);
                        }

                        let future = SpaceCacheFutureHolder::new(self.space.clone(), |space| Box::pin(space.fetch((addr, self.block_size))));
                        let sce = sync::Arc::new(SpaceCacheEntry {
                            future: tokio::sync::Mutex::new(future),
                            permit: parking_lot::Mutex::new((1, Some(permit))),
                            permit_notify: tokio::sync::Notify::new(),
                        });
                        lru_lock.put(addr, sce.clone());
                        self.lru_update_notifier.notify_waiters();

                        return SpaceCacheEntryReference(sce);
                    },

                    Err((permit_future, _)) => {
                        /* It's possible another task asked for the same block and put it into the cache. Check and see if that happened. */
                        return self.get_block(addr, permit_future).await;
                    },
                }
            },
        }
    }
    
    pub async fn fetch_block(&self, addr: u64) {
        /* Get in line for acquiring a semaphore permit. We may or may not wind up actually needing it. */
        let permit_future = self.availability.clone().acquire_owned();
        
        let entry_ref = self.get_block(addr, permit_future).await;
        let mut future_holder = entry_ref.0.future.lock().await;
        let fr = future_holder.ref_rent_mut(|f| f).await;
        todo!();
    }
}

impl std::fmt::Debug for SpaceCache {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        Ok(())
    }
}

impl SpaceCacheEntryReference {
    async fn new(entry: sync::Arc<SpaceCacheEntry>, permit_future: impl std::future::Future<Output = Result<tokio::sync::OwnedSemaphorePermit, tokio::sync::AcquireError>>) -> Self {
        let mut lock = entry.permit.lock();

        if lock.0 > 0 {
            lock.0+= 1;
            std::mem::drop(lock);
            /* implicitly drop permit_future; this entry should already have a permit so we don't need to keep trying to acquire one. */
            return Self(entry);
        }

        /* need to either get a permit or be notified that someone else did it for us. */
        let notify_future = entry.permit_notify.notified();
        std::mem::drop(lock);

        match PreemptableFuture(permit_future, notify_future).await {
            Ok((permit, notify_future)) => {
                /* got a permit! */
                let mut lock = entry.permit.lock();
                lock.0+= 1;
                lock.1 = Some(permit.unwrap());
                entry.permit_notify.notify_waiters();
                std::mem::drop(lock);
                std::mem::drop(notify_future);
                Self(entry.clone())
            },
            Err((permit_future, _)) => {
                /* just try it all over again from the top */
                Self::new(entry.clone(), permit_future).await
            }
        }
    }

    fn new_with_permit(entry: sync::Arc<SpaceCacheEntry>, permit: tokio::sync::OwnedSemaphorePermit) -> Self {
        let mut lock = entry.permit.lock();

        lock.0+= 1;
        lock.1 = Some(permit);

        Self(entry)
    }    
}

impl Drop for SpaceCacheEntryReference {
    fn drop(&mut self) {
        let mut lock = self.0.permit.lock();

        match lock.0 {
            0 => panic!("SpaceCacheEntry reached 0 users while a reference still existed"),
            
            /* We were the last reference; release the permit to indicate to that something can be kicked out */
            1 => *lock = (0, None),
            
            n => lock.0 = n - 1,
        }
    }
}
