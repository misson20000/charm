//! This module provides a cache to sit above each address space loaded into a
//! datapath. This is necessary in order to solve a few big problems:
//!
//!    - Token views and datapath filters both tend to favor generating many,
//!    many, small fetch requests to the datapath. Accesses to the underlying
//!    address space may have limited parallelism and high latency (imagine a
//!    gdbstub address space), meaning we'd prefer to coalesce many small
//!    accesses into fewer, larger accesses.
//!
//!    - We need to erase the type of the underlying address space in providing
//!    a Future for LoadSpaceFilter to use, and ideally it should be a fairly
//!    cheap future since we're going to have a lot of htem.
//!
//! Additionally, there are a few other design constraints:
//!
//!    - It needs to have an upper memory bound. Charm doesn't run in an
//!    embedded environment and we're not counting every byte we allocate, but
//!    datapath is somewhere where it's easy to waste huge amounts of memory and
//!    crashing due to OOM is bad.

use std::future::Future;
use std::pin::Pin;
use std::pin::pin;
use std::sync;

use crate::model::space;
use crate::util::PreemptableFuture;

enum CacheLineState {
    Pending(Pin<Box<dyn Future<Output = space::FetchResult> + Send + Sync>>),
    Finished(space::FetchResult),
}

struct CacheLine {
    state: tokio::sync::Mutex<CacheLineState>,

    /* If users is > 0, there must be a permit. */
    rememberance: parking_lot::Mutex<(usize, Option<tokio::sync::OwnedSemaphorePermit>)>,
    rememberance_notify: tokio::sync::Notify,
}

struct CacheLineReference(sync::Arc<CacheLine>);

pub struct SpaceCache {
    pub block_size: u64,
    pub block_count: std::num::NonZeroUsize,
    pub space: sync::Arc<space::AddressSpace>,

    /// Notifies whenever a new entry is added to lru so that other tasks can
    /// check if it's the one they wanted.
    lru_update_notifier: tokio::sync::Notify,
    lru: parking_lot::Mutex<lru::LruCache<u64, sync::Arc<CacheLine>>>,

    /// A permit from this semaphore means that a cache line is being awaited upon by somebody and should *not* be
    /// forgotten. This has the same number of permits as the number of blocks in the cache. 
    rememberance: sync::Arc<tokio::sync::Semaphore>,
}

impl SpaceCache {
    pub fn new(space: sync::Arc<space::AddressSpace>, block_size: u64, block_count: std::num::NonZeroUsize) -> SpaceCache {
        SpaceCache {
            block_size,
            block_count: block_count,
            space,
            lru_update_notifier: tokio::sync::Notify::new(),
            lru: parking_lot::Mutex::new(lru::LruCache::new(block_count)),
            rememberance: sync::Arc::new(tokio::sync::Semaphore::new(block_count.get())),
        }
    }

    async fn get_line(&self, addr: u64) -> CacheLineReference {
        assert!(addr % self.block_size == 0, "misaligned address");

        /* Get in line for acquiring a semaphore permit. We may or may not wind up actually needing it. */
        let mut permit_future_pin = pin!(self.rememberance.clone().acquire_owned());
        
        loop {
            let mut lru_lock = self.lru.lock();
            if let Some(entry) = lru_lock.get(&addr).cloned() {
                std::mem::drop(lru_lock);
                /* This address was in the cache. If it has been forgotten about, it will need a new permit to be remembered. Let the CacheLineReference constructor take care of awaiting the future to get a rememberance permit, if it needs one. */
                return CacheLineReference::new(entry, permit_future_pin).await;
            }
            std::mem::drop(lru_lock);
            
            /* This address wasn't in the cache need to either get a rememberance permit to create a new CacheLine
            object, or luck out and have some other task do it for the same block in the meantime. */
            
            match PreemptableFuture(permit_future_pin.as_mut(), pin!(self.lru_update_notifier.notified())).await {
                /* Got a rememberance permit! */
                Ok(permit) => {
                    let permit = permit.unwrap();

                    let mut lru_lock = self.lru.lock();

                    /* Double check that the entry still isn't there. */
                    match lru_lock.get(&addr) {
                        /* Entry was there, but it could've been forgotten. Give the permit to the
                         * CacheLineReference constructor in case it needs it to mark the line as being
                         * remembered. If it doesn't need it, it'll drop it. */
                        Some(entry) => return CacheLineReference::new_with_permit(entry.clone(), permit),
                        None => {},
                    }
                    
                    /* Kick out a forgotten cache line if necessary. */
                    while lru_lock.len() == lru_lock.cap().get() {
                        /* Need to try to kick someone out. */
                        let (key, entry) = lru_lock.peek_lru().unwrap();

                        let being_remembered = entry.rememberance.lock().0 > 0;
                        let key = *key;
                        
                        if being_remembered {
                            /* Don't kick out lines that are still being used. */
                            lru_lock.promote(&key);

                            /* We know there has to be at least one entry that isn't being remembered because we
                             * obtained a rememberance permit, so we can keep looping on this. */
                            
                            continue;
                        }

                        lru_lock.pop(&key);
                    }

                    /* Construct a new cache line. */
                    let space = self.space.clone();
                    let future = Box::pin(space.fetch_owned((addr, self.block_size)));
                    
                    let line = sync::Arc::new(CacheLine {
                        state: tokio::sync::Mutex::new(CacheLineState::Pending(future)),
                        rememberance: parking_lot::Mutex::new((1, Some(permit))),
                        rememberance_notify: tokio::sync::Notify::new(),
                    });

                    /* Put it in the LRU cache and notify other tasks that might be waiting on a permit to
                     * create this same cache line. */
                    lru_lock.put(addr, line.clone());
                    self.lru_update_notifier.notify_waiters();

                    return CacheLineReference(line);
                },

                Err(_notification) => {
                    /* It's possible another task asked for the same block and put it into the cache. Start over
                     * again and check if that happened. If it didn't, we're still holding onto our place in
                     * line to get a rememberance permit and will get it eventually. */
                    
                    continue;
                }
            }
        }
    }
    
    pub async fn fetch_block<R, F: FnOnce(&space::FetchResult) -> R>(&self, addr: u64, cb: F) -> R {
        let line_ref = self.get_line(addr).await;
        let mut state_lock = line_ref.0.state.lock().await;
        
        match &mut *state_lock {
            CacheLineState::Pending(f) => {
                let fr = f.as_mut().await;
                let r = cb(&fr);
                *state_lock = CacheLineState::Finished(fr);
                r
            },
            CacheLineState::Finished(fr) => cb(fr),
        }
    }
}

impl std::fmt::Debug for SpaceCache {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        Ok(())
    }
}

impl CacheLineReference {
    async fn new(entry: sync::Arc<CacheLine>, mut permit_future_pin: Pin<&mut impl std::future::Future<Output = Result<tokio::sync::OwnedSemaphorePermit, tokio::sync::AcquireError>>>) -> Self {
        loop {
            let mut lock = entry.rememberance.lock();

            if lock.0 > 0 {
                /* The line is already being remembered. */
                lock.0+= 1;
                std::mem::drop(lock);
                /* implicitly drop permit_future; this entry should already have a permit so we don't need to keep trying to acquire one. */
                return Self(entry);
            }

            /* need to either get a rememberance permit or be notified that someone else did it for us. */
            let notify_future = entry.rememberance_notify.notified();
            std::mem::drop(lock);

            match PreemptableFuture(permit_future_pin.as_mut(), pin!(notify_future)).await {
                Ok(permit) => {
                    /* got a permit! */
                    let mut lock = entry.rememberance.lock();

                    /* It's possible that while we were locking the mutex, someone else gave it a permit. That's fine,
                     * we don't even need to bother checking. We'll just clobber over it. If it existed, it'll get
                     * returned to the semaphore just the same as if we dropped ours instead. */
                    
                    lock.0+= 1;
                    lock.1 = Some(permit.unwrap());

                    /* Notify other tasks that might also be waiting here on a rememberance permit for this line. */
                    entry.rememberance_notify.notify_waiters();
                    
                    std::mem::drop(lock);
                    return Self(entry.clone());
                },
                Err(_notification) => {
                    /* Another task notified that it might've given this line a rememberance permit and we can give
                     * up. */
                    continue
                }
            }
        }
    }

    fn new_with_permit(entry: sync::Arc<CacheLine>, permit: tokio::sync::OwnedSemaphorePermit) -> Self {
        let mut lock = entry.rememberance.lock();

        lock.0+= 1;
        lock.1 = Some(permit);

        std::mem::drop(lock);
        
        Self(entry)
    }    
}

impl Drop for CacheLineReference {
    fn drop(&mut self) {
        let mut lock = self.0.rememberance.lock();

        match lock.0 {
            0 => panic!("CacheLine reached 0 users while a reference still existed"),
            
            /* We were the last reference; put this line in forgotten state and return the rememberance permit. */
            1 => *lock = (0, None),
            
            n => lock.0 = n - 1,
        }
    }
}
