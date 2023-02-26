use std::future;
use std::pin;
use std::sync;
use std::task;

use crate::util;

pub trait Change<Object>: Clone {
    type ApplyError;

    fn apply(self, old: &sync::Arc<Object>) -> Result<Object, Self::ApplyError>;
}

pub trait Versioned: Sized {
    type Change: Change<Self>;
    
    fn get_generation(&self) -> u64;
    fn get_uid(&self) -> u64;
    fn get_previous(&self) -> Option<&(sync::Arc<Self>, Self::Change)>;

    fn is_outdated(&self, other: &Self) -> bool {
        self.get_uid() != other.get_uid() ||
            self.get_generation() != other.get_generation()
    }
}

static NEXT_UID:        sync::atomic::AtomicU64 = sync::atomic::AtomicU64::new(1);
static NEXT_GENERATION: sync::atomic::AtomicU64 = sync::atomic::AtomicU64::new(1);

pub fn next_uid() -> u64 {
    NEXT_UID.fetch_add(1, sync::atomic::Ordering::Relaxed)
}

pub fn next_generation() -> u64 {
    NEXT_GENERATION.fetch_add(1, sync::atomic::Ordering::Relaxed)
}

pub struct Host<Object: Versioned> {
    notifier: util::Notifier,
    current: arc_swap::ArcSwap<Object>,
}

impl<Object: Versioned> Host<Object> {
    pub fn new(initial: Object) -> Self {
        Host {
            notifier: util::Notifier::new(),
            current: arc_swap::ArcSwap::from(sync::Arc::new(initial)),
        }
    }

    pub fn enroll(&self, cx: &task::Context) {
        self.notifier.enroll(cx);
    }

    pub fn wait_for_update<'a>(&'a self, current: &'_ Object) -> ObjectUpdateFuture<'a, Object> {
        ObjectUpdateFuture {
            host: self,
            generation: current.get_generation(),
        }
    }
    
    pub fn borrow(&self) -> arc_swap::Guard<sync::Arc<Object>> {
        self.current.load()
    }

    pub fn get(&self) -> sync::Arc<Object> {
        self.current.load_full()
    }

    pub fn change(&self, change: Object::Change) -> Result<sync::Arc<Object>, <<Object as Versioned>::Change as Change<Object>>::ApplyError> {
        let old = self.current.load();
        let new = sync::Arc::new(change.apply(&old)?);
        let swapped = self.current.compare_and_swap(&*old, new.clone());
            
        if !sync::Arc::ptr_eq(&*old, &swapped) {
            /* very sad, another thread updated the object. need to fish the change back out of our object and try again. */
            return self.change((*new).get_previous().unwrap().1.clone())
        }
        
        self.notifier.notify();
        Ok(new)
    }
}

pub struct ObjectUpdateFuture<'a, Object: Versioned> {
    host: &'a Host<Object>,
    generation: u64,
}

impl<'a, Object: Versioned> future::Future for ObjectUpdateFuture<'a, Object> {
    type Output = sync::Arc<Object>;

    fn poll(self: pin::Pin<&mut Self>, cx: &mut task::Context<'_>) -> task::Poll<Self::Output> {
        let guard = self.host.current.load();
        if guard.get_generation() != self.generation {
            /* fast path */
            task::Poll::Ready(arc_swap::Guard::into_inner(guard))
        } else {
            /* slow path. need to enroll for change notifications... */
            std::mem::drop(guard);
            self.host.enroll(cx);

            /* check whether the object was updated while we were enrolling */
            let guard = self.host.current.load();
            if guard.get_generation() != self.generation {
                /* object was updated while we were enrolling */
                task::Poll::Ready(arc_swap::Guard::into_inner(guard))
            } else {
                /* still no change... we'll pick it up when our task gets woken again. */
                task::Poll::Pending
            }
        }
    }
}

impl<Object: Versioned> std::fmt::Debug for Host<Object> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(std::any::type_name::<Host<Object>>())
            .field("generation", &self.borrow().get_generation())
            .finish_non_exhaustive()
    }
}
