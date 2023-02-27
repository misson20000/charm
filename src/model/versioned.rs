use std::future;
use std::pin;
use std::sync;
use std::task;

use crate::util;

pub trait Change<Object>: Clone {
    type ApplyError;
    type ApplyRecord: Clone;

    fn apply(self, object: &mut Object) -> Result<(Self, Self::ApplyRecord), Self::ApplyError>;
}

#[derive(Clone)]
pub struct Version<Object: Versioned> {
    previous: Option<(sync::Arc<Object>, <Object::Change as Change<Object>>::ApplyRecord)>,
    uid: u64,
    generation: u64
}

impl<Object: Versioned> std::fmt::Debug for Version<Object> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Version")
            .field("uid", &self.uid)
            .field("generation", &self.generation)
            .finish_non_exhaustive()
    }
}

impl<T: Versioned> Default for Version<T> {
    fn default() -> Self {
        Version {
            previous: None,
            uid: next_uid(),
            generation: next_generation(),
        }
    }
}

impl<T: Versioned> Version<T> {
    fn is_outdated(&self, other: &Self) -> bool {
        self.uid != other.uid ||
            self.generation != other.generation
    }
}

pub trait Versioned: Sized + Clone {
    type Change: Change<Self>;

    fn version(&self) -> &Version<Self>;
    fn version_mut(&mut self) -> &mut Version<Self>;

    fn generation(&self) -> u64 {
        self.version().generation
    }

    fn previous(&self) -> Option<&(sync::Arc<Self>, <Self::Change as Change<Self>>::ApplyRecord)> {
        self.version().previous.as_ref()
    }
    
    fn assert_same_uid(&self, other: &Self) {
        assert_eq!(self.version().uid, other.version().uid);
    }
    
    fn is_outdated(&self, other: &Self) -> bool {
        self.version().is_outdated(other.version())
    }

    fn change_for_debug(&mut self, change: Self::Change) -> Result<Self::Change, <Self::Change as Change<Self>>::ApplyError> {
        let old = sync::Arc::new(self.clone());
        let (change, record) = change.apply(self)?;

        let version = self.version_mut();
        version.previous = Some((old, record));
        version.generation = next_generation();

        Ok(change)
    }
}

static NEXT_UID:        sync::atomic::AtomicU64 = sync::atomic::AtomicU64::new(1);
static NEXT_GENERATION: sync::atomic::AtomicU64 = sync::atomic::AtomicU64::new(1);

fn next_uid() -> u64 {
    NEXT_UID.fetch_add(1, sync::atomic::Ordering::Relaxed)
}

fn next_generation() -> u64 {
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
            generation: current.generation(),
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
        let mut object = (**old).clone();
        let (change, record) = change.apply(&mut object)?;

        let version = object.version_mut();
        version.previous = Some((old.clone(), record));
        version.generation = next_generation();
        
        let new = sync::Arc::new(object);
        let swapped = self.current.compare_and_swap(&*old, new.clone());
            
        if !sync::Arc::ptr_eq(&*old, &swapped) {
            /* very sad, another thread updated the object. need to try again. */
            return self.change(change);
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
        if guard.generation() != self.generation {
            /* fast path */
            task::Poll::Ready(arc_swap::Guard::into_inner(guard))
        } else {
            /* slow path. need to enroll for change notifications... */
            std::mem::drop(guard);
            self.host.enroll(cx);

            /* check whether the object was updated while we were enrolling */
            let guard = self.host.current.load();
            if guard.generation() != self.generation {
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
            .field("generation", &self.borrow().generation())
            .finish_non_exhaustive()
    }
}
