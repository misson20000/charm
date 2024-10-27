use std::vec;

use crate::model::datapath::DataPath;
use crate::model::datapath::fetch_filters;
use crate::model::datapath::FetchFlags;
use crate::model::datapath::FetchRequest;
use crate::model::datapath::FetchResult;
use crate::model::datapath::Filter;

use atomig::Atomic;
use atomig::Ordering;
use ouroboros::self_referencing;

#[self_referencing]
struct FetcherInterior {
    data: Vec<Atomic<u8>>,
    flags: Vec<Atomic<FetchFlags>>,

    #[borrows(data, flags)]
    #[not_covariant]
    future: Option<std::pin::Pin<Box<dyn std::future::Future<Output = (FetchResult, &'this [Atomic<u8>], &'this [Atomic<FetchFlags>], imbl::Vector<Filter>)> + 'this>>>,
}

pub struct Fetcher {
    addr: u64,
    progress: usize,
    size: usize,
    total_flags: FetchFlags,
    interior: FetcherInterior,
}

impl Fetcher {
    fn default_interior() -> FetcherInterior {
        FetcherInteriorBuilder {
            data: vec![],
            flags: vec![],
            future_builder: |_, _| None,
        }.build()
    }
    
    pub fn new(datapath: &DataPath, addr: u64, size: usize) -> Fetcher {
        let filters = datapath.filters.clone();

        let mut data = vec![];
        data.resize_with(size, || Atomic::new(0));
        let mut flags = vec![];
        flags.resize_with(size, || Atomic::new(FetchFlags::empty()));
        
        Fetcher {
            addr,
            progress: 0,
            size,
            total_flags: FetchFlags::empty(),
            interior: FetcherInteriorBuilder {
                data,
                flags,
                future_builder: move |data_ref, flags_ref| Self::make_future(addr, &data_ref[..], &flags_ref[..], filters),
            }.build()
        }
    }

    pub fn reset(&mut self, datapath: &DataPath, addr: u64, size: usize) {
        self.addr = addr;
        self.progress = 0;
        self.size = size;
        self.total_flags = FetchFlags::empty();

        let mut interior = std::mem::replace(&mut self.interior, Self::default_interior()).into_heads();
        interior.data.clear();
        interior.data.resize_with(size, || Atomic::new(0));
        interior.flags.clear();
        interior.flags.resize_with(size, || Atomic::new(FetchFlags::empty()));

        let filters = datapath.filters.clone();
        
        self.interior = FetcherInteriorBuilder {
            data: interior.data,
            flags: interior.flags,
            future_builder: move |data_ref, flags_ref| Self::make_future(addr, &data_ref[..], &flags_ref[..], filters),
        }.build();
    }

    pub fn data(&self) -> &[Atomic<u8>] {
        self.interior.borrow_data()
    }
    
    pub fn flags(&self) -> &[Atomic<FetchFlags>] {
        self.interior.borrow_flags()
    }

    pub fn finished(&self) -> bool {
        self.progress == self.size
    }

    pub fn byte(&self, offset: usize) -> Option<u8> {
        if self.flags()[offset].load(Ordering::Relaxed).intersects(FetchFlags::HAS_ANY_DATA) {
            /* need to synchronize with flag-setting using Release ordering */
            Some(self.data()[offset].load(Ordering::Acquire))
        } else {
            None
        }
    }

    pub fn byte_and_flags(&self, offset: usize) -> (u8, FetchFlags) {
        let flags = self.flags()[offset].load(Ordering::Relaxed);
        let data = self.data()[offset].load(Ordering::Acquire);
        
        (data, flags)
    }

    fn make_future<'data>(addr: u64, data: &'data [Atomic<u8>], flags: &'data [Atomic<FetchFlags>], filters: imbl::Vector<Filter>) -> Option<std::pin::Pin<Box<dyn std::future::Future<Output = (FetchResult, &'data [Atomic<u8>], &'data [Atomic<FetchFlags>], imbl::Vector<Filter>)> + 'data>>> {
        if data.len() > 0 {
            Some(Box::pin(async move {
                (fetch_filters(filters.clone(), FetchRequest::new(addr, &*data, Some(&*flags))).await, data, flags, filters)
            }))
        } else {
            None
        }
    }

    pub fn work(&mut self, cx: &mut std::task::Context) -> bool {
        let mut did_work = false;

        let addr = self.addr;
        let mut total_flags = self.total_flags;
        let mut progress = self.progress;
        
        while self.interior.with_future_mut(|f| match f {
            Some(future) => match future.as_mut().poll(cx) {
                std::task::Poll::Pending => false,
                std::task::Poll::Ready((fr, data, flags, filters)) => {
                    if fr.loaded == 0 {
                        panic!("fetch request finished with no bytes loaded");
                    }
                    
                    total_flags|= fr.flags;
                    progress+= fr.loaded;

                    let data = &data[fr.loaded..];
                    let flags = &flags[fr.loaded..];

                    did_work = true;
                    
                    if self.progress < self.size {
                        *f = Self::make_future(addr + progress as u64, data, flags, filters);
                        true
                    } else {
                        *f = None;
                        false
                    }
                }
            },
            None => false
        }) {
            /* spin */
        }

        self.addr = addr;
        self.total_flags = total_flags;
        self.progress = progress;

        did_work
    }
}

impl std::fmt::Debug for Fetcher {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Fetcher")
            .field("addr", &self.addr)
            .field("progress", &self.progress)
            .field("size", &self.size)
            .field("total_flags", &self.total_flags)
            .finish_non_exhaustive()
    }
}
