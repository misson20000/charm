use std::sync;
use std::vec;
use std::task;
use std::pin;
use std::io::Read;
use std::io::Seek;

use crate::space;
use crate::addr;

struct Inner {
    file: std::fs::File,
}

pub struct FileAddressSpace {
    inner: sync::RwLock<Inner>,
    tokio_handle: tokio::runtime::Handle,
}

struct FetchFuture {
    delay: pin::Pin<Box<tokio::time::Delay>>, // pin projection is obnoxious and prevents us from mutating the other fields here
    fas: sync::Arc<FileAddressSpace>,
    addr: addr::Address,
    size: addr::Size,
    out: vec::Vec<u8>,
}

impl FileAddressSpace {
    pub fn open(rt: tokio::runtime::Handle, path: &'_ str) -> std::io::Result<FileAddressSpace> {
        Ok(FileAddressSpace {
            inner: sync::RwLock::<Inner>::new(Inner {
                file: std::fs::File::open(path)?,
            }),
            tokio_handle: rt
        })
    }

    fn read_sync(&self, addr: addr::Address, size: addr::Size, mut out: vec::Vec<u8>) -> space::FetchResult {
        let (offset, length) = addr::round_span(addr, size);
        let mut inner = self.inner.write().unwrap();
        (*inner).file.seek(std::io::SeekFrom::Start(offset))
            .and_then(|_| (*inner).file.read(&mut out[..(length as usize)]))
            .map(|r| {
                addr::bitshift_span(&mut out[..], addr.bit);
                match r {
                    i if i == (length as usize) => space::FetchResult::Ok(out),
                    0 => space::FetchResult::Unreadable,
                    i => space::FetchResult::Partial(out, i)
                }}).unwrap_or_else(|e| space::FetchResult::IoError(e))
   }
}

impl futures::Future for FetchFuture {
    type Output = space::FetchResult;

    fn poll(mut self: pin::Pin<&mut Self>, cx: &mut task::Context<'_>) -> task::Poll<space::FetchResult> {
        match self.delay.as_mut().poll(cx) {
            task::Poll::Ready(_) => {
                let workaround = &mut *self;
                task::Poll::Ready(workaround.fas.read_sync(workaround.addr, workaround.size, std::mem::replace(&mut workaround.out, vec::Vec::new())))
            },
            task::Poll::Pending => task::Poll::Pending
        }
    }
}

impl space::AddressSpace for FileAddressSpace {
    fn fetch(self: sync::Arc<Self>, addr: addr::Address, size: addr::Size, out: vec::Vec<u8>) -> pin::Pin<Box<dyn futures::Future<Output = space::FetchResult> + Send + Sync>> {
        self.tokio_handle.enter(|| {
            Box::pin(FetchFuture {
                delay: Box::pin(tokio::time::delay_for(tokio::time::Duration::new(1, 0))),
                fas: self.clone(),
                addr,
                size,
                out,
            })
        })
    }
}
