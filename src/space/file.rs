use std::sync;
use std::vec;
use std::io::Read;
use std::io::Seek;

use crate::space;
use crate::addr;

struct Inner {
    file: std::fs::File,
}

pub struct FileAddressSpace {
    inner: sync::RwLock<Inner>,
}

struct FetchFuture {
    fas: sync::Arc<FileAddressSpace>,
    addr: addr::Address,
    size: addr::Size,
    out: vec::Vec<u8>,
}

impl FileAddressSpace {
    pub fn open(path: &'_ str) -> std::io::Result<FileAddressSpace> {
        Ok(FileAddressSpace {
            inner: sync::RwLock::<Inner>::new(Inner {
                file: std::fs::File::open(path)?,
            }),
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

    fn poll(mut self: std::pin::Pin<&mut Self>, _: &mut std::task::Context<'_>) -> std::task::Poll<space::FetchResult> {
        let workaround = &mut *self;
        std::task::Poll::Ready(workaround.fas.read_sync(workaround.addr, workaround.size, std::mem::replace(&mut workaround.out, vec::Vec::new())))
    }
}

impl space::AddressSpace for FileAddressSpace {
    fn fetch(self: sync::Arc<Self>, addr: addr::Address, size: addr::Size, out: vec::Vec<u8>) -> futures::future::BoxFuture<'static, space::FetchResult> {
        std::boxed::Box::pin(FetchFuture {
            fas: self.clone(),
            addr,
            size,
            out,
        })
    }
}
