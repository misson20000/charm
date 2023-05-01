use std::sync;
use std::vec;
use std::task;
use std::pin;
use std::string;
use std::io::Read;
use std::io::Seek;

use crate::model::space;
#[cfg(feature = "gtk")]
use crate::view::config;

struct Inner {
    file: std::fs::File,
}

pub struct FileAddressSpace {
    inner: sync::RwLock<Inner>,
    label: string::String,
    tokio_handle: tokio::runtime::Handle,
}

struct FetchFuture {
    delay: pin::Pin<Box<tokio::time::Sleep>>, /* pin projection is obnoxious and prevents us from mutating the other fields here */
    fas: sync::Arc<FileAddressSpace>,
    extent: (u64, u64),
}

impl FileAddressSpace {
    pub fn open(rt: tokio::runtime::Handle, path: &'_ std::path::PathBuf, label: &str) -> std::io::Result<FileAddressSpace> {
        Ok(FileAddressSpace {
            inner: sync::RwLock::<Inner>::new(Inner {
                file: std::fs::File::open(path)?,
            }),
            label: label.to_string(),
            tokio_handle: rt
        })
    }

    fn read_sync(&self, offset: u64, mut out: vec::Vec<u8>) -> space::FetchResult {
        let mut inner = self.inner.write().unwrap();
        (*inner).file.seek(std::io::SeekFrom::Start(offset))
            .and_then(|_| (*inner).file.read(&mut out[..]))
            .map(|r| {
                match r {
                    i if i == out.len() => space::FetchResult::Ok(out),
                    0 => space::FetchResult::Unreadable,
                    i => {
                        out.truncate(i);
                        space::FetchResult::Partial(out)
                    }
                }}).unwrap_or_else(space::FetchResult::IoError)
   }
}

impl futures::Future for FetchFuture {
    type Output = space::FetchResult;

    fn poll(mut self: pin::Pin<&mut Self>, cx: &mut task::Context<'_>) -> task::Poll<space::FetchResult> {
        match self.delay.as_mut().poll(cx) {
            task::Poll::Ready(_) => {
                let workaround = &mut *self;
                task::Poll::Ready(workaround.fas.read_sync(workaround.extent.0, vec![0; workaround.extent.1 as usize]))
            },
            task::Poll::Pending => task::Poll::Pending
        }
    }
}

#[cfg(feature = "gtk")]
fn get_file_access_delay() -> u64 {
    config::INSTANCE.borrow().file_access_delay
}

#[cfg(not(feature = "gtk"))]
fn get_file_access_delay() -> u64 {
    0
}

impl space::AddressSpace for FileAddressSpace {
    fn get_label(&self) -> &str {
        &self.label
    }
    
    fn fetch(self: sync::Arc<Self>, extent: (u64, u64)) -> pin::Pin<Box<dyn futures::Future<Output = space::FetchResult> + Send + Sync>> {
        let _guard = self.tokio_handle.enter();
        Box::pin(FetchFuture {
            delay: Box::pin(tokio::time::sleep(tokio::time::Duration::from_millis(get_file_access_delay()))),
            fas: self.clone(),
            extent,
        })
    }
}
