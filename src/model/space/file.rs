use std::sync;
use std::vec;
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

#[cfg(feature = "gtk")]
fn get_file_access_delay() -> u64 {
    config::INSTANCE.borrow().file_access_delay
}

#[cfg(not(feature = "gtk"))]
fn get_file_access_delay() -> u64 {
    0
}

impl space::AddressSpaceExt for FileAddressSpace {
    fn get_label(&self) -> &str {
        &self.label
    }
    
    async fn fetch(&self, extent: (u64, u64)) -> space::FetchResult {
        tokio::time::sleep(tokio::time::Duration::from_millis(get_file_access_delay())).await;

        self.read_sync(extent.0, vec![0; extent.1 as usize])
    }
}
