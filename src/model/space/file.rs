use std::string;
use std::sync;
use std::time;
use std::vec;
use std::io::Read;
use std::io::Seek;

use crate::model::space;
#[cfg(feature = "gtk")]
use crate::view::config;

enum State {
    Open(std::fs::File),
    Closed,
    Error(std::io::Error),
}

pub struct FileAddressSpace {
    inner: sync::RwLock<State>,
    pub path: std::path::PathBuf,
    pub label: string::String,
}

impl FileAddressSpace {
    pub fn new(path: std::path::PathBuf, label: &str) -> FileAddressSpace {
        FileAddressSpace {
            inner: sync::RwLock::<State>::new(State::Closed),
            path,
            label: label.to_string(),
        }
    }

    /// In case of error, the error is returned instead of being stored in this object as an error state.
    pub fn try_open(&self) -> Result<&Self, std::io::Error> {
        let mut guard = self.inner.write().unwrap();

        match &*guard {
            State::Open(_) => Ok(self),
            State::Closed | State::Error(_) => match std::fs::File::open(&self.path) {
                Ok(f) => { *guard = State::Open(f); Ok(self) },
                Err(e) => Err(e),
            },
        }
    }

    /// In case of error, the error is stored in this object as an error state instead of being returned.
    pub fn open(&self) {
        let mut guard = self.inner.write().unwrap();

        match &*guard {
            State::Open(_) => {},
            State::Closed | State::Error(_) => *guard = match std::fs::File::open(&self.path) {
                Ok(f) => State::Open(f),
                Err(e) => State::Error(e),
            },
        }
    }
    
    fn read_sync(&self, offset: u64, mut out: vec::Vec<u8>) -> space::FetchResult {
        let mut inner = self.inner.write().unwrap();

        match &mut *inner {
            State::Open(file) => file.seek(std::io::SeekFrom::Start(offset))
                .and_then(|_| file.read(&mut out[..]))
                .map(|r| {
                    match r {
                        i if i == out.len() => space::FetchResult::Ok(out),
                        0 => space::FetchResult::Unreadable,
                        i => {
                            out.truncate(i);
                            space::FetchResult::Partial(out)
                        }
                    }}).unwrap_or_else(space::FetchResult::IoError),
            
            _ => space::FetchResult::Unreadable,
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

impl space::AddressSpaceExt for FileAddressSpace {
    fn get_label(&self) -> &str {
        &self.label
    }
    
    async fn fetch(&self, extent: (u64, u64)) -> space::FetchResult {
        tokio::time::sleep(tokio::time::Duration::from_millis(get_file_access_delay())).await;

        self.read_sync(extent.0, vec![0; extent.1 as usize])
    }

    fn is_dirty_since(&self, timestamp: time::SystemTime) -> bool {
        let Ok(meta) = std::fs::metadata(&self.path) else { return false };

        if let Ok(modified) = meta.modified() {
            return modified > timestamp;
        }

        false
    }
}
