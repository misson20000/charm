use std::sync;
use std::time;
use std::vec;

pub mod cache;

pub mod file;

pub enum FetchResult {
    Ok(vec::Vec<u8>),
    Partial(vec::Vec<u8>),
    Unreadable,
    IoError(std::io::Error)
}

pub trait AddressSpaceExt {
    fn get_label(&self) -> &str;
    fn fetch(&self, extent: (u64, u64)) -> impl std::future::Future<Output = FetchResult>;

    /// Returns whether or not the underlying data may have been
    /// modified since the provided timestamp. This doesn't need to be
    /// precise, this is just a best-effort guess.
    fn is_dirty_since(&self, timestamp: time::SystemTime) -> bool;
}

pub enum AddressSpace {
    File(file::FileAddressSpace)
}

impl AddressSpace {
    fn fetch_owned(self: sync::Arc<Self>, extent: (u64, u64)) -> impl std::future::Future<Output = FetchResult> + 'static {
        async move {
            match &*self {
                AddressSpace::File(fas) => fas.fetch(extent).await,
            }
        }
    }
}

impl AddressSpaceExt for AddressSpace {
    fn get_label(&self) -> &str {
        match self {
            AddressSpace::File(fas) => fas.get_label(),
        }
    }

    async fn fetch(&self, extent: (u64, u64)) -> FetchResult {
        match self {
            AddressSpace::File(fas) => fas.fetch(extent).await,
        }
    }

    fn is_dirty_since(&self, timestamp: time::SystemTime) -> bool {
        match self {
            AddressSpace::File(fas) => fas.is_dirty_since(timestamp),
        }
    }
}

impl From<file::FileAddressSpace> for AddressSpace {
    fn from(fas: file::FileAddressSpace) -> AddressSpace {
        AddressSpace::File(fas)
    }
}
