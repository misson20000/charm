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
}

pub enum AddressSpace {
    File(file::FileAddressSpace)
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
}

impl From<file::FileAddressSpace> for AddressSpace {
    fn from(fas: file::FileAddressSpace) -> AddressSpace {
        AddressSpace::File(fas)
    }
}
