use std::vec;
use std::sync;

pub mod file;

pub enum FetchResult {
    Ok(vec::Vec<u8>),
    Partial(vec::Vec<u8>),
    Unreadable,
    IoError(std::io::Error)
}

pub trait AddressSpace: Send + Sync {
    fn get_label(&self) -> &str;
    fn fetch(self: sync::Arc<Self>, extent: (u64, u64)) -> std::pin::Pin<Box<dyn futures::Future<Output = FetchResult> + Send + Sync>>;
}

