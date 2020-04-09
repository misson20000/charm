use std::vec;
use std::sync;

use crate::addr;

pub enum FetchResult {
    Ok(vec::Vec<u8>),
    Partial(vec::Vec<u8>, usize),
    Unreadable,
    IoError(std::io::Error)
}

pub trait AddressSpace {
    fn get_label(&self) -> &str;
    fn fetch(self: sync::Arc<Self>, loc: addr::Address, size: addr::Size, out: vec::Vec<u8>) -> std::pin::Pin<Box<dyn futures::Future<Output = FetchResult> + Send + Sync>>;
}

pub mod file;
