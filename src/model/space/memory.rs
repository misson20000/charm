use std::string;
use std::time;
use std::vec;

use crate::model::space;

pub struct InMemoryAddressSpace {
    pub data: vec::Vec<u8>,
    pub label: string::String,
}

impl InMemoryAddressSpace {
    pub fn new(data: vec::Vec<u8>, label: &str) -> InMemoryAddressSpace {
        InMemoryAddressSpace {
            data,
            label: label.to_string(),
        }
    }
}

impl space::AddressSpaceExt for InMemoryAddressSpace {
    fn get_label(&self) -> &str {
        &self.label
    }
    
    async fn fetch(&self, extent: (u64, u64)) -> space::FetchResult {
        let begin = if extent.0 < self.data.len() as u64 {
            extent.0
        } else {
            return space::FetchResult::Unreadable;
        };

        let end = std::cmp::min(extent.0 + extent.1, self.data.len() as u64);
        let mut out = vec![0; (end - begin) as usize];
        out[..].copy_from_slice(&self.data[(begin as usize)..(end as usize)]);

        if end == extent.0 + extent.1 {
            space::FetchResult::Ok(out)
        } else {
            space::FetchResult::Partial(out)
        }
    }

    fn is_dirty_since(&self, _timestamp: time::SystemTime) -> bool {
        false
    }
}
