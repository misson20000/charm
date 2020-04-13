use std::pin;
use std::sync;
use std::task;
use std::string;
use std::future::Future;

use crate::addr;
use crate::space;
use crate::listing;

enum AsyncState {
    Pending(pin::Pin<Box<dyn Future<Output = space::FetchResult> + Send + Sync>>),
    Finished(space::FetchResult)
}

pub struct HexLine {
    pub extent: addr::Extent,
    pub distance_from_break: u64,
    internal: sync::RwLock<AsyncState>
}

impl HexLine {
    pub fn new(space: sync::Arc<dyn space::AddressSpace + Send + Sync>, extent: addr::Extent, distance_from_break: u64) -> HexLine {
        HexLine {
            extent,
            distance_from_break,
            internal: sync::RwLock::new(
                AsyncState::Pending(
                    Box::pin(space.fetch(extent, vec![0; (extent.size.bytes + 1) as usize])))),
        }
    }
    
    pub fn num_lines(&self) -> usize {
        1
    }

    pub fn progress(&self, cx: &mut task::Context) {
        let mut ih = self.internal.write().unwrap();
        match &mut *ih {
            AsyncState::Pending(future) => {
                match future.as_mut().poll(cx) {
                    task::Poll::Ready(result) => { std::mem::replace(&mut *ih, AsyncState::Finished(result)); },
                    task::Poll::Pending => ()
                }
            },
            AsyncState::Finished(_) => ()
        }
    }

    pub fn byte_at(&self, offset: usize) -> Option<u8> {
        let ih = self.internal.read().unwrap();
        
        let region = match *ih {
            AsyncState::Finished(space::FetchResult::Ok(ref data)) => &data[..],
            AsyncState::Finished(space::FetchResult::Partial(ref data, amt)) => &data[..amt],
            AsyncState::Finished(space::FetchResult::Unreadable) => &[] as &[u8],
            AsyncState::Finished(space::FetchResult::IoError(_)) => &[] as &[u8],
            AsyncState::Pending(_) => &[] as &[u8]
        };

        region.get(offset).map(|v| *v)
    }
    
    pub fn render(&self) -> string::String {
        let bytes = self.extent.size.round_up().bytes as usize;
        let ih = self.internal.read().unwrap();
        
        let (state, region) = match *ih {
            AsyncState::Finished(space::FetchResult::Ok(ref data)) => ("Ok", &data[..bytes]),
            AsyncState::Finished(space::FetchResult::Partial(ref data, amt)) => ("Partial", &data[..std::cmp::min(amt, bytes)]),
            AsyncState::Finished(space::FetchResult::Unreadable) => ("Unreadable", &[] as &[u8]),
            AsyncState::Finished(space::FetchResult::IoError(_)) => ("IO Error", &[] as &[u8]),
            AsyncState::Pending(_) => ("Pending", &[] as &[u8])
        };
        let mut str = std::string::String::new();
        
        for i in 0..(listing::LINE_SIZE.bytes as usize) {
            let offset = addr::Size { bytes: i as u64, bits: 0 };
            
            if i == 8 {
                str+= " ";
            }
            
            if i < region.len() {
                if offset < self.extent.size {
                    str+= &format!("{:02x} ", region[i]);
                } else {
                    str+= "   ";
                }
            } else {
                str+= "   ";
            }
        }
        str+= "| ";
        for i in 0..(listing::LINE_SIZE.bytes as usize) {
            if i == 8 {
                str+= " ";
            }
            if i < region.len() && (region[i] as char).is_ascii_alphanumeric() {
                str.push(region[i] as char)
            } else {
                str+= ".";
            }
        }
        str+= "  ";
        str+= state;

        str
    }
}
