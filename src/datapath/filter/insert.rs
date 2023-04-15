use std::iter;
use std::task;
use std::vec;

use super::overwrite::OverwriteFilter;

#[derive(Clone, Debug)]
pub struct InsertFilter {
    pub offset: u64,
    pub bytes: vec::Vec<u8>,
}

impl InsertFilter {
    fn fetch<'a, 'b, 'c>(&self, iter: impl iter::Iterator<Item = &'a Filter> + Clone, range: &'c mut ByteRecordRange<'b>, cx: &mut task::Context) {
        let (mut before, mut overlap, mut after) = range.split3((self.offset, Some(self.bytes.len() as u64)));

        if !before.is_empty() {
            Filter::fetch_next(iter.clone(), &mut before, cx);
        }

        if !after.is_empty() {
            after.addr-= self.bytes.len() as u64;
            Filter::fetch_next(iter.clone(), &mut after, cx);

            for br in after.out.iter_mut() {
                br.moved = true;
            }
        }

        if !overlap.is_empty() {
            /* so we set load flags properly */
            Filter::fetch_next(iter, &mut overlap, cx);
            
            // TODO: optimize this
            let addr = range.addr;
            
            for (i, br) in range.out.iter_mut().enumerate() {
                if addr + i as u64 >= self.offset && addr + i as u64 - self.offset < self.bytes.len() as u64 {
                    br.inserted = true;
                    br.value = self.bytes[(addr + i as u64 - self.offset) as usize];
                }
            }            
        }
    }

    fn stack(a: &InsertFilter, b: &InsertFilter) -> Option<InsertFilter> {
        if b.offset == a.offset + a.bytes.len() as u64 {
            /* Stack if b immediately follows a */
            let mut bytes = vec::Vec::new();
            bytes.extend(a.bytes.iter());
            bytes.extend(b.bytes.iter());

            Some(InsertFilter {
                offset: a.offset,
                bytes,
            })
        } else {
            None
        }
    }

    fn human_details(&self) -> String {
        util::fmt_hex_slice(&self.bytes).unwrap_or_else(|_| "error".to_string())
    }

    fn human_affects_addr(&self) -> u64 {
        self.offset
    }

    fn human_affects_size(&self) -> Option<u64> {
        Some(self.bytes.len() as u64)
    }
    
    fn to_filter(self) -> Filter {
        Filter::Insert(self)
    }
}

impl InsertFilter {
    fn stack_overwrite(a: &InsertFilter, b: &OverwriteFilter) -> Option<InsertFilter> {
        if b.offset == a.offset + a.bytes.len() as u64 - 1 && b.bytes.len() == 1 {
            /* Stack if b overwrites the last byte of a */
            let mut filter = a.clone();
            filter.bytes[a.bytes.len() - 1] = b.bytes[0];
            Some(filter)
        } else {
            None
        }
    }
}
