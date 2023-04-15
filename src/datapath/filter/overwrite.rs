#[derive(Clone, Debug)]
pub struct OverwriteFilter {
    pub offset: u64,
    pub bytes: vec::Vec<u8>,
}

impl OverwriteFilter {
    fn fetch<'a, 'b, 'c>(&self, iter: impl iter::Iterator<Item = &'a Filter> + Clone, range: &'c mut ByteRecordRange<'b>, cx: &mut task::Context) {
        /* so we set load flags properly */
        Filter::fetch_next(iter, range, cx);

        // TODO: optimize this
        let addr = range.addr;
        
        for (i, br) in range.out.iter_mut().enumerate() {
            if addr + i as u64 >= self.offset && addr + i as u64 - self.offset < self.bytes.len() as u64 {
                br.overwritten = true;
                br.value = self.bytes[(addr + i as u64 - self.offset) as usize];
            }
        }
    }
    
    fn stack(a: &OverwriteFilter, b: &OverwriteFilter) -> Option<OverwriteFilter> {
        if b.offset == a.offset + a.bytes.len() as u64 {
            /* Stack if b immediately follows a */
            let mut bytes = vec::Vec::new();
            bytes.extend(a.bytes.iter());
            bytes.extend(b.bytes.iter());
            
            Some(OverwriteFilter {
                offset: a.offset,
                bytes,
            })
        } else if a.offset == b.offset + b.bytes.len() as u64 {
            /* Stack if a immediately follows b */
            let mut bytes = vec::Vec::new();
            bytes.extend(b.bytes.iter());
            bytes.extend(a.bytes.iter());

            Some(OverwriteFilter {
                offset: b.offset,
                bytes,
            })
        } else if b.offset >= a.offset && b.offset + b.bytes.len() as u64 <= a.offset + a.bytes.len() as u64 {
            /* Stack if b is contained entirely within a */
            let mut bytes = a.bytes.clone();

            for (i, byte) in bytes.iter_mut().enumerate() {
                if a.offset + i as u64 >= b.offset && a.offset + (i as u64) < b.offset + b.bytes.len() as u64 {
                    *byte = b.bytes[(a.offset + i as u64 - b.offset) as usize];
                }
            }

            Some(OverwriteFilter {
                offset: a.offset,
                bytes
            })
        } else {
            // TODO: stack some more complicated overlapping cases?
            None
        }
    }

    fn human_details(&self) -> string::String {
        util::fmt_hex_slice(&self.bytes).unwrap_or_else(|_| "error".to_string())
    }

    fn human_affects_addr(&self) -> u64 {
        self.offset
    }

    fn human_affects_size(&self) -> Option<u64> {
        Some(self.bytes.len() as u64)
    }
    
    pub fn to_filter(self) -> Filter {
        Filter::Overwrite(self)
    }
}
