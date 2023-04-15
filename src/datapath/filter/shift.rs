#[derive(Clone, Debug)]
pub struct MoveFilter {
    pub from: u64,
    pub to: u64,
    pub size: u64,
}

impl MoveFilter {
    fn fetch<'a, 'b, 'c>(&self, _iter: impl iter::Iterator<Item = &'a Filter>, _range: &'c mut ByteRecordRange<'b>, _cx: &mut task::Context) {
        todo!("implement MoveFilter::fetch");
    }
    
    fn stack(a: &MoveFilter, b: &MoveFilter) -> Option<MoveFilter> {
        if a.to == b.from && a.size == b.size {
            /* Stack if b's source is the same as a's destination */
            Some(MoveFilter {
                from: a.from,
                to: b.to,
                size: a.size,
            })
        } else {
            None
        }
    }


    fn human_details(&self) -> string::String {
        "todo".to_string()
    }

    fn human_affects_addr(&self) -> u64 {
        self.to
    }

    fn human_affects_size(&self) -> Option<u64> {
        Some(self.size)
    }
}
