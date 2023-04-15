//use enum_dispatch::enum_dispatch;

//pub mod insert;
pub mod load_space;
//pub mod shift;
//pub mod overwrite;

pub use load_space::LoadSpaceFilter;

/*
pub trait Filter {
    fn fetch(&self, iter: impl iter::Iterator<Item = &'_ Filter> + Clone, range: ByteRecordRange<'_>, cx: &mut task::Context);
    fn stack(a: &Self, b: &Self) -> Option<Self>;

    fn human_details(&self) -> string::String;
    fn human_affects_addr(&self) -> u64;
    fn human_affects_size(&self) -> Option<u64>;
    fn to_filter(self) -> super::Filter;
}
*/
