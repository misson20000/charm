use std::sync;
use std::vec;

use crate::model::addr;
use crate::model::datapath;
use crate::model::document;
use crate::model::document::structure;
use crate::model::document::structure::NodeKind;
use crate::model::space;

use serde_derive::Deserialize;
use serde::de::SeqAccess;

/* /===============================\   
 * | Struct definitions.           |   
 * \===============================/
 *
 * These struct definitions are copied here so that changing them elsewhere won't ruin the serialization.
 */

#[derive(Deserialize)]
pub struct Document {
    root: Childhood,
    datapath: DataPath,
}

#[derive(Deserialize)]
struct Addr {
    bytes: u64,
    bits: u8
}

#[derive(Deserialize)]
enum TitleDisplay {
    Inline,
    Major,
    Minor
}

#[derive(Deserialize)]
enum ChildrenDisplay {
    None,
    Summary,
    Full
}

#[derive(Deserialize)]
enum ContentDisplay {
    None,
    Hexdump {
        line_pitch: Addr,
        gutter_pitch: Addr,
    },
    Hexstring,
    Bindump {
        line_pitch: Addr,
        word_size: Addr,
    },
}

#[derive(Deserialize)]
struct Childhood {
    offset: Addr,
    name: String,
    title_display: TitleDisplay,
    children_display: ChildrenDisplay,
    content_display: ContentDisplay,
    locked: bool,
    size: Addr,
    children: Children,
}

/// This is done as a newtype so that we can use custom serialize/deserialize impls to convert between v1 and model::document::structure types at the correct time. This refers to the children of the given parent node, not that single node as a child.
struct Children(vec::Vec<structure::Childhood>);

#[derive(Deserialize)]
struct DataPath(vec::Vec<Filter>);

#[derive(Deserialize)]
enum Filter {
    LoadSpace(LoadSpaceFilter),
    Overwrite(OverwriteFilter),
    Move(MoveFilter),
    Insert(InsertFilter),
    /* This doesn't match the order in datapath because it was added later */
    Fill(FillFilter),
}

#[derive(Deserialize)]
struct LoadSpaceFilter {
    load_offset: u64,
    space_offset: u64,
    size: Option<u64>,
    
    space: Space,

    cache_block_size: u64,
    cache_block_count: std::num::NonZeroUsize,
}

#[derive(Deserialize)]
struct OverwriteFilter {
    offset: u64,
    bytes: vec::Vec<u8>,
}

#[derive(Deserialize)]
struct FillFilter {
    offset: u64,
    bytes: vec::Vec<u8>,
    len: u64,
}

#[derive(Deserialize)]
struct MoveFilter {
    from: u64,
    to: u64,
    size: u64,
}

#[derive(Deserialize)]
struct InsertFilter {
    offset: u64,
    bytes: vec::Vec<u8>,
}

#[derive(Deserialize)]
enum Space {
    File(FileAddressSpace),
    Memory(InMemoryAddressSpace),
}

#[derive(Deserialize)]
struct FileAddressSpace {
    path: std::path::PathBuf,
    label: std::string::String,
}

#[derive(Deserialize)]
struct InMemoryAddressSpace {
    data: vec::Vec<u8>,
    label: std::string::String,
}

/* /===============================\   
 * | Conversions.                  |   
 * \===============================/
 */

impl Into<document::Document> for Document {
    fn into(self) -> document::Document {
        let root_childhood: structure::Childhood = self.root.into();
        document::Builder::new(root_childhood.node)
            .datapath(self.datapath.into())
            .build()
    }
}

impl<Kind> Into<addr::Address<Kind>> for Addr {
    fn into(self) -> addr::Address<Kind> {
        addr::Address::<Kind>::new(self.bytes, self.bits)
    }
}

impl Into<structure::Childhood> for Childhood {
    fn into(self) -> structure::Childhood {
        structure::Childhood {
            offset: self.offset.into(),
            node: structure::HierarchyNode {
                props: structure::Properties {
                    name: self.name,
                    title_display: match self.title_display {
                        TitleDisplay::Inline => structure::TitleDisplay::Inline,
                        TitleDisplay::Major => structure::TitleDisplay::Major,
                        TitleDisplay::Minor => structure::TitleDisplay::Minor,
                    },
                    children_display: match self.children_display {
                        ChildrenDisplay::None => structure::ChildrenDisplay::None,
                        ChildrenDisplay::Summary => structure::ChildrenDisplay::Summary,
                        ChildrenDisplay::Full => structure::ChildrenDisplay::Full,
                    },
                    content_display: match self.content_display {
                        ContentDisplay::None => structure::ContentDisplay::None,
                        ContentDisplay::Hexdump { line_pitch, gutter_pitch } => structure::ContentDisplay::Hexdump {
                            line_pitch: line_pitch.into(),
                            gutter_pitch: gutter_pitch.into()
                        },
                        ContentDisplay::Bindump { line_pitch, word_size } => structure::ContentDisplay::Bindump {
                            line_pitch: line_pitch.into(),
                            word_size: word_size.into()
                        },
                        ContentDisplay::Hexstring => structure::ContentDisplay::Hexstring,
                    },
                    locked: self.locked,
                },
                size: self.size.into(),
                children: self.children.0,
            }.into_arc()
        }
    }
}

impl Into<datapath::DataPath> for DataPath {
    fn into(self) -> datapath::DataPath {
        datapath::DataPath::from_filters(self.0.into_iter().map(Filter::into).map(sync::Arc::new).collect())
    }
}

impl Into<datapath::Filter> for Filter {
    fn into(self) -> datapath::Filter {
        match self {
            Filter::LoadSpace(f) => datapath::Filter::LoadSpace(f.into()),
            Filter::Overwrite(f) => datapath::Filter::Overwrite(f.into()),
            Filter::Fill(f) => datapath::Filter::Fill(f.into()),
            Filter::Move(f) => datapath::Filter::Move(f.into()),
            Filter::Insert(f) => datapath::Filter::Insert(f.into()),
        }
    }
}

impl Into<datapath::LoadSpaceFilter> for LoadSpaceFilter {
    fn into(self) -> datapath::LoadSpaceFilter {
        datapath::LoadSpaceFilter::new_complete(
            sync::Arc::new(self.space.into()),
            self.load_offset,
            self.space_offset,
            self.size,
            self.cache_block_size,
            self.cache_block_count)
    }
}

impl Into<datapath::OverwriteFilter> for OverwriteFilter {
    fn into(self) -> datapath::OverwriteFilter {
        datapath::OverwriteFilter {
            offset: self.offset,
            bytes: self.bytes,
        }
    }
}

impl Into<datapath::FillFilter> for FillFilter {
    fn into(self) -> datapath::FillFilter {
        datapath::FillFilter {
            offset: self.offset,
            bytes: self.bytes,
            len: self.len,
        }
    }
}

impl Into<datapath::MoveFilter> for MoveFilter {
    fn into(self) -> datapath::MoveFilter {
        datapath::MoveFilter {
            from: self.from,
            to: self.to,
            size: self.size,
        }
    }
}

impl Into<datapath::InsertFilter> for InsertFilter {
    fn into(self) -> datapath::InsertFilter {
        datapath::InsertFilter {
            offset: self.offset,
            bytes: self.bytes,
        }
    }
}

impl Into<space::AddressSpace> for Space {
    fn into(self) -> space::AddressSpace {
        match self {
            Space::File(f) => space::AddressSpace::File(f.into()),
            Space::Memory(m) => space::AddressSpace::Memory(m.into()),
        }
    }
}

impl Into<space::file::FileAddressSpace> for FileAddressSpace {
    fn into(self) -> space::file::FileAddressSpace {
        space::file::FileAddressSpace::new(self.path, &self.label)
    }
}

impl Into<space::memory::InMemoryAddressSpace> for InMemoryAddressSpace {
    fn into(self) -> space::memory::InMemoryAddressSpace {
        space::memory::InMemoryAddressSpace::new(self.data, &self.label)
    }
}

/*
 * /===============================\   
 * | Serialize/deserialize impls.  |   
 * \===============================/
 */

struct Visitor<T>(std::marker::PhantomData<T>);

impl<T> Visitor<T> {
    fn new() -> Self {
        Self(std::marker::PhantomData)
    }
}

impl<'de> serde::Deserialize<'de> for Children {
    fn deserialize<D>(de: D) -> Result<Self, D::Error> where D: serde::Deserializer<'de> {
        de.deserialize_seq(Visitor::<Children>::new())
    }
}

impl<'de> serde::de::Visitor<'de> for Visitor<Children> {
    type Value = Children;
    
    fn expecting(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "a charm Children array")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Children, A::Error> where A: SeqAccess<'de> {
        let mut vec = vec::Vec::new();
        if let Some(hint) = seq.size_hint() {
            vec.reserve_exact(hint);
        }

        while let Some(e) = seq.next_element::<Childhood>()? {
            vec.push(e.into())
        }

        Ok(Children(vec))
    }
}
