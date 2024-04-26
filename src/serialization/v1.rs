use std::sync;
use std::vec;

use crate::model::addr;
use crate::model::datapath;
use crate::model::document;
use crate::model::document::structure;

use serde::Serialize;
use serde::Deserialize;
use serde::de::SeqAccess;
use serde::ser::SerializeSeq;

/* /===============================\   
 * | Struct definitions.           |   
 * \===============================/
 *
 * These struct definitions are copied here so that changing them elsewhere won't ruin the serialization.
 */

#[derive(Serialize, Deserialize)]
pub struct Document {
    root: Childhood,
    datapath: DataPath,
}

#[derive(Serialize, Deserialize)]
struct Addr {
    bytes: u64,
    bits: u8
}

#[derive(Serialize, Deserialize)]
enum TitleDisplay {
    Inline,
    Major,
    Minor
}

#[derive(Serialize, Deserialize)]
enum ChildrenDisplay {
    None,
    Summary,
    Full
}

#[derive(Serialize, Deserialize)]
enum ContentDisplay {
    None,
    Hexdump {
        line_pitch: Addr,
        gutter_pitch: Addr,
    },
    Hexstring
}

#[derive(Serialize, Deserialize)]
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

#[derive(Serialize, Deserialize)]
struct DataPath(vec::Vec<Filter>);

#[derive(Serialize, Deserialize)]
enum Filter {
    LoadSpace(LoadSpaceFilter),
    Overwrite(OverwriteFilter),
    Move(MoveFilter),
    Insert(InsertFilter),
}

#[derive(Serialize, Deserialize)]
struct LoadSpaceFilter {
    load_offset: u64,
    space_offset: u64,
    size: Option<u64>,
    
    path: String,

    cache_block_size: u64,
    cache_block_count: std::num::NonZeroUsize,
}

#[derive(Serialize, Deserialize)]
struct OverwriteFilter {
    offset: u64,
    bytes: vec::Vec<u8>
}

#[derive(Serialize, Deserialize)]
struct MoveFilter {
    from: u64,
    to: u64,
    size: u64,
}

#[derive(Serialize, Deserialize)]
struct InsertFilter {
    offset: u64,
    bytes: vec::Vec<u8>,
}

/* /===============================\   
 * | Conversions.                  |   
 * \===============================/
 */

impl From<&document::Document> for Document {
    fn from(model: &document::Document) -> Document {
        Document {
            root: Childhood::from(structure::Childhood::new(model.root.clone(), addr::unit::NULL)),
            datapath: DataPath::from(&model.datapath),
        }
    }
}

impl Into<document::Document> for Document {
    fn into(self) -> document::Document {
        let root_childhood: structure::Childhood = self.root.into();
        document::Builder::new(root_childhood.node)
            .datapath(self.datapath.into())
            .build()
    }
}

impl From<addr::Address> for Addr {
    fn from(addr: addr::Address) -> Addr {
        Addr { bits: addr.bit, bytes: addr.byte }
    }
}

impl From<addr::Size> for Addr {
    fn from(size: addr::Size) -> Addr {
        Self::from(size.to_addr())
    }
}

impl Into<addr::Address> for Addr {
    fn into(self) -> addr::Address {
        addr::Address { bit: self.bits, byte: self.bytes }
    }
}

impl Into<addr::Size> for Addr {
    fn into(self) -> addr::Size {
        addr::Size { bits: self.bits, bytes: self.bytes }
    }
}

impl From<structure::Childhood> for Childhood {
    fn from(c: structure::Childhood) -> Self {
        Self {
            offset: c.offset.into(),
            name: c.node.props.name.clone(),
            title_display: match c.node.props.title_display {
                structure::TitleDisplay::Inline => TitleDisplay::Inline,
                structure::TitleDisplay::Major => TitleDisplay::Major,
                structure::TitleDisplay::Minor => TitleDisplay::Minor,
            },
            children_display: match c.node.props.children_display {
                structure::ChildrenDisplay::None => ChildrenDisplay::None,
                structure::ChildrenDisplay::Summary => ChildrenDisplay::Summary,
                structure::ChildrenDisplay::Full => ChildrenDisplay::Full,
            },
            content_display: match c.node.props.content_display {
                structure::ContentDisplay::None => ContentDisplay::None,
                structure::ContentDisplay::Hexdump { line_pitch, gutter_pitch } => ContentDisplay::Hexdump {
                    line_pitch: line_pitch.into(),
                    gutter_pitch: gutter_pitch.into()
                },
                structure::ContentDisplay::Hexstring => ContentDisplay::Hexstring,
            },
            locked: c.node.props.locked,
            size: c.node.size.into(),
            children: Children(c.node.children.clone()),
        }
    }
}

impl Into<structure::Childhood> for Childhood {
    fn into(self) -> structure::Childhood {
        structure::Childhood {
            offset: self.offset.into(),
            node: sync::Arc::new(structure::Node {
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
                        ContentDisplay::Hexstring => structure::ContentDisplay::Hexstring,
                    },
                    locked: self.locked,
                },
                size: self.size.into(),
                children: self.children.0,
            })
        }
    }
}

impl From<&datapath::DataPath> for DataPath {
    fn from(dp: &datapath::DataPath) -> Self {
        DataPath(dp.iter().map(Filter::from).collect())
    }
}

impl Into<datapath::DataPath> for DataPath {
    fn into(self) -> datapath::DataPath {
        self.0.into_iter().map(Filter::into).collect()
    }
}

impl From<&datapath::Filter> for Filter {
    fn from(f: &datapath::Filter) -> Filter {
        match f {
            datapath::Filter::LoadSpace(f) => Filter::LoadSpace(LoadSpaceFilter::from(f)),
            datapath::Filter::Overwrite(f) => Filter::Overwrite(OverwriteFilter::from(f)),
            datapath::Filter::Move(f) => Filter::Move(MoveFilter::from(f)),
            datapath::Filter::Insert(f) => Filter::Insert(InsertFilter::from(f)),
        }
    }
}

impl Into<datapath::Filter> for Filter {
    fn into(self) -> datapath::Filter {
        match self {
            Filter::LoadSpace(f) => datapath::Filter::LoadSpace(f.into()),
            Filter::Overwrite(f) => datapath::Filter::Overwrite(f.into()),
            Filter::Move(f) => datapath::Filter::Move(f.into()),
            Filter::Insert(f) => datapath::Filter::Insert(f.into()),
        }
    }
}

impl From<&datapath::LoadSpaceFilter> for LoadSpaceFilter {
    fn from(f: &datapath::LoadSpaceFilter) -> LoadSpaceFilter {
        LoadSpaceFilter {
            load_offset: f.load_offset,
            space_offset: f.space_offset,
            size: f.size,
            
            path: f.path(),
            
            cache_block_size: f.cache_block_size(),
            cache_block_count: f.cache_block_count(),
        }
    }
}

impl Into<datapath::LoadSpaceFilter> for LoadSpaceFilter {
    fn into(self) -> datapath::LoadSpaceFilter {
        todo!();
    }
}

impl From<&datapath::OverwriteFilter> for OverwriteFilter {
    fn from(f: &datapath::OverwriteFilter) -> OverwriteFilter {
        todo!();
    }
}

impl Into<datapath::OverwriteFilter> for OverwriteFilter {
    fn into(self) -> datapath::OverwriteFilter {
        todo!();
    }
}

impl From<&datapath::MoveFilter> for MoveFilter {
    fn from(f: &datapath::MoveFilter) -> MoveFilter {
        todo!();
    }
}

impl Into<datapath::MoveFilter> for MoveFilter {
    fn into(self) -> datapath::MoveFilter {
        todo!();
    }
}

impl From<&datapath::InsertFilter> for InsertFilter {
    fn from(f: &datapath::InsertFilter) -> InsertFilter {
        todo!();
    }
}

impl Into<datapath::InsertFilter> for InsertFilter {
    fn into(self) -> datapath::InsertFilter {
        todo!();
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

impl serde::Serialize for Children {
    fn serialize<S>(&self, ser: S) -> Result<S::Ok, S::Error> where S: serde::Serializer {
        let mut ser = ser.serialize_seq(Some(self.0.len()))?;

        for child in &self.0 {
            ser.serialize_element(&Childhood::from(child.clone()))?;
        }

        ser.end()
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

