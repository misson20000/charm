use std::sync;
use std::vec;

use crate::model::addr;
use crate::model::datapath;
use crate::model::document;
use crate::model::document::structure;
use crate::model::space;

use serde_derive::Serialize;
use serde_derive::Deserialize;
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
    Hexstring,
    Bindump {
        line_pitch: Addr,
        word_size: Addr,
    },
}

#[derive(Serialize, Deserialize)]
enum Node {
    Hierarchy(HierarchyNode),
    CommentV1(CommentNodeV1),
}

#[derive(Serialize, Deserialize)]
struct Childhood {
    offset: Addr,
    node: Node,
}

#[derive(Serialize, Deserialize)]
struct Properties {
    name: String,
    title_display: TitleDisplay,
    children_display: ChildrenDisplay,
    content_display: ContentDisplay,
    locked: bool,
}

#[derive(Serialize, Deserialize)]
struct HierarchyNode {
    props: Properties,
    size: Addr,
    children: Children,
}

#[derive(Serialize, Deserialize)]
struct CommentNodeV1 {
    text: String,
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
    /* This doesn't match the order in datapath because it was added later */
    Fill(FillFilter),
}

#[derive(Serialize, Deserialize)]
struct LoadSpaceFilter {
    load_offset: u64,
    space_offset: u64,
    size: Option<u64>,
    
    space: Space,

    cache_block_size: u64,
    cache_block_count: std::num::NonZeroUsize,
}

#[derive(Serialize, Deserialize)]
struct OverwriteFilter {
    offset: u64,
    bytes: vec::Vec<u8>,
}

#[derive(Serialize, Deserialize)]
struct FillFilter {
    offset: u64,
    bytes: vec::Vec<u8>,
    len: u64,
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

#[derive(Serialize, Deserialize)]
enum Space {
    File(FileAddressSpace),
    Memory(InMemoryAddressSpace),
}

#[derive(Serialize, Deserialize)]
struct FileAddressSpace {
    path: std::path::PathBuf,
    label: std::string::String,
}

#[derive(Serialize, Deserialize)]
struct InMemoryAddressSpace {
    data: vec::Vec<u8>,
    label: std::string::String,
}

/* /===============================\   
 * | Conversions.                  |   
 * \===============================/
 */

impl From<&document::Document> for Document {
    fn from(model: &document::Document) -> Document {
        Document {
            root: Childhood::from(structure::Childhood::new(model.root.clone(), addr::Offset::NULL)),
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

impl<Kind> From<addr::Address<Kind>> for Addr {
    fn from(addr: addr::Address<Kind>) -> Addr {
        Addr { bits: addr.bits(), bytes: addr.bytes() }
    }
}

impl<Kind> Into<addr::Address<Kind>> for Addr {
    fn into(self) -> addr::Address<Kind> {
        addr::Address::<Kind>::new(self.bytes, self.bits)
    }
}

impl From<structure::Childhood> for Childhood {
    fn from(c: structure::Childhood) -> Self {
        Self {
            offset: c.offset.into(),
            node: (*c.node).clone().into(),
        }
    }
}

impl Into<structure::Childhood> for Childhood {
    fn into(self) -> structure::Childhood {
        structure::Childhood {
            offset: self.offset.into(),
            node: sync::Arc::new(self.node.into())
        }
    }
}

impl From<structure::Properties> for Properties {
    fn from(p: structure::Properties) -> Self {
        Self {
            name: p.name.clone(),
            title_display: match p.title_display {
                structure::TitleDisplay::Inline => TitleDisplay::Inline,
                structure::TitleDisplay::Major => TitleDisplay::Major,
                structure::TitleDisplay::Minor => TitleDisplay::Minor,
            },
            children_display: match p.children_display {
                structure::ChildrenDisplay::None => ChildrenDisplay::None,
                structure::ChildrenDisplay::Summary => ChildrenDisplay::Summary,
                structure::ChildrenDisplay::Full => ChildrenDisplay::Full,
            },
            content_display: match p.content_display {
                structure::ContentDisplay::None => ContentDisplay::None,
                structure::ContentDisplay::Hexdump { line_pitch, gutter_pitch } => ContentDisplay::Hexdump {
                    line_pitch: line_pitch.into(),
                    gutter_pitch: gutter_pitch.into()
                },
                structure::ContentDisplay::Bindump { line_pitch, word_size } => ContentDisplay::Bindump {
                    line_pitch: line_pitch.into(),
                    word_size: word_size.into()
                },
                structure::ContentDisplay::Hexstring => ContentDisplay::Hexstring,
            },
            locked: p.locked,
        }
    }
}

impl Into<structure::Properties> for Properties {
    fn into(self) -> structure::Properties {
        structure::Properties {
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
        }
    }
}

impl From<structure::HierarchyNode> for HierarchyNode {
    fn from(h: structure::HierarchyNode) -> Self {
        Self {
            props: h.props.into(),
            size: h.size.into(),
            children: Children(h.children.clone()),
        }
    }
}

impl Into<structure::HierarchyNode> for HierarchyNode {
    fn into(self) -> structure::HierarchyNode {
        structure::HierarchyNode {
            props: self.props.into(),
            size: self.size.into(),
            children: self.children.0,
        }
    }
}

impl From<structure::CommentNode> for CommentNodeV1 {
    fn from(c: structure::CommentNode) -> Self {
        Self {
            text: c.text.clone(),
        }
    }
}

impl Into<structure::CommentNode> for CommentNodeV1 {
    fn into(self) -> structure::CommentNode {
        structure::CommentNode {
            text: self.text,
        }
    }
}

impl From<structure::Node> for Node {
    fn from(n: structure::Node) -> Self {
        match n {
            structure::Node::Hierarchy(h) => Self::Hierarchy(h.into()),
            structure::Node::Comment(c) => Self::CommentV1(c.into()),
        }
    }
}

impl Into<structure::Node> for Node {
    fn into(self) -> structure::Node {
        match self {
            Self::Hierarchy(h) => structure::Node::Hierarchy(h.into()),
            Self::CommentV1(c) => structure::Node::Comment(c.into()),
        }
    }
}

impl From<&datapath::DataPath> for DataPath {
    fn from(dp: &datapath::DataPath) -> Self {
        DataPath(dp.iter_filters().map(std::ops::Deref::deref).map(Filter::from).collect())
    }
}

impl Into<datapath::DataPath> for DataPath {
    fn into(self) -> datapath::DataPath {
        datapath::DataPath::from_filters(self.0.into_iter().map(Filter::into).map(sync::Arc::new).collect())
    }
}

impl From<&datapath::Filter> for Filter {
    fn from(f: &datapath::Filter) -> Filter {
        match f {
            datapath::Filter::LoadSpace(f) => Filter::LoadSpace(LoadSpaceFilter::from(f)),
            datapath::Filter::Overwrite(f) => Filter::Overwrite(OverwriteFilter::from(f)),
            datapath::Filter::Fill(f) => Filter::Fill(FillFilter::from(f)),
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
            Filter::Fill(f) => datapath::Filter::Fill(f.into()),
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
            
            space: (&**f.space()).into(),
            
            cache_block_size: f.cache_block_size(),
            cache_block_count: f.cache_block_count(),
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

impl From<&datapath::OverwriteFilter> for OverwriteFilter {
    fn from(f: &datapath::OverwriteFilter) -> OverwriteFilter {
        OverwriteFilter {
            offset: f.offset,
            bytes: f.bytes.clone(),
        }
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

impl From<&datapath::FillFilter> for FillFilter {
    fn from(f: &datapath::FillFilter) -> FillFilter {
        FillFilter {
            offset: f.offset,
            bytes: f.bytes.clone(),
            len: f.len,
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

impl From<&datapath::MoveFilter> for MoveFilter {
    fn from(f: &datapath::MoveFilter) -> MoveFilter {
        MoveFilter {
            from: f.from,
            to: f.to,
            size: f.size,
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

impl From<&datapath::InsertFilter> for InsertFilter {
    fn from(f: &datapath::InsertFilter) -> InsertFilter {
        InsertFilter {
            offset: f.offset,
            bytes: f.bytes.clone(),
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

impl From<&space::AddressSpace> for Space {
    fn from(s: &space::AddressSpace) -> Space {
        match s {
            space::AddressSpace::File(f) => Space::File(f.into()),
            space::AddressSpace::Memory(m) => Space::Memory(m.into()),
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

impl From<&space::file::FileAddressSpace> for FileAddressSpace {
    fn from(f: &space::file::FileAddressSpace) -> FileAddressSpace {
        FileAddressSpace {
            path: f.path.clone(),
            label: f.label.clone(),
        }
    }
}

impl Into<space::file::FileAddressSpace> for FileAddressSpace {
    fn into(self) -> space::file::FileAddressSpace {
        space::file::FileAddressSpace::new(self.path, &self.label)
    }
}

impl From<&space::memory::InMemoryAddressSpace> for InMemoryAddressSpace {
    fn from(m: &space::memory::InMemoryAddressSpace) -> InMemoryAddressSpace {
        InMemoryAddressSpace {
            data: m.data.clone(),
            label: m.label.clone(),
        }
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
