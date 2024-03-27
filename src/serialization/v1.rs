use std::sync;
use std::vec;

use crate::model::addr;
use crate::model::datapath;
use crate::model::document;
use crate::model::document::structure;

use serde::Serialize;
use serde::Deserialize;
use serde::de::Error;
use serde::de::SeqAccess;
use serde::ser::SerializeSeq;
use serde::ser::SerializeStruct;

/* /===============================\   
 * | Struct definitions.           |   
 * \===============================/
 *
 * These struct definitions are copied here so that changing them elsewhere won't ruin the serialization.
 */

pub struct Document {
    pub root: sync::Arc<structure::Node>,
    pub datapath: datapath::DataPath,
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

/* /===============================\   
 * | Conversions.                  |   
 * \===============================/
 */

impl From<&document::Document> for Document {
    fn from(model: &document::Document) -> Document {
        Document {
            root: model.root.clone(),
            datapath: model.datapath.clone(),
        }
    }
}

impl Into<document::Document> for Document {
    fn into(self) -> document::Document {
        document::Document::from_root_and_datapath(self.root, self.datapath)
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

impl serde::Serialize for Document {
    fn serialize<S>(&self, ser: S) -> Result<S::Ok, S::Error> where S: serde::Serializer {
        let mut ser = ser.serialize_struct("DocumentV1", 1)?;

        ser.serialize_field("root", &Childhood::from(structure::Childhood::new(self.root.clone(), addr::unit::NULL)))?;
        //ser.serialize_field("datapath", &None)?;
        
        ser.end()
    }
}

impl<'de> serde::Deserialize<'de> for Document {
    fn deserialize<D>(de: D) -> Result<Self, D::Error> where D: serde::Deserializer<'de> {
        de.deserialize_struct("DocumentV1", &["root"], Visitor::<Document>::new())
    }
}

impl<'de> serde::de::Visitor<'de> for Visitor<Document> {
    type Value = Document;
    
    fn expecting(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "a charm DocumentV1 struct")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Document, A::Error> where A: SeqAccess<'de> {
        let root: structure::Childhood = seq.next_element::<Childhood>()?.ok_or(A::Error::missing_field("root"))?.into();
        Ok(Document {
            root: root.node,
            datapath: datapath::DataPath::new(),
        })
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

