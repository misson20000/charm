use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::cursor;
use crate::model::selection;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Part {
    Title,
    Hexdump {
        index: usize,
        offset: addr::Offset,
        low_nybble: bool,
    },
    Bindump {
        index: usize,
        offset: addr::Offset,
    },
    Hexstring {
        index: usize,
        offset: addr::Offset,
        low_nybble: bool,
    },
    Ellipsis {
        index: usize,
        offset: addr::Offset,
    },
}

#[derive(Clone, Debug)]
pub struct Triplet {
    pub begin: (structure::Path, Part),
    pub middle: (structure::Path, Part),
    pub end: (structure::Path, Part),
}

#[derive(PartialEq, Eq)]
struct PickSort<'a>(&'a (structure::Path, Part));

impl Triplet {
    pub fn all3(path: structure::Path, part: Part) -> Self {
        Triplet {
            begin: (path.clone(), part.clone()),
            middle: (path.clone(), part.clone()),
            end: (path, part),
        }
    }
}

fn path_part_to_endpoint<'a>(document: &'_ document::Document, tuple: &'a (structure::Path, Part), is_end: bool) -> Result<selection::listing::StructureEndpoint<'a>, selection::listing::StructureMode> {
    Ok(match tuple {
        (path, Part::Title) if path.len() == 0 => return Err(selection::listing::StructureMode::All),
        
        (path, Part::Title) => {
            let adj_path = &path[0..path.len()-1];
            let mut adj_child = *path.last().unwrap();
            let child = &document.lookup_node(adj_path).0.children[adj_child];
            let mut adj_offset = child.offset;

            if is_end {
                adj_child+= 1;
                adj_offset = child.end();
            }
            
            selection::listing::StructureEndpoint {
                parent: adj_path,
                child_index: adj_child,
                offset: adj_offset
            }
        }
        
        (path, Part::Hexdump { index, offset, .. }) => selection::listing::StructureEndpoint {
            parent: &path[..],
            child_index: *index,
            offset: *offset
        },

        (path, Part::Bindump { index, offset, .. }) => selection::listing::StructureEndpoint {
            parent: &path[..],
            child_index: *index,
            offset: *offset
        },
        
        (path, Part::Hexstring { index, offset, .. }) => selection::listing::StructureEndpoint {
            parent: &path[..],
            child_index: *index,
            offset: *offset
        },

        (path, Part::Ellipsis { index, offset, .. }) => selection::listing::StructureEndpoint {
            parent: &path[..],
            child_index: *index,
            offset: *offset
        },
    })
}    

pub fn to_structure_selection(document: &document::Document, a: &Triplet, b: &Triplet) -> selection::listing::StructureMode {
    let begin = std::cmp::min(PickSort(&a.begin), PickSort(&b.begin)).0;
    let end = std::cmp::max(PickSort(&a.end), PickSort(&b.end)).0;

    let begin = match path_part_to_endpoint(document, &begin, false) {
        Ok(x) => x,
        Err(sm) => return sm,
    };
    
    let end = match path_part_to_endpoint(document, &end, true) {
        Ok(x) => x,
        Err(sm) => return sm,
    };

    selection::listing::StructureMode::Range(selection::listing::StructureRange::between(document, begin, end))
}

impl Part {
    pub fn cursor_placement_hint(&self) -> cursor::PlacementHint {
        match self {
            Part::Title => cursor::PlacementHint::Title,
            Part::Hexdump { low_nybble, .. } => cursor::PlacementHint::Hexdump(cursor::hexdump::HexdumpPlacementHint {
                low_nybble: *low_nybble
            }),
            Part::Bindump { .. } => cursor::PlacementHint::Bindump(cursor::bindump::BindumpPlacementHint {
            }),
            Part::Hexstring { low_nybble, .. } => cursor::PlacementHint::Hexstring(cursor::hexstring::HexstringPlacementHint {
                low_nybble: *low_nybble
            }),
            Part::Ellipsis { .. } => cursor::PlacementHint::Unused,
        }
    }

    pub fn offset(&self) -> addr::Offset {
        match self {
            Part::Title => addr::Offset::NULL,
            Part::Hexdump { offset, .. } => *offset,
            Part::Bindump { offset, .. } => *offset,
            Part::Hexstring { offset, .. } => *offset,
            Part::Ellipsis { offset, .. } => *offset,
        }
    }
}

impl<'a> Ord for PickSort<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let mut prefix_length = 0;
        for (a, b) in std::iter::zip(&self.0.0, &other.0.0) {
            match a.cmp(&b) {
                std::cmp::Ordering::Equal => prefix_length+= 1,
                x => return x
            }
        }

        /* They share a common prefix. */
        
        if self.0.0.len() == prefix_length && other.0.0.len() > prefix_length {
            return match (&self.0.1, other.0.0[prefix_length]) {
                (Part::Title, _) => std::cmp::Ordering::Less,
                (Part::Hexdump { index: self_index, .. }, other_index) if *self_index <= other_index => std::cmp::Ordering::Less,
                (Part::Hexdump { index: _, .. }, _) => std::cmp::Ordering::Greater,
                (Part::Bindump { index: self_index, .. }, other_index) if *self_index <= other_index => std::cmp::Ordering::Less,
                (Part::Bindump { index: _, .. }, _) => std::cmp::Ordering::Greater,
                (Part::Hexstring { index: self_index, .. }, other_index) if *self_index <= other_index => std::cmp::Ordering::Less,
                (Part::Hexstring { index: _, .. }, _) => std::cmp::Ordering::Greater,
                (Part::Ellipsis { index: self_index, .. }, other_index) if *self_index <= other_index => std::cmp::Ordering::Less,
                (Part::Ellipsis { index: _, .. }, _) => std::cmp::Ordering::Greater,
            }
        } else if self.0.0.len() > prefix_length && other.0.0.len() == prefix_length {
            return match (self.0.0[prefix_length], &other.0.1) {
                (_, Part::Title) => std::cmp::Ordering::Greater,
                (self_index, Part::Hexdump { index: other_index, .. }) if self_index >= *other_index => std::cmp::Ordering::Greater,
                (_, Part::Hexdump { index: _, .. }) => std::cmp::Ordering::Less,
                (self_index, Part::Bindump { index: other_index, .. }) if self_index >= *other_index => std::cmp::Ordering::Greater,
                (_, Part::Bindump { index: _, .. }) => std::cmp::Ordering::Less,
                (self_index, Part::Hexstring { index: other_index, .. }) if self_index >= *other_index => std::cmp::Ordering::Greater,
                (_, Part::Hexstring { index: _, .. }) => std::cmp::Ordering::Less,
                (self_index, Part::Ellipsis { index: other_index, .. }) if self_index >= *other_index => std::cmp::Ordering::Greater,
                (_, Part::Ellipsis { index: _, .. }) => std::cmp::Ordering::Less,
            }
        } else if self.0.0.len() > prefix_length && other.0.0.len() > prefix_length {
            return self.0.0[prefix_length].cmp(&other.0.0[prefix_length]);
        } else if self.0.0.len() == prefix_length && other.0.0.len() == prefix_length {
            return self.0.1.cmp(&other.0.1);
        } else {
            panic!("should be unreachable");
        }
    }
}

impl<'a> PartialOrd for PickSort<'a> {
    fn partial_cmp(&self, other: &PickSort<'a>) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pick_sort() {
        let a = (vec![0, 0], Part::Hexdump {
            index: 0,
            offset: 1.into(),
            low_nybble: false
        });

        let b = (vec![0], Part::Hexdump {
            index: 0,
            offset: 1.into(),
            low_nybble: false
        });

        assert_eq!(PickSort(&a).cmp(&PickSort(&b)), std::cmp::Ordering::Greater);
        assert_eq!(PickSort(&b).cmp(&PickSort(&a)), std::cmp::Ordering::Less);
    }

    #[test]
    fn rubber_band_hexdump_to_title() {
        let document = document::Builder::new(
            structure::Node::builder()
                .name("root")
                .size(0x100)
                .child(0x40, |b| b
                       .name("child")
                       .size(0x20))
                .build()
        ).build();

        let pr1 = Triplet::all3(vec![], Part::Hexdump {
            index: 0,
            offset: addr::Address::from(0x18),
            low_nybble: false,
        });

        let pr2 = Triplet::all3(vec![0], Part::Title);

        let sel = to_structure_selection(&document, &pr1, &pr2);

        const BEGIN_OFFSET: addr::Offset = addr::Offset::new(0x18, 0);
        const END_OFFSET: addr::Offset = addr::Offset::new(0x60, 0);
        
        assert_eq!(sel, selection::listing::StructureMode::Range(selection::listing::StructureRange {
            path: vec![],
            begin: (0x18.into(), 0),
            end: (0x60.into(), 1),
        }));
    }
}
