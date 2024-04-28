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
        offset: addr::Address,
        low_nybble: bool,
    }
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

fn adjust_tuple_for_structure_selection(tuple: &(structure::Path, Part)) -> Result<(structure::PathSlice, usize, &Part), selection::listing::StructureMode> {
    Ok(match tuple {
        (path, Part::Title) if path.len() == 0 => return Err(selection::listing::StructureMode::All),
        (path, Part::Title) => (&path[0..path.len()-1], *path.last().unwrap(), &Part::Title),
        (path, part) => (&path[..], 0, part),
    })
}    

pub fn to_structure_selection(document: &document::Document, a: &Triplet, b: &Triplet) -> selection::listing::StructureMode {
    let begin = std::cmp::min(PickSort(&a.begin), PickSort(&b.begin)).0;
    let end = std::cmp::max(PickSort(&a.end), PickSort(&b.end)).0;

    let (begin_path, begin_child, begin_part) = match adjust_tuple_for_structure_selection(&begin) {
        Ok(x) => x,
        Err(sm) => return sm,
    };
    
    let (end_path, end_child, end_part) = match adjust_tuple_for_structure_selection(&end) {
        Ok(x) => x,
        Err(sm) => return sm,
    };
    
    /* This is the common prefix of the path between the two pick results. */
    let path: Vec<usize> = std::iter::zip(begin_path.iter(), end_path.iter()).map_while(|(a, b)| if a == b { Some(*a) } else { None }).collect();
    let (node, _node_addr) = document.lookup_node(&path);

    let begin = match begin_part {
        /* Pick result 'a' was deeper in the hierarchy than the common prefix. Round down to the start of the child of the common prefix. */
        _ if path.len() < begin_path.len() => (node.children[begin_path[path.len()]].offset, begin_path[path.len()]),
        _ if path[..] != begin_path[..] => panic!("beginning pick result was shallower than the common prefix, which means the common prefix wasn't actually common"),

        Part::Title => (node.children[begin_child].offset, begin_child),
        Part::Hexdump { offset, .. } => (*offset, node.child_at_offset(*offset)),
    };

    let end = match end_part {
        /* Pick result 'b' was deeper in the hierarchy than the common prefix. Bump out to the end of the child of the common prefix. */
        _ if path.len() < end_path.len() => (node.children[end_path[path.len()]].end(), end_path[path.len()]+1),
        _ if path[..] != end_path[..] => panic!("ending pick result was shallower than the common prefix, which means the common prefix wasn't actually common"),

        Part::Title => (node.children[end_child].end(), end_child+1),
        Part::Hexdump { offset, .. } => (*offset, node.child_at_offset(*offset)),
    };

    selection::listing::StructureMode::Range(selection::listing::StructureRange {
        path,
        begin,
        end,
    })
}

impl Part {
    pub fn cursor_placement_hint(&self) -> cursor::PlacementHint {
        match self {
            Part::Title => cursor::PlacementHint::Title,
            Part::Hexdump { low_nybble, .. } => cursor::PlacementHint::Hexdump(cursor::hexdump::HexdumpPlacementHint {
                low_nybble: *low_nybble
            }),
        }
    }

    pub fn offset(&self) -> addr::Address {
        match self {
            Part::Title => addr::unit::NULL,
            Part::Hexdump { offset, .. } => *offset,
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
            }
        } else if self.0.0.len() > prefix_length && other.0.0.len() == prefix_length {
            return match (self.0.0[prefix_length], &other.0.1) {
                (_, Part::Title) => std::cmp::Ordering::Greater,
                (self_index, Part::Hexdump { index: other_index, .. }) if self_index >= *other_index => std::cmp::Ordering::Greater,
                (_, Part::Hexdump { index: _, .. }) => std::cmp::Ordering::Less,
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

        const BEGIN_OFFSET: addr::Address = addr::Address::new(0x18);
        const END_OFFSET: addr::Address = addr::Address::new(0x60);
        
        assert_eq!(sel, selection::listing::StructureMode::Range(selection::listing::StructureRange {
            path: vec![],
            begin: (0x18.into(), 0),
            end: (0x60.into(), 1),
        }));
    }
}
