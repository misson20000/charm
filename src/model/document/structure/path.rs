use std::vec;

pub type Path = vec::Vec<usize>;
pub type PathSlice<'a> = &'a [usize];
pub type PathIter<'a> = vec::IntoIter<usize>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SiblingRange {
    pub parent: Path,
    pub first: usize,

    /// Inclusive
    pub last: usize
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RangeInvalidity {
    IndexExceedsNumberOfChildren,
    Inverted,
}

impl SiblingRange {
    pub fn new(parent: Path, first: usize, last: usize) -> Self {
        Self { parent, first, last }
    }

    pub fn check_validity(&self, parent_node: &impl super::NodeKind) -> Result<(), RangeInvalidity> {
        if self.first >= parent_node.num_children() || self.last >= parent_node.num_children() {
            return Err(RangeInvalidity::IndexExceedsNumberOfChildren);
        }

        if self.last < self.first {
            return Err(RangeInvalidity::Inverted);
        }

        return Ok(());
    }
    
    /// How many nodes this range refers to (not counting descendants).
    pub fn count(&self) -> usize {
        self.last - self.first + 1
    }
    
    pub fn indices(&self) -> std::ops::RangeInclusive<usize> {
        self.first..=self.last
    }

    pub fn contains_index(&self, index: usize) -> bool {
        self.indices().contains(&index)
    }

    /// Returns true if the path refers to one of the siblings included in this range, but not if it refers to one of
    /// their descendants.
    pub fn contains_path(&self, path: PathSlice) -> bool {
        path.len() == self.parent.len() + 1 && &path[0..self.parent.len()] == &self.parent[..] && self.contains_index(path[self.parent.len()])
    }

    /// Returns true if the path refers to one of the siblings included in this range or one of their descendants.
    pub fn contains_descendant(&self, path: PathSlice) -> bool {
        path.len() > self.parent.len() && &path[0..self.parent.len()] == &self.parent[..] && self.contains_index(path[self.parent.len()])
    }

    pub fn overlaps(&self, other: &SiblingRange) -> bool {
        &self.parent[..] == &other.parent[..] && self.first <= other.last && other.first <= self.last
    }
}
