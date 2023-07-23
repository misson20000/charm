use std::sync;

use crate::model::document;
use crate::model::document::change as doc_change;
use crate::model::document::structure;
use crate::model::versioned;
use crate::model::versioned::Versioned;

#[derive(Debug, Clone)]
pub enum Mode {
    Empty,
    Single(structure::Path),

    /// Inclusive
    SiblingRange(structure::Path, usize, usize),
    
    All,
}

impl Mode {
    fn new_range_between(begin: &Vec<usize>, end: &Vec<usize>) -> Mode {
        if begin.is_empty() || end.is_empty() {
            return Mode::All;
        }
        
        let shared_prefix: Vec<usize> = (&begin[0..begin.len()-1])
            .iter()
            .zip((&end[0..end.len()-1]).iter())
            .map_while(|(x, y)| if x == y { Some(*x) } else { None })
            .collect();
        
        let sp_len = shared_prefix.len();
        
        Mode::SiblingRange(shared_prefix, begin[sp_len], end[sp_len])
    }
}

#[derive(Clone)]
pub struct Selection {
    pub document: sync::Arc<document::Document>,
    pub mode: Mode,

    version: versioned::Version<Selection>,
}

impl Selection {
    pub fn new(document: sync::Arc<document::Document>) -> Self {
        Selection {
            document,
            mode: Mode::Empty,

            version: Default::default(),
        }
    }

    fn update_document(&mut self, new_doc: &sync::Arc<document::Document>) -> ChangeRecord {
        if self.document.is_outdated(new_doc) {
            let from_generation = self.document.generation();
            let mut selection_changed = false;
            new_doc.changes_since(&self.document.clone(), &mut |new_doc, change| selection_changed|= self.port_doc_change(new_doc, change));

            ChangeRecord {
                document_updated: Some((from_generation, new_doc.clone())),
                selection_changed
            }
        } else {
            ChangeRecord {
                document_updated: None,
                selection_changed: false
            }
        }
    }

    fn port_doc_change(&mut self, new_doc: &sync::Arc<document::Document>, change: &doc_change::Change) -> bool {
        self.document = new_doc.clone();

        /* Whether the selection changed significantly in a way the user should
         * care about or not (e.g. they had selected a node that got deleted and
         * we had to clear their selection) */
        let mut selection_changed = false;
        
        self.mode = match std::mem::replace(&mut self.mode, Mode::Empty) {
            Mode::Empty => Mode::Empty,
            Mode::Single(mut path) => match change.update_path(&mut path) {
                doc_change::UpdatePathResult::Moved | doc_change::UpdatePathResult::Unmoved => Mode::Single(path),
                doc_change::UpdatePathResult::Destructured => match &change.ty {
                    doc_change::ChangeType::Destructure { parent: affected_parent, child_index, num_grandchildren, offset: _ } => {
                        selection_changed = true;

                        if *num_grandchildren == 0 {
                            Mode::Empty
                        } else {
                            Mode::SiblingRange(affected_parent.clone(), *child_index, *child_index + *num_grandchildren - 1)
                        }
                    },
                    _ => panic!("got UpdatePathResult::Destructured from a non-Destructure type Change"),
                },
                doc_change::UpdatePathResult::Deleted => {
                    selection_changed = true;
                    Mode::Empty
                },
            },
            Mode::SiblingRange(mut path, mut first_child, mut last_child) => match change.update_path(&mut path) {
                doc_change::UpdatePathResult::Moved | doc_change::UpdatePathResult::Unmoved => match &change.ty {
                    doc_change::ChangeType::AlterNode { .. } => Mode::SiblingRange(path, first_child, last_child),
                    doc_change::ChangeType::InsertNode { parent: affected_path, index: insertion_index, child: _ } if affected_path == &path => {
                        if (first_child..=last_child).contains(insertion_index) {
                            selection_changed = true;
                            Mode::SiblingRange(path, first_child, last_child+1)
                        } else {
                            if first_child >= *insertion_index {
                                first_child+= 1;
                            }
                            if last_child >= *insertion_index {
                                last_child+= 1;
                            }
                            Mode::SiblingRange(path, first_child, last_child)
                        }
                    }
                    doc_change::ChangeType::InsertNode { .. } => Mode::SiblingRange(path, first_child, last_child),
                    doc_change::ChangeType::Nest { parent: affected_path, .. } if affected_path == &path => {
                        // TODO: smarter logic here
                        path.push(first_child);
                        selection_changed = true;
                        Mode::Single(path)
                    },
                    doc_change::ChangeType::Nest { .. } => Mode::SiblingRange(path, first_child, last_child),
                    doc_change::ChangeType::Destructure { parent: affected_parent, child_index, num_grandchildren, offset: _ } if affected_parent == &path => {
                        if first_child == *child_index && last_child == *child_index && *num_grandchildren == 0 {
                            selection_changed = true;
                            Mode::Empty
                        } else {
                            if (first_child..=last_child).contains(child_index) {
                                selection_changed = true;
                            }
                            
                            if first_child > *child_index {
                                first_child+= num_grandchildren;
                            }

                            if last_child >= *child_index {
                                last_child+= num_grandchildren - 1;
                            }

                            Mode::SiblingRange(path, first_child, last_child)
                        }
                    },
                    doc_change::ChangeType::Destructure { .. } => Mode::SiblingRange(path, first_child, last_child),
                    doc_change::ChangeType::DeleteRange { parent: affected_path, first_child: deleted_first, last_child: deleted_last } if affected_path == &path => {
                        let deleted = *deleted_first..=*deleted_last;
                        if deleted.contains(&first_child) && deleted.contains(&last_child) {
                            selection_changed = true;
                            Mode::Empty
                        } else {
                            if deleted.contains(&first_child) {
                                selection_changed = true;
                                first_child = *deleted_first;
                            } else if first_child > *deleted_last {
                                first_child-= (*deleted_last-*deleted_first)+1;
                            }
                            
                            if deleted.contains(&last_child) {
                                selection_changed = true;
                                last_child = *deleted_first-1;
                            } else if last_child > *deleted_last {
                                last_child-= (*deleted_last-*deleted_first)+1;
                            }

                            Mode::SiblingRange(path, first_child, last_child)
                        }
                    },
                    doc_change::ChangeType::DeleteRange { .. } => Mode::SiblingRange(path, first_child, last_child),
                },
                doc_change::UpdatePathResult::Destructured => match &change.ty {
                    /* We had selected siblings that belonged to a node that got
                     * destructued, so we need to select those same siblings in
                     * their new positions in their new parent. */
                    doc_change::ChangeType::Destructure { child_index, .. } => {
                        first_child+= *child_index;
                        last_child+= *child_index;
                        Mode::SiblingRange(path, first_child, last_child)
                    },
                    _ => panic!("got UpdatePathResult::Destructured from a non-Destructure type Change"),
                },
                doc_change::UpdatePathResult::Deleted => {
                    selection_changed = true;
                    Mode::Empty
                },
            },
            Mode::All => Mode::All,
        };
        
        selection_changed
    }
}

#[derive(Debug, Clone)]
pub enum Change {
    DocumentUpdated(sync::Arc<document::Document>),

    Clear,
    
    SetSingle(sync::Arc<document::Document>, structure::Path),
    AddSingle(sync::Arc<document::Document>, structure::Path),
    
    SetRange(sync::Arc<document::Document>, structure::Path, structure::Path),
    AddRange(sync::Arc<document::Document>, structure::Path, structure::Path),
    
    SelectAll,
}

#[derive(Debug, Clone)]
pub struct ChangeRecord {
    pub document_updated: Option<(u64, sync::Arc<document::Document>)>,
    pub selection_changed: bool,
}

#[derive(Debug, Clone)]
pub enum ApplyError {
    NodeDeleted
}

impl versioned::Versioned for Selection {
    type Change = Change;

    fn version(&self) -> &versioned::Version<Selection> {
        &self.version
    }

    fn version_mut(&mut self) -> &mut versioned::Version<Selection> {
        &mut self.version
    }
}

pub type Host = versioned::Host<Selection>;

impl versioned::Change<Selection> for Change {
    type ApplyError = ApplyError;
    type ApplyRecord = ChangeRecord;

    fn apply(mut self, selection: &mut Selection) -> Result<(Change, ChangeRecord), ApplyError> {
        let record = match &mut self {
            Change::DocumentUpdated(new_document) => selection.update_document(new_document),

            Change::Clear => {
                selection.mode = Mode::Empty;
                
                ChangeRecord {
                    document_updated: None,
                    selection_changed: true
                }
            },

            Change::SetSingle(document, ref mut path) => {
                let mut cr = selection.update_document(document);

                selection.document.changes_since_fallible(&document.clone(), &mut |doc, change| {
                    *document = doc.clone();
                    match change.update_path(path) {
                        doc_change::UpdatePathResult::Unmoved | doc_change::UpdatePathResult::Moved => Ok(()),
                        doc_change::UpdatePathResult::Deleted | doc_change::UpdatePathResult::Destructured => Err(ApplyError::NodeDeleted),
                    }
                })?;

                selection.mode = Mode::Single(path.clone());
                
                cr.selection_changed = true;
                cr
            },

            Change::AddSingle(document, ref mut path) => {
                let mut cr = selection.update_document(document);

                selection.document.changes_since_fallible(&document.clone(), &mut |doc, change| {
                    *document = doc.clone();
                    match change.update_path(path) {
                        doc_change::UpdatePathResult::Unmoved | doc_change::UpdatePathResult::Moved => Ok(()),
                        doc_change::UpdatePathResult::Deleted | doc_change::UpdatePathResult::Destructured => Err(ApplyError::NodeDeleted),
                    }
                })?;

                let (first, last) = match &selection.mode {
                    Mode::Empty => {
                        selection.mode = Mode::Single(path.clone());
                        cr.selection_changed = true;
                        return Ok((self, cr))
                    },
                    Mode::Single(path) => (path.clone(), path.clone()),
                    Mode::SiblingRange(parent, first_child, last_child) => {
                        let mut first = parent.clone(); first.push(*first_child);
                        let mut  last = parent.clone();  last.push(* last_child);
                        (first, last)
                    },
                    Mode::All => {
                        return Ok((self, cr))
                    },
                };
                
                let first = std::cmp::min(&first, path);
                let last = std::cmp::max(&last, path);

                selection.mode = Mode::new_range_between(first, last);
                
                cr.selection_changed = true;
                cr
            },
            
            Change::SetRange(document, ref mut first, ref mut last) => {
                let mut cr = selection.update_document(document);

                selection.document.changes_since_fallible(&document.clone(), &mut |doc, change| {
                    *document = doc.clone();
                    match change.update_path(first) {
                        doc_change::UpdatePathResult::Unmoved | doc_change::UpdatePathResult::Moved => {},
                        doc_change::UpdatePathResult::Deleted | doc_change::UpdatePathResult::Destructured => return Err(ApplyError::NodeDeleted),
                    }
                    match change.update_path(last) {
                        doc_change::UpdatePathResult::Unmoved | doc_change::UpdatePathResult::Moved => {},
                        doc_change::UpdatePathResult::Deleted | doc_change::UpdatePathResult::Destructured => return Err(ApplyError::NodeDeleted),
                    }
                    Ok(())
                })?;

                selection.mode = Mode::new_range_between(first, last);
                
                cr.selection_changed = true;
                cr                
            },

            Change::AddRange(document, ref mut first, ref mut last) => {
                let mut cr = selection.update_document(document);

                selection.document.changes_since_fallible(&document.clone(), &mut |doc, change| {
                    *document = doc.clone();
                    match change.update_path(first) {
                        doc_change::UpdatePathResult::Unmoved | doc_change::UpdatePathResult::Moved => {},
                        doc_change::UpdatePathResult::Deleted | doc_change::UpdatePathResult::Destructured => return Err(ApplyError::NodeDeleted),
                    }
                    match change.update_path(last) {
                        doc_change::UpdatePathResult::Unmoved | doc_change::UpdatePathResult::Moved => {},
                        doc_change::UpdatePathResult::Deleted | doc_change::UpdatePathResult::Destructured => return Err(ApplyError::NodeDeleted),
                    }
                    Ok(())
                })?;

                let (current_first, current_last) = match &selection.mode {
                    Mode::Empty => {
                        selection.mode = Mode::new_range_between(first, last);
                        cr.selection_changed = true;
                        return Ok((self, cr))
                    },
                    Mode::Single(path) => (path.clone(), path.clone()),
                    Mode::SiblingRange(parent, first_child, last_child) => {
                        let mut first = parent.clone(); first.push(*first_child);
                        let mut  last = parent.clone();  last.push(* last_child);
                        (first, last)
                    },
                    Mode::All => {
                        return Ok((self, cr))
                    },
                };
                
                let first = std::cmp::min(&*first, &current_first);
                let last = std::cmp::max(&*last, &current_last);

                selection.mode = Mode::new_range_between(first, last);
                
                cr.selection_changed = true;
                cr
            },
            
            Change::SelectAll => {
                selection.mode = Mode::All;
                ChangeRecord {
                    document_updated: None,
                    selection_changed: true,
                }
            },
        };

        Ok((self, record))
    }
}
