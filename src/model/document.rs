pub mod structure;

use std::string;
use std::sync;
use std::task;
use std::vec;

use crate::util;
use crate::model::addr;
use crate::model::datapath;
use crate::model::space::AddressSpace;

extern crate imbl;
//use imbl::ordmap;

pub type DocumentRef<'a> = owning_ref::OwningRef<sync::RwLockReadGuard<'a, DocumentHostInterior>, Document>;

#[derive(Clone)]
enum UndoOp {
    //ModifyBreak,
    //DeleteBreak,
    AddFilter(datapath::Filter),
}

#[derive(Clone)]
pub struct Document {
    pub root: sync::Arc<structure::Node>,
    pub datapath: datapath::DataPath,
    id: u64,
    layout_generation: u64,
    datapath_generation: u64,
}

static NEXT_DOCUMENT_ID: sync::atomic::AtomicU64 = sync::atomic::AtomicU64::new(1);
static NEXT_LAYOUT_GENERATION: sync::atomic::AtomicU64 = sync::atomic::AtomicU64::new(1);
static NEXT_DATAPATH_GENERATION: sync::atomic::AtomicU64 = sync::atomic::AtomicU64::new(1);

pub struct DocumentHostInterior { // implementation detail...
    current: Document,
    undo: vec::Vec<(UndoOp, Document)>,
}

pub struct DocumentHost {
    notifier: util::Notifier,
    interior: sync::RwLock<DocumentHostInterior>,
}

impl DocumentHost {
    pub fn new(doc: Document) -> DocumentHost {
        DocumentHost {
            notifier: util::Notifier::new(),
            interior: sync::RwLock::new(DocumentHostInterior {
                current: doc,
                undo: vec::Vec::new(),
            })
        }
    }

    pub fn wait(&self, cx: &task::Context) {
        self.notifier.enroll(cx);
    }
    
    pub fn get_document(&self) -> DocumentRef {
        owning_ref::OwningRef::new(self.interior.read().unwrap()).map(|r| &r.current)
    }

    /*
    /// Inserts a break or replaces an existing one.
    pub fn insert_break(&self, brk: brk::Break) {
        self.modify_breaks(UndoOp::ModifyBreak, |doc| {
            doc.breaks.insert(brk.addr, sync::Arc::new(brk));
            Result::<(), ()>::Ok(())
        }).expect("insert break has no failure case");
    }
    
    /// Removes a break.
    pub fn delete_break(&self, addr: &addr::Address) -> Result<(), BreakDeletionError> {
        if *addr == addr::unit::NULL {
            return Err(BreakDeletionError::IsZeroBreak);
        }
        
        self.modify_breaks(UndoOp::DeleteBreak, |doc| {
            match doc.breaks.remove(addr) {
                Some(_) => Ok(()),
                None => Err(BreakDeletionError::NotFound)
            }
        })
}
    */

    pub fn patch_byte(&self, location: u64, patch: u8) {
        self.apply_filter(datapath::OverwriteFilter {
            offset: location,
            bytes: vec![patch]
        }.to_filter())
    }

    pub fn insert_byte(&self, location: u64, patch: u8) {
        self.apply_filter(datapath::InsertFilter {
            offset: location,
            bytes: vec![patch]
        }.to_filter())
    }

    pub fn apply_filter(&self, filter: datapath::Filter) {
        let mut interior = self.interior.write().unwrap();

        let last_undo = interior.undo.last_mut();
        
        /* Try to stack this filter with the most recent one. */
        let stack = match last_undo {
            /* If the last undo operation was a filter, try to stack with it. */
            Some((UndoOp::AddFilter(ref mut undo_filter), ref undo_doc)) => datapath::Filter::stack(undo_filter, &filter).map(|se| (undo_filter, undo_doc, se)).ok_or(filter),
            _ => Err(filter)
        };

        match stack {
            /* If we successfully stacked, replace the undo op's filter with the
             * new stacked version, revert the document, and apply the new
             * stacked filter to the document in place of the old filter. */
            Ok((undo_filter, undo_doc, stacked_filter)) => {
                *undo_filter = stacked_filter.clone();
                interior.current = undo_doc.clone();
                interior.current.datapath.push_back(stacked_filter);
            },
            
            /* Stacking failed. Just add a new undo op. */
            Err(filter) => {
                let old_doc = interior.current.clone();
                interior.undo.push((UndoOp::AddFilter(filter.clone()), old_doc));
                interior.current.datapath.push_back(filter);
            }
        }

        interior.current.datapath_generation = NEXT_DATAPATH_GENERATION.fetch_add(1, sync::atomic::Ordering::SeqCst);
        
        self.notifier.notify();
    }

    /*
    fn modify_breaks<E, T, F>(&self, op: UndoOp, cb: F) -> Result<T, E>
    where F: FnOnce(&mut Document) -> Result<T, E> {
        let mut interior = self.interior.write().unwrap();
        let interior_ref = &mut *interior; // rustc can't see splitting borrow through MutexGuard
        
        match match interior_ref.undo.last_mut() {
            Some((old_op, _)) => match old_op.stack_with(&op) {
                Some(new_op) => Some((old_op, new_op)),
                None => None,
            },
            None => None,
        } {
            Some((op_target, op_value)) => {
                cb(&mut interior_ref.current).map(|v| {
                    *op_target = op_value;
                    v
                })
            }
            None => {
                let undo = interior_ref.current.clone();
                cb(&mut interior_ref.current).map(|v| {
                    interior_ref.undo.push((op, undo));
                    v
                })
            }
        }.map(|v| {
            interior_ref.current.layout_generation = NEXT_LAYOUT_GENERATION.fetch_add(1, sync::atomic::Ordering::SeqCst);
            self.notifier.notify();
            v
        })
}
    */
}
    
impl Document {
    pub fn new(space: sync::Arc<dyn AddressSpace + Send + Sync>) -> Document {
        let mut datapath = datapath::DataPath::new();
        datapath.push_back(datapath::LoadSpaceFilter::new(space, 0, 0).to_filter());
        
        Document {
            root: sync::Arc::new(structure::Node {
                name: "root".to_string(),
                size: addr::unit::REAL_MAX,
                title_display: structure::TitleDisplay::Major,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::Hexdump(16),
                locked: true,
                children: vec::Vec::new()
            }),
            datapath,
            id: NEXT_DOCUMENT_ID.fetch_add(1, sync::atomic::Ordering::SeqCst),
            layout_generation: 0,
            datapath_generation: 0,
        }
    }

    pub fn invalid() -> Document {
        Document {
            root: sync::Arc::new(structure::Node {
                name: "root".to_string(),
                size: addr::unit::REAL_MAX,
                title_display: structure::TitleDisplay::Major,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::Hexdump(16),
                locked: true,
                children: vec::Vec::new()
            }),
            datapath: datapath::DataPath::new(),
            id: 0,
            layout_generation: 0,
            datapath_generation: 0,
        }
    }

    pub fn is_outdated(&self, other: &Self) -> bool {
        self.id != other.id ||
            self.layout_generation != other.layout_generation ||
            self.datapath_generation != other.datapath_generation
    }

    pub fn is_layout_outdated(&self, other: &Self) -> bool {
        self.id != other.id ||
            self.layout_generation != other.layout_generation
    }

    pub fn is_datapath_outdated(&self, other: &Self) -> bool {
        self.id != other.id ||
            self.datapath_generation != other.datapath_generation
    }

    pub fn get_layout_generation_for_debug(&self) -> u64 {
        self.layout_generation
    }

    pub fn get_datapath_generation_for_debug(&self) -> u64 {
        self.datapath_generation
    }
}

impl UndoOp {
    fn stack_with(&self, op: &UndoOp) -> Option<UndoOp> {
        match (self, op) {
            /* AddFilter is handled separately */
            (_, _) => None,
        }
    }

    fn describe(&self) -> string::String {
        match self {
            UndoOp::AddFilter(filter) => format!("add filter: {}", filter.human_details()), // TODO
        }
    }
}
