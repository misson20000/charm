pub mod brk;

use std::string;
use std::sync;
use std::task;
use std::vec;

use crate::util;
use crate::model::addr;
use crate::model::datapath;
use crate::model::space::AddressSpace;

extern crate imbl;
use imbl::ordmap;

pub type BreakMap = ordmap::OrdMap<addr::Address, sync::Arc<brk::Break>>;
pub type DocumentRef<'a> = owning_ref::OwningRef<sync::RwLockReadGuard<'a, DocumentHostInterior>, Document>;

#[derive(Clone)]
enum UndoOp {
    ModifyBreak,
    DeleteBreak,
    AddFilter(datapath::Filter),
}

#[derive(Clone)]
pub struct Document {
    pub breaks: BreakMap,
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
}
    
impl Document {
    pub fn new(space: sync::Arc<dyn AddressSpace + Send + Sync>) -> Document {
        let breaks = ordmap!{
            addr::unit::NULL => sync::Arc::new(brk::Break::new(
                addr::unit::NULL,
                Some(space.get_label()),
                brk::BreakClass::Hex(brk::hex::HexBreak {
                    line_size: addr::Size::from(16)
                })))
        };
        
        let mut datapath = datapath::DataPath::new();
        datapath.push_back(datapath::LoadSpaceFilter::new(space, 0, 0).to_filter());
        
        Document {
            breaks,
            datapath,
            id: NEXT_DOCUMENT_ID.fetch_add(1, sync::atomic::Ordering::SeqCst),
            layout_generation: 0,
            datapath_generation: 0,
        }
    }

    pub fn invalid() -> Document {
        Document {
            breaks: ordmap!{},
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

pub trait BreakMapExt {
    fn break_at(&self, addr: addr::Address) -> &sync::Arc<brk::Break>;
    fn break_before<'a, 'b>(&'a self, brk: &'b sync::Arc<brk::Break>) -> Option<&'a sync::Arc<brk::Break>>;
    fn break_after<'a, 'b>(&'a self, brk: &'b sync::Arc<brk::Break>) -> Option<&'a sync::Arc<brk::Break>>;
    fn break_before_addr(&self, addr: addr::Address) -> Option<&sync::Arc<brk::Break>>;
    fn break_after_addr(&self, addr: addr::Address) -> Option<&sync::Arc<brk::Break>>;
    fn extents_at(&self, target: addr::Address) -> addr::Extent;
    fn extents_near(&self, target: addr::Address) -> addr::ExtentTriplet;
}

impl BreakMapExt for BreakMap {
    /// Gets the break at an address.
    fn break_at(&self, addr: addr::Address) -> &sync::Arc<brk::Break> {
        self.get_prev(&addr).expect("zero break should always exist").1
    }

    fn break_before<'a, 'b>(&'a self, brk: &'b sync::Arc<brk::Break>) -> Option<&'a sync::Arc<brk::Break>> {
        self.break_before_addr(brk.addr)
    }

    fn break_after<'a, 'b>(&'a self, brk: &'b sync::Arc<brk::Break>) -> Option<&'a sync::Arc<brk::Break>> {
        self.break_after_addr(brk.addr)
    }

    fn break_before_addr(&self, addr: addr::Address) -> Option<&sync::Arc<brk::Break>> {
        if addr == addr::unit::NULL {
            None
        } else {
            Some(self.get_prev(&(addr - addr::unit::BIT)).expect("zero break should always exist").1)
        }
    }

    fn break_after_addr(&self, addr: addr::Address) -> Option<&sync::Arc<brk::Break>> {
        if addr == addr::unit::REAL_END {
            None
        } else {
            self.get_next(&(addr + addr::unit::BIT)).map(|tuple| tuple.1)
        }
    }
    
    /// Gets the extents of the break containing the target address.
    fn extents_at(&self, target: addr::Address) -> addr::Extent {
        let at = self.get_prev(&target).expect("zero break should always exist").0;
        let next = self.get_next(&(target + addr::unit::BIT));
        
        match next {
            Some(next) => addr::Extent::between(*at, *next.0),
            None => addr::Extent::unbounded(*at)
        }
    }

    /// Gets the extents of the break containing the target address, and if they
    /// exist, the extents of the breaks before and after.
    fn extents_near(&self, target: addr::Address) -> addr::ExtentTriplet {
        let at = self.get_prev(&target).expect("zero break should always exist").0;
        let next = self.get_next(&(target + addr::unit::BIT)).map(|tuple| tuple.0);

        addr::ExtentTriplet {
            before: if *at == addr::unit::NULL {
                None
            } else {
                self.get_prev(&(*at - addr::unit::BIT)).map(|tuple| *tuple.0)
            },
            at: *at,
            after: match next {
                Some(next) => match self.get_next(&(*next + addr::unit::BIT)).map(|tuple| tuple.0) {
                    Some(next_next) => addr::ExtentTripletEnding::AfterBounded(*next, *next_next),
                    None => addr::ExtentTripletEnding::AfterUnbounded(*next),
                },
                None => addr::ExtentTripletEnding::AtUnbounded,
            }
        }
    }
}

pub enum BreakDeletionError {
    IsZeroBreak,
    NotFound,
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
            UndoOp::ModifyBreak => "modify break".to_string(),
            UndoOp::DeleteBreak => "delete break".to_string(),
            UndoOp::AddFilter(filter) => format!("add filter: {}", filter.human_details()), // TODO
        }
    }
}
