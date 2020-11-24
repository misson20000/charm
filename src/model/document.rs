pub mod brk;

use std::string;
use std::sync;
use std::task;
use std::vec;

use crate::util;
use crate::model::addr;
use crate::model::space::AddressSpace;

extern crate im;
use im::ordmap;

pub type BreakMap = ordmap::OrdMap<addr::Address, sync::Arc<brk::Break>>;
pub type PatchMap = ordmap::OrdMap<u64, Patch>;
pub type DocumentRef<'a> = owning_ref::OwningRef<sync::RwLockReadGuard<'a, DocumentHostInterior>, Document>;

#[derive(Clone, Copy)]
enum UndoOp {
    ModifyBreak,
    DeleteBreak,
    Patch(u64, u64),
}

#[derive(Clone)]
pub struct Document {
    space: sync::Arc<dyn AddressSpace>,
    breaks: BreakMap,
    patches: PatchMap,
}

pub struct DocumentHostInterior { // implementation detail...
    current: Document,
    undo: vec::Vec<(UndoOp, Document)>,
}

pub struct DocumentHost {
    notifier: util::Notifier,
    interior: sync::RwLock<DocumentHostInterior>,
}

#[derive(Clone, Debug)]
pub struct Patch {
    pub bytes: vec::Vec<u8>,
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
        self.mutating_transaction(UndoOp::ModifyBreak, |doc| {
            doc.breaks.insert(brk.addr, sync::Arc::new(brk));
            Result::<(), ()>::Ok(())
        }).expect("insert break has no failure case");
    }
    
    /// Removes a break.
    pub fn delete_break(&self, addr: &addr::Address) -> Result<(), BreakDeletionError> {
        if *addr == addr::unit::NULL {
            return Err(BreakDeletionError::IsZeroBreak);
        }
        
        self.mutating_transaction(UndoOp::DeleteBreak, |doc| {
            match doc.breaks.remove(addr) {
                Some(_) => Ok(()),
                None => Err(BreakDeletionError::NotFound)
            }
        })
    }

    pub fn patch_byte(&self, location: u64, patch: u8) {
        self.mutating_transaction(UndoOp::Patch(location, 1), |doc| {
            match doc.patches.get_prev_mut(&location) {
                Some((k, v)) if k <= &location && location - k < v.bytes.len() as u64 => {
                    v.bytes[(location - k) as usize] = patch;
                },
                Some((&k, v)) if k + v.bytes.len() as u64 == location => {
                    v.bytes.push(patch);
                    match doc.patches.get_next_mut(&location) {
                        Some((&nk, _)) if k == location + 1 => {
                            let mut old_patch = doc.patches.remove(&nk).unwrap();
                            doc.patches[&k].bytes.append(&mut old_patch.bytes);
                        },
                        _ => ()
                    }
                },
                Some((&k, _)) if k == (location + 1) => {
                    let mut old_patch = doc.patches.remove(&k).unwrap();
                    old_patch.bytes.insert(0, patch);
                    doc.patches.insert(location, old_patch);
                },
                _ => {
                    doc.patches.insert(location, Patch { bytes: vec![patch] });
                }
            };
            Result::<(),()>::Ok(())
        }).expect("patch has no failure case");
    }

    fn mutating_transaction<E, T, F>(&self, op: UndoOp, cb: F) -> Result<T, E>
    where F: FnOnce(&mut Document) -> Result<T, E> {
        let mut interior = self.interior.write().unwrap();
        let interior_ref = &mut *interior; // rustc can't see splitting borrow through MutexGuard
        
        match match interior_ref.undo.last_mut() {
            Some((old_op, _)) => match old_op.stack_with(op) {
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
            self.notifier.notify();
            v
        })
    }
}
    
impl Document {
    pub fn new(space: sync::Arc<dyn AddressSpace + Send + Sync>) -> Document {
        Document {
            breaks: ordmap!{
                addr::unit::NULL => sync::Arc::new(brk::Break::new(
                    addr::unit::NULL,
                    Some(space.get_label()),
                    brk::BreakClass::Hex(brk::hex::HexBreak {
                        line_size: addr::Size::from(16)
                    })))
            },
            patches: ordmap!{
                /*
                0x3 => Patch {
                    bytes: vec![0x44, 0x55, 0x66, 0x77]
                }*/
            },
            space,
        }
    }
        
    // read only, please
    pub fn get_space(&self) -> &sync::Arc<dyn AddressSpace> {
        &self.space
    }
    
    // read only, please
    pub fn get_breaks(&self) -> &BreakMap {
        &self.breaks
    }

    // read only, please
    pub fn get_patches(&self) -> &PatchMap {
        &self.patches
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
    fn stack_with(&self, op: UndoOp) -> Option<UndoOp> {
        match (*self, op) {
            (UndoOp::Patch(old_loc, old_n), UndoOp::Patch(new_loc, new_n)) if new_loc == old_loc || new_loc == old_loc + 1 => Some(UndoOp::Patch(new_loc, old_n + new_n)),
            _ => None,
        }
    }

    fn describe(&self) -> string::String {
        match self {
            UndoOp::ModifyBreak => "modify break".to_string(),
            UndoOp::DeleteBreak => "delete break".to_string(),
            UndoOp::Patch(_loc, num) => format!("patch {} bytes", num),
        }
    }
}

impl PartialEq for Document {
    fn eq(&self, other: &Self) -> bool {
        sync::Arc::ptr_eq(&self.space, &other.space) &&
            self.breaks.ptr_eq(&other.breaks) &&
            self.patches.ptr_eq(&other.patches)
    }
}

impl Eq for Document {
}
