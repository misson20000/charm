pub mod brk;
pub mod cursor;
pub mod line_group;
pub mod window;

use std::string;
use std::sync;
use std::vec;

use crate::addr;
use crate::space::AddressSpace;

extern crate im;
use im::ordmap;

type BreakMap = ordmap::OrdMap<addr::Address, sync::Arc<brk::Break>>;
type PatchMap = ordmap::OrdMap<u64, Patch>;

#[derive(Clone, Copy)]
enum UndoOp {
    ModifyBreak,
    DeleteBreak,
    Patch(u64, u64),
}

#[derive(Clone)]
pub struct Listing {
    space: sync::Arc<dyn AddressSpace>,
    breaks: BreakMap,
    patches: PatchMap,
    
    redo: Option<sync::Arc<Listing>>,
    undo: Option<(UndoOp, sync::Arc<Listing>)>,
    // TODO: undo depth limit
}

#[derive(Clone, Debug)]
pub struct Patch {
    bytes: vec::Vec<u8>,
}

impl Listing {
    pub fn new(space: sync::Arc<dyn AddressSpace + Send + Sync>) -> Listing {
        Listing {
            breaks: ordmap!{
                addr::unit::NULL => sync::Arc::new(brk::Break::new(
                    addr::unit::NULL,
                    Some(space.get_label()),
                    brk::BreakClass::Hex(brk::hex::HexBreak {
                        line_size: addr::Size::from(16)
                    })))
            },
            patches: ordmap!{
                0x3 => Patch {
                    bytes: vec![0x44, 0x55, 0x66, 0x77]
                }
            },
            space,
            redo: None,
            undo: None,
        }
    }
    
    fn clone_for_undo(&self, op: UndoOp) -> Option<(UndoOp, sync::Arc<Listing>)> {
        match &self.undo {
            Some((old_op, old_undo)) => match old_op.stack_with(op) {
                Some(new_op) => return Some((new_op, old_undo.clone())),
                None => (),
            },
            None => ()
        }

        let mut undo = (*self).clone();
        undo.redo = None;
        Some((op, sync::Arc::new(undo)))
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

    /// Inserts a break or replaces an existing one.
    pub fn insert_break(&mut self, brk: brk::Break) {
        self.undo = self.clone_for_undo(UndoOp::ModifyBreak);
        self.breaks.insert(brk.addr, sync::Arc::new(brk));
    }
    
    /// Removes a break.
    pub fn delete_break(&mut self, addr: &addr::Address) -> Result<(), BreakDeletionError> {
        if *addr == addr::unit::NULL {
            return Err(BreakDeletionError::IsZeroBreak);
        }

        let undo = self.clone_for_undo(UndoOp::DeleteBreak);
        match self.breaks.remove(addr) {
            Some(_) => {
                self.undo = undo;
                Ok(())
            },
            None => Err(BreakDeletionError::NotFound)
        }
    }

    pub fn patch_byte(&mut self, location: u64, patch: u8) {
        self.undo = self.clone_for_undo(UndoOp::Patch(location, 1));
        
        match self.patches.get_prev_mut(&location) {
            Some((k, v)) if k <= &location && location - k < v.bytes.len() as u64 => {
                v.bytes[(location - k) as usize] = patch;
            },
            Some((&k, v)) if k + v.bytes.len() as u64 == location => {
                v.bytes.push(patch);
                match self.patches.get_next_mut(&location) {
                    Some((&nk, _)) if k == location + 1 => {
                        let mut old_patch = self.patches.remove(&nk).unwrap();
                        self.patches[&k].bytes.append(&mut old_patch.bytes);
                    },
                    _ => ()
                }
            },
            Some((&k, _)) if k == (location + 1) => {
                let mut old_patch = self.patches.remove(&k).unwrap();
                old_patch.bytes.insert(0, patch);
                self.patches.insert(location, old_patch);
            },
            _ => {
                self.patches.insert(location, Patch { bytes: vec![patch] });
            }
        }
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
