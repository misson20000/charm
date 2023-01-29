pub mod change;
pub mod search;
pub mod structure;

use std::future;
use std::pin;
use std::sync;
use std::task;
use std::vec;

use crate::util;
use crate::model::addr;
use crate::model::datapath;
use crate::model::space::AddressSpace;

#[derive(Clone)]
pub struct Document {
    pub previous: Option<(sync::Arc<Document>, change::Change)>,
    pub root: sync::Arc<structure::Node>,
    pub datapath: datapath::DataPath,
    uid: u64, //< unique ID
    generation: u64,
}

static NEXT_DOCUMENT_ID: sync::atomic::AtomicU64 = sync::atomic::AtomicU64::new(1);
static NEXT_GENERATION: sync::atomic::AtomicU64 = sync::atomic::AtomicU64::new(1);

pub struct DocumentHost {
    notifier: util::Notifier,
    current: arc_swap::ArcSwap<Document>,
}

impl DocumentHost {
    pub fn new(doc: Document) -> DocumentHost {
        DocumentHost {
            notifier: util::Notifier::new(),
            current: arc_swap::ArcSwap::from(sync::Arc::new(doc)),
        }
    }

    pub fn enroll(&self, cx: &task::Context) {
        self.notifier.enroll(cx);
    }

    pub fn wait_for_update<'a>(&'a self, current: &'_ Document) -> DocumentUpdateFuture<'a> {
        DocumentUpdateFuture {
            host: self,
            generation: current.generation,
        }
    }
    
    pub fn borrow(&self) -> arc_swap::Guard<sync::Arc<Document>> {
        self.current.load()
    }

    pub fn get(&self) -> sync::Arc<Document> {
        self.current.load_full()
    }

    /*
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
        let mut interior = self.interior.write();

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
     */

    pub fn change(&self, change: change::Change) -> Result<(), change::ApplyError> {
        let old = self.current.load();
        let new = sync::Arc::new(change::apply_structural_change(&old, change)?);
        let swapped = self.current.compare_and_swap(&*old, new.clone());
            
        if !sync::Arc::ptr_eq(&*old, &swapped) {
            /* very sad, another thread updated the document. need to fish the change back out of our fake new document and try again. */
            return self.change((*new).clone().previous.unwrap().1)
        }
        
        self.notifier.notify();
        Ok(())
    }

    pub fn alter_node(&self, doc: &Document, path: structure::Path, props: structure::Properties) -> Result<(), change::ApplyError> {
        let change = change::Change {
            ty: change::ChangeType::AlterNode(path, props),
            generation: doc.generation,
        };

        self.change(change)
    }

    pub fn insert_node(&self, doc: &Document, path: structure::Path, after_child: usize, offset: addr::Address, node: sync::Arc<structure::Node>) -> Result<(), change::ApplyError> {
        let change = change::Change {
            ty: change::ChangeType::InsertNode(path, after_child, offset, node),
            generation: doc.generation,
        };

        self.change(change)
    }

    pub fn nest(&self, doc: &Document, path: structure::Path, first_sibling: usize, last_sibling: usize, props: structure::Properties) -> Result<(), change::ApplyError> {
        let change = change::Change {
            ty: change::ChangeType::Nest(path, first_sibling, last_sibling, props),
            generation: doc.generation,
        };

        self.change(change)
    }
}

pub struct DocumentUpdateFuture<'a> {
    host: &'a DocumentHost,
    generation: u64,
}

impl<'a> future::Future for DocumentUpdateFuture<'a> {
    type Output = sync::Arc<Document>;

    fn poll(self: pin::Pin<&mut Self>, cx: &mut task::Context<'_>) -> task::Poll<Self::Output> {
        let guard = self.host.current.load();
        if guard.generation != self.generation {
            /* fast path */
            task::Poll::Ready(arc_swap::Guard::into_inner(guard))
        } else {
            /* slow path. need to enroll for change notifications... */
            std::mem::drop(guard);
            self.host.enroll(cx);

            /* check whether the document was updated while we were enrolling */
            let guard = self.host.current.load();
            if guard.generation != self.generation {
                /* document was updated while we were enrolling */
                task::Poll::Ready(arc_swap::Guard::into_inner(guard))
            } else {
                /* still no change... we'll pick it up when our task gets woken again. */
                task::Poll::Pending
            }
        }
    }
}

impl std::fmt::Debug for DocumentHost {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DocumentHost")
            .field("generation", &self.borrow().generation)
            .finish_non_exhaustive()
    }
}

#[derive(Debug)]
pub enum LoadForTestingError {
    IoError(std::io::Error),
    XmlError(roxmltree::Error),
}

impl From<std::io::Error> for LoadForTestingError {
    fn from(e: std::io::Error) -> LoadForTestingError {
        LoadForTestingError::IoError(e)
    }
}

impl From<roxmltree::Error> for LoadForTestingError {
    fn from(e: roxmltree::Error) -> LoadForTestingError {
        LoadForTestingError::XmlError(e)
    }
}

impl Document {
    pub fn new(space: sync::Arc<dyn AddressSpace + Send + Sync>) -> Document {
        let mut datapath = datapath::DataPath::new();
        datapath.push_back(datapath::LoadSpaceFilter::new(space, 0, 0).to_filter());
        
        Document {
            previous: None,
            root: sync::Arc::new(structure::Node {
                size: addr::unit::REAL_MAX,
                props: structure::Properties {
                    name: "root".to_string(),
                    title_display: structure::TitleDisplay::Major,
                    children_display: structure::ChildrenDisplay::Full,
                    content_display: structure::ContentDisplay::Hexdump(16.into()),
                    locked: true,
                },
                children: vec::Vec::new()
            }),
            datapath,
            uid: NEXT_DOCUMENT_ID.fetch_add(1, sync::atomic::Ordering::Relaxed),
            generation: 0,
        }
    }

    pub fn load_from_testing_structure<P: AsRef<std::path::Path>>(path: P) -> Result<Document, LoadForTestingError> {
        let xml = std::fs::read_to_string(path)?;
        let xml = roxmltree::Document::parse(&xml)?;
        let tc = crate::logic::tokenizer::xml::Testcase::from_xml(&xml);

        Ok(Document {
            previous: None,
            root: tc.structure,
            datapath: datapath::DataPath::new(),
            uid: NEXT_DOCUMENT_ID.fetch_add(1, sync::atomic::Ordering::Relaxed),
            generation: NEXT_GENERATION.fetch_add(1, sync::atomic::Ordering::Relaxed),
        })
    }

    #[cfg(test)]
    pub fn new_for_structure_test(root: sync::Arc<structure::Node>) -> Document {
        Document {
            previous: None,
            root,
            datapath: datapath::DataPath::new(),
            uid: NEXT_DOCUMENT_ID.fetch_add(1, sync::atomic::Ordering::Relaxed),
            generation: NEXT_GENERATION.fetch_add(1, sync::atomic::Ordering::Relaxed),
        }
    }
    
    pub fn invalid() -> Document {
        Document {
            previous: None,
            root: sync::Arc::new(structure::Node {
                size: addr::unit::REAL_MAX,
                props: structure::Properties {
                    name: "root".to_string(),
                    title_display: structure::TitleDisplay::Major,
                    children_display: structure::ChildrenDisplay::Full,
                    content_display: structure::ContentDisplay::Hexdump(16.into()),
                    locked: true,
                },
                children: vec::Vec::new()
            }),
            datapath: datapath::DataPath::new(),
            uid: 0,
            generation: 0,
        }
    }

    pub fn is_outdated(&self, other: &Self) -> bool {
        self.uid != other.uid ||
            self.generation != other.generation
    }

    pub fn get_generation_for_debug(&self) -> u64 {
        self.generation
    }

    pub fn lookup_node(&self, path: &structure::Path) -> (&sync::Arc<structure::Node>, addr::Address) {
        let mut current_node = &self.root;
        let mut node_addr = addr::unit::NULL;

        for i in path {
            let childhood = &current_node.children[*i];
            node_addr+= childhood.offset.to_size();
            current_node = &childhood.node;
        }

        (current_node, node_addr)
    }

    pub fn search_addr<A: Into<addr::Address>>(&self, addr: A, traversal: search::Traversal) -> Result<search::AddressSearch<'_>, search::SetupError> {
        search::AddressSearch::new(self, addr.into(), traversal)
    }
}

impl std::fmt::Debug for Document {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Document")
            .field("uid", &self.uid)
            .field("generation", &self.generation)
            .finish_non_exhaustive()
    }
}
