use std::cell;
use std::sync;
use std::vec;

use gtk::gio;
use gtk::glib;
use gtk::prelude::*;
use gtk::subclass::prelude::*;

use crate::model::addr;
use crate::model::document;
use crate::model::document::change;
use crate::model::document::structure;
use crate::model::selection;
use crate::model::versioned::Versioned;

glib::wrapper! {
    /// List model representing the children of a particular
    /// [structure::Node]. Does not subscribe to document updates and
    /// must be manually updated via [StructureListModel::update, but
    /// will recursively update child [super::NodeItem]s.
    pub struct StructureListModel(ObjectSubclass<imp::StructureListModel>)
        @implements gio::ListModel;
}

mod imp {
    use std::cell;
    use std::sync;
    use std::vec;
    
    use gtk::gio;
    use gtk::glib;
    use gtk::subclass::prelude::*;
    use gtk::prelude::*;

    use crate::catch_panic;
    use crate::model::addr;
    use crate::model::document;
    use crate::model::document::structure;

    #[derive(Debug)]
    pub struct StructureListModelInterior {
        pub path: structure::Path,
        pub children: vec::Vec<super::super::NodeItem>,
        pub address: addr::AbsoluteAddress,
        pub document_host: sync::Arc<document::DocumentHost>,
        pub document: sync::Arc<document::Document>,
        pub deleted: bool,
    }

    #[derive(Default)]
    pub struct StructureListModel {
        pub interior: once_cell::unsync::OnceCell<cell::RefCell<StructureListModelInterior>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for StructureListModel {
        const NAME: &'static str = "CharmStructureListModel";
        type Type = super::StructureListModel;
        type Interfaces = (gio::ListModel,);
    }

    impl ObjectImpl for StructureListModel {
    }

    impl ListModelImpl for StructureListModel {
        fn item_type(&self) -> glib::Type {
            /* FFI CALLBACK: assumed panic-safe */
            super::super::NodeItem::static_type()
        }

        fn n_items(&self) -> u32 {
            catch_panic! {
                @default(0);
                
                self.interior.get().map_or(0, |i| i.borrow().children.len() as u32)
            }
        }

        fn item(&self, position: u32) -> Option<glib::Object> {
            catch_panic! {
                @default(None);
                
                self.interior.get().and_then(|i| {
                    let i = i.borrow();
                    i.children.get(position as usize).map(|ch| ch.clone().upcast())
                })
            }
        }
    }
}

impl StructureListModel {
    pub fn from_node_info(info: &super::NodeInfo) -> Self {
        let model: Self = glib::Object::builder().build();

        model.imp().interior.set(cell::RefCell::new(imp::StructureListModelInterior {
            path: info.path.clone(),
            children: info.node.children[..].iter().enumerate().map(|tuple| {
                let (i, ch) = tuple;
                
                let mut path = info.path.clone();
                path.push(i);

                super::NodeItem::new(super::NodeInfo {
                    path,
                    node: ch.node.clone(),
                    props: ch.node.props.clone(),
                    offset: ch.offset,
                    address: info.address + ch.offset,
                    document_host: info.document_host.clone(),
                    document: info.document.clone()
                })                
            }).collect(),
            address: info.address,
            document_host: info.document_host.clone(),
            document: info.document.clone(),
            deleted: false,
        })).unwrap();
        
        model
    }

    fn port_change(&self, new_doc: &sync::Arc<document::Document>, change_record: &change::ApplyRecord) -> Option<(u32, u32, u32)> {
        let mut i = self.imp().interior.get().unwrap().borrow_mut();

        i.document = new_doc.clone();
        
        match change_record.update_path(&mut i.path) {
            change::UpdatePathResult::Unmoved | change::UpdatePathResult::Moved => {
            },
            change::UpdatePathResult::Deleted | change::UpdatePathResult::Destructured => {
                i.deleted = true;
                i.path.clear();
                i.children.clear();
                i.address = addr::AbsoluteAddress::NULL;
                return None;
            },
        };
        
        let (new_node, addr) = new_doc.lookup_node(&i.path);

        i.address = addr;

        /* Some((index, removed, added)) */
        let items_changed = match &change_record {
            /* Was one of our children altered? */
            change::ApplyRecord::AlterNode { path, node: new_node_from_record } if i.path.len() + 1 == path.len() && path[0..i.path.len()] == i.path[..] => {
                let index = path[i.path.len()];
                let child_item = &i.children[index];
                let childhood = &new_node.children[index];
                let document_host = i.document_host.clone();

                assert!(sync::Arc::ptr_eq(new_node_from_record, &childhood.node));

                child_item.stage(super::NodeInfo {
                    path: path.clone(),
                    node: childhood.node.clone(),
                    props: childhood.node.props.clone(),
                    offset: childhood.offset,
                    address: addr + childhood.offset,
                    document: new_doc.clone(),
                    document_host,
                });

                None
            },
            change::ApplyRecord::AlterNode { .. } => None,
            change::ApplyRecord::AlterNodesBulk { selection, prop_changes: _ } => {
                let i = &mut *i;
                if let Ok(mut walker) = selection.subtree_walker(&i.path, AlterNodesBulkNodeInfoUpdater(&mut i.children, &new_node)) {
                    while walker.visit_next().is_some() {
                    }
                }

                None
            },
            
            /* Did we get a new child? */
            change::ApplyRecord::InsertNode { parent: affected_path, index: affected_index, child: _ } if affected_path[..] == i.path[..] => {
                let childhood = &new_node.children[*affected_index];
                let document_host = i.document_host.clone();

                i.children.insert(*affected_index, super::NodeItem::new(super::NodeInfo {
                    path: vec![], /* will be fixed up later */
                    node: childhood.node.clone(),
                    props: childhood.node.props.clone(),
                    offset: childhood.offset,
                    address: addr + childhood.offset,
                    document: new_doc.clone(),
                    document_host,
                }));

                Some((*affected_index as u32, 0, 1))
            },
            change::ApplyRecord::InsertNode { .. } => None,

            /* Were some of our children nested? */
            change::ApplyRecord::Nest { range, .. } if range.parent == i.path => {
                let childhood = &new_node.children[range.first];
                let document_host = i.document_host.clone();
                
                let count_removed = i.children.splice(range.indices(), [super::NodeItem::new(super::NodeInfo {
                    path: vec![], /* will be fixed up later */
                    node: childhood.node.clone(),
                    props: childhood.node.props.clone(),
                    offset: childhood.offset,
                    address: addr + childhood.offset,
                    document: new_doc.clone(),
                    document_host,
                })]).count();

                Some((range.first as u32, count_removed as u32, 1))
            },
            change::ApplyRecord::Nest { .. } => None,

            /* Was one of our children destructured? */
            change::ApplyRecord::Destructure(dsr) if &dsr.parent == &i.path => {
                let document_host = i.document_host.clone();

                i.children.remove(dsr.child_index);

                let mut highest_affected_index = dsr.child_index;
                
                for inserted_index in &dsr.mapping {
                    let childhood = &new_node.children[*inserted_index];
                    
                    i.children.insert(*inserted_index, super::NodeItem::new(super::NodeInfo {
                        path: vec![], /* will be fixed up later */
                        node: childhood.node.clone(),
                        props: childhood.node.props.clone(),
                        offset: childhood.offset,
                        address: addr + childhood.offset,
                        document: new_doc.clone(),
                        document_host: document_host.clone(),
                    }));

                    highest_affected_index = inserted_index + 1;
                }

                let num_affected = highest_affected_index - dsr.child_index;

                Some((dsr.child_index as u32, (num_affected - dsr.mapping.len() + 1) as u32, num_affected as u32))
            },
            change::ApplyRecord::Destructure { .. } => None,

            /* Were some of our children deleted? */
            change::ApplyRecord::DeleteRange { range } if range.parent == i.path => {
                let count_removed = i.children.splice(range.indices(), []).count();

                Some((range.first as u32, count_removed as u32, 0))
            },
            change::ApplyRecord::DeleteRange { .. } => None,

            change::ApplyRecord::StackFilter { .. } => None,

            /* Taken care of in the next step as property notifications via stage. */
            change::ApplyRecord::Resize { .. } => None,

            /* Were nodes pasted into us? */
            change::ApplyRecord::Paste(par) if par.parent == i.path => {
                let document_host = i.document_host.clone();

                for pasted_index in &par.mapping {
                    let childhood = &new_node.children[*pasted_index];
                    
                    i.children.insert(*pasted_index, super::NodeItem::new(super::NodeInfo {
                        path: vec![], /* will be fixed up later */
                        node: childhood.node.clone(),
                        props: childhood.node.props.clone(),
                        offset: childhood.offset,
                        address: addr + childhood.offset,
                        document: new_doc.clone(),
                        document_host: document_host.clone(),
                    }));
                }

                let num_affected = par.dst_end.1 - par.dst_begin.1;

                Some((par.dst_begin.1 as u32, (num_affected - par.mapping.len()) as u32, num_affected as u32))
            },
            change::ApplyRecord::Paste(_) => None,
        };

        /* Fixup children's paths and node pointers */
        {
            let i = &mut *i;
            for (index, child) in i.children.iter_mut().enumerate() {
                let mut child_info = child.staged_info();
                child_info.path.splice(.., i.path.iter().cloned());
                child_info.path.push(index);
                child_info.document = new_doc.clone();
                child_info.node = new_node.children[index].node.clone();
                child.stage(child_info);
            }
        }

        std::mem::drop(i);

        items_changed
    }
    
    pub fn update(&self, new_doc: &sync::Arc<document::Document>) {
        if let Some(i) = self.imp().interior.get().map(|i| i.borrow()) {
            let old_doc = i.document.clone();
            drop(i);

            let mut items_changed_records = vec![];
            
            new_doc.changes_since(&old_doc, &mut |doc, change| {
                if let Some(items_changed) = self.port_change(doc, change) {
                    items_changed_records.push(items_changed);
                }
            });

            /* We need to avoid notifying gtk of its own property updates. Our system to do this works by filtering out
             * updates that match what GTK thinks the properties already are. Unfortunately, there are some cases where
             * GTK updates a property twice in quick succession, which breaks this filtering, so we need to coalesce
             * updates before deciding whether to filter them out or not. */
            
            for item in &self.imp().interior.get().unwrap().borrow().children {
                item.update_document(new_doc);
                item.trigger_notifies();                
            }

            for (index, removed, added) in items_changed_records {
                self.items_changed(index, removed, added);
            }
        }
    }
}

struct AlterNodesBulkNodeInfoUpdater<'a>(&'a mut vec::Vec<super::NodeItem>, &'a structure::Node);

impl<'tree, 'nodeinfo> selection::tree::TreeVisitor<'tree> for AlterNodesBulkNodeInfoUpdater<'nodeinfo> {
    type NodeContext = ();
    type VisitResult = ();

    fn root_context(&mut self, _root: &'tree sync::Arc<structure::Node>) -> Self::NodeContext {
        ()
    }

    /// Returning None skips descending.
    fn descend<'b>(&mut self, _parent: &'b mut Self::NodeContext, _node: &'tree sync::Arc<structure::Node>, child_index: usize, child_selected: bool) -> Option<Self::NodeContext> {
        if child_selected {
            let child = &mut self.0[child_index];
            let mut info = child.imp().info.get().unwrap().borrow().clone();

            /* Need to fetch new props from node from *new* tree, not old tree that Selection and TreeVisitor know of */
            info.props = self.1.children[child_index].node.props.clone();
            
            child.stage(info);
        }
        
        None
    }
    
    fn visit_child<'b>(&mut self, _parent_context: &'b mut Self::NodeContext, _node: &'tree sync::Arc<structure::Node>, child_index: usize) -> Self::VisitResult {
        let child = &mut self.0[child_index];
        let mut info = child.imp().info.get().unwrap().borrow().clone();

        info.props = self.1.children[child_index].node.props.clone();
        
        child.stage(info);
    }
    
    fn visit<'b>(&mut self, _context: &'b mut Self::NodeContext, _node: &'tree sync::Arc<structure::Node>) -> Self::VisitResult {
        ()
    }
    
    fn ascend<'b>(&mut self, _parent: &'b mut Self::NodeContext, _child: Self::NodeContext, _child_index: usize) {
    }
}
