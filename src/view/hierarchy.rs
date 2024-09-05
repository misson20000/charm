use std::cell;
use std::sync;
use std::vec;

use gtk::gio;
use gtk::glib;
use gtk::prelude::*;
use gtk::subclass::prelude::*;

use crate::catch_panic;
use crate::model::addr;
use crate::model::document;
use crate::model::document::change;
use crate::model::document::structure;
use crate::model::selection;
use crate::model::versioned::Versioned;

/// Our TreeListModel doesn't update itself. This is so that we can guarantee that the selection model updates first by having it be the one that subscribes to SelectionHost updates and informs us when the document updates. We still need a document_host though so we can change the document when properties are edited.
pub fn create_tree_list_model(document_host: sync::Arc<document::DocumentHost>, document: sync::Arc<document::Document>, autoexpand: bool) -> gtk::TreeListModel {
    let root_model = RootListModel::new(document_host, document);
    
    let model = gtk::TreeListModel::new(root_model, false, autoexpand, |obj| catch_panic! {
        @default(None);

        Some(obj.downcast_ref::<NodeItem>().unwrap().imp().expand().upcast())
    });
    
    model
}

#[derive(Debug, Clone)]
pub struct NodeInfo {
    pub node: sync::Arc<structure::Node>,
    
    /* when we update a gobject property, it needs to be reflected immediately,
     * before document update happens. */
    pub props: structure::Properties,
    
    pub path: structure::Path,
    pub offset: addr::Address,
    pub address: addr::Address,
    pub document_host: sync::Arc<document::DocumentHost>,
    pub document: sync::Arc<document::Document>,
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

    use super::NodeInfo;

    #[derive(Debug)]
    pub struct StructureListModelInterior {
        pub path: structure::Path,
        pub children: vec::Vec<super::NodeItem>,
        pub address: addr::Address,
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
            super::NodeItem::static_type()
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
    
    #[derive(Default)]
    pub struct NodeItem {
        pub info: once_cell::unsync::OnceCell<cell::RefCell<NodeInfo>>,
        pub staged_info: cell::RefCell<Option<NodeInfo>>,
        pub expansion: cell::RefCell<glib::object::WeakRef<super::StructureListModel>>
    }

    #[glib::object_subclass]
    impl ObjectSubclass for NodeItem {
        const NAME: &'static str = "CharmNodeItem";
        type Type = super::NodeItem;
        type ParentType = glib::Object;
    }

    impl ObjectImpl for NodeItem {
        fn properties() -> &'static [glib::ParamSpec] {
            /* FFI CALLBACK: panic-safe */
            
            static PROPERTIES: once_cell::sync::Lazy<Vec<glib::ParamSpec>> =
                once_cell::sync::Lazy::new(|| vec![
                    glib::ParamSpecString::builder("name").build(),
                    glib::ParamSpecString::builder("addr").build(),
                    glib::ParamSpecString::builder("size").build(),
                    glib::ParamSpecString::builder("children-display").build(),
                ]);
            PROPERTIES.as_ref()
        }

        fn set_property(&self, _id: usize, value: &glib::Value, pspec: &glib::ParamSpec) {
            catch_panic! {
                /* we update our local copy of properties immediately. when the
                document update goes through, it will notify us, but we'll see
                that the new properties already match our local properties and we
                won't notify the properties as changed again. */

                let mut info = self.info.get().unwrap().borrow_mut();
                let old_info = info.clone();
                
                match pspec.name() {
                    "name" => info.props.name = value.get().unwrap(),
                    "children-display" => info.props.children_display = match value.get().unwrap() {
                        "n" => structure::ChildrenDisplay::None,
                        "s" => structure::ChildrenDisplay::Summary,
                        "f" => structure::ChildrenDisplay::Full,
                        _ => structure::ChildrenDisplay::Full,
                    },
                    _ => unimplemented!(),
                };

                if let Err(e) = info.document_host.change(info.document.alter_node(info.path.clone(), info.props.clone())) {
                    /* roll back */
                    println!("failed to alter node: {:?}", e);
                    std::mem::drop(info);
                    self.obj().stage(old_info);
                    self.obj().trigger_notifies();
                }
            }
        }

        fn property(&self, _id: usize, pspec: &glib::ParamSpec) -> glib::Value {
            catch_panic! {
                @default(glib::Value::from_type(glib::Type::INVALID));
                
                let info = self.info.get().unwrap().borrow();
                match pspec.name() {
                    "name" => glib::value::ToValue::to_value(&info.props.name),
                    "addr" => glib::value::ToValue::to_value(&format!("{}", info.address)),
                    "size" => glib::value::ToValue::to_value(&format!("{}", info.node.size)),
                    "children-display" => glib::value::ToValue::to_value(match info.props.children_display {
                        structure::ChildrenDisplay::None => "n",
                        structure::ChildrenDisplay::Summary => "s",
                        structure::ChildrenDisplay::Full => "f",
                    }),
                    _ => unimplemented!(),
                }
            }
        }
    }

    impl NodeItem {
        pub fn expand(&self) -> super::StructureListModel {
            let expansion = self.expansion.borrow_mut();
            match expansion.upgrade() {
                Some(slm) => slm,
                None => {
                    let slm = super::StructureListModel::from_node_info(&*self.info.get().unwrap().borrow());
                    expansion.set(Some(&slm));
                    slm
                }
            }
        }
    }

    #[derive(Default)]
    pub struct RootListModel {
        pub root_item: once_cell::unsync::OnceCell<super::NodeItem>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for RootListModel {
        const NAME: &'static str = "CharmRootListModel";
        type Type = super::RootListModel;
        type Interfaces = (gio::ListModel,);
    }

    impl ObjectImpl for RootListModel {
    }

    impl ListModelImpl for RootListModel {
        fn item_type(&self) -> glib::Type {
            /* FFI CALLBACK: assumed panic-safe */
            super::NodeItem::static_type()
        }

        fn n_items(&self) -> u32 {
            /* FFI CALLBACK: trivially panic-safe */
            1
        }
        
        fn item(&self, position: u32) -> Option<glib::Object> {
            catch_panic! {
                @default(None);
                
                assert_eq!(position, 0);
                self.root_item.get().map(|ch| ch.clone().upcast())
            }
        }
    }
}

glib::wrapper! {
    pub struct StructureListModel(ObjectSubclass<imp::StructureListModel>)
        @implements gio::ListModel;
}

glib::wrapper! {
    pub struct RootListModel(ObjectSubclass<imp::RootListModel>)
        @implements gio::ListModel;
}

glib::wrapper! {
    pub struct NodeItem(ObjectSubclass<imp::NodeItem>)
        ;
}

impl StructureListModel {
    fn from_node_info(info: &NodeInfo) -> Self {
        let model: Self = glib::Object::builder().build();

        model.imp().interior.set(cell::RefCell::new(imp::StructureListModelInterior {
            path: info.path.clone(),
            children: info.node.children[..].iter().enumerate().map(|tuple| {
                let (i, ch) = tuple;
                
                let mut path = info.path.clone();
                path.push(i);

                NodeItem::new(NodeInfo {
                    path,
                    node: ch.node.clone(),
                    props: ch.node.props.clone(),
                    offset: ch.offset,
                    address: info.address + ch.offset.to_size(),
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

    fn port_change(&self, new_doc: &sync::Arc<document::Document>, change: &change::Change) -> Option<(u32, u32, u32)> {
        let mut i = self.imp().interior.get().unwrap().borrow_mut();

        i.document = new_doc.clone();
        
        match change.update_path(&mut i.path) {
            change::UpdatePathResult::Unmoved | change::UpdatePathResult::Moved => {
            },
            change::UpdatePathResult::Deleted | change::UpdatePathResult::Destructured => {
                i.deleted = true;
                i.path.clear();
                i.children.clear();
                i.address = addr::unit::NULL;
                return None;
            },
        };
        
        let (new_node, addr) = new_doc.lookup_node(&i.path);

        i.address = addr;
        
        let items_changed = match &change.ty {
            /* Was one of our children altered? */
            change::ChangeType::AlterNode { path, props: new_props } if i.path.len() + 1 == path.len() && path[0..i.path.len()] == i.path[..] => {
                let index = path[i.path.len()];
                let child_item = i.children[index].clone();
                let childhood = &new_node.children[index];
                let document_host = i.document_host.clone();

                child_item.stage(NodeInfo {
                    path: path.clone(),
                    node: childhood.node.clone(),
                    props: new_props.clone(),
                    offset: childhood.offset,
                    address: addr + childhood.offset.to_size(),
                    document: new_doc.clone(),
                    document_host,
                });

                None
            },
            change::ChangeType::AlterNode { .. } => None,
            change::ChangeType::AlterNodesBulk { selection, prop_changes: _ } => {
                let i = &mut *i;
                if let Ok(mut walker) = selection.subtree_walker(&i.path, AlterNodesBulkNodeInfoUpdater(&mut i.children, &new_node)) {
                    while walker.visit_next().is_some() {
                    }
                }

                None
            },
            
            /* Did we get a new child? */
            change::ChangeType::InsertNode { parent: affected_path, index: affected_index, child: _ } if affected_path[..] == i.path[..] => {
                let childhood = &new_node.children[*affected_index];
                let document_host = i.document_host.clone();

                i.children.insert(*affected_index, NodeItem::new(NodeInfo {
                    path: vec![], /* will be fixed up later */
                    node: childhood.node.clone(),
                    props: childhood.node.props.clone(),
                    offset: childhood.offset,
                    address: addr + childhood.offset.to_size(),
                    document: new_doc.clone(),
                    document_host,
                }));

                Some((*affected_index as u32, 0, 1))
            },
            change::ChangeType::InsertNode { .. } => None,

            /* Were some of our children nested? */
            change::ChangeType::Nest { range, extent: _, props: _ } if range.parent == i.path => {
                let childhood = &new_node.children[range.first];
                let document_host = i.document_host.clone();
                
                let count_removed = i.children.splice(range.indices(), [NodeItem::new(NodeInfo {
                    path: vec![], /* will be fixed up later */
                    node: childhood.node.clone(),
                    props: childhood.node.props.clone(),
                    offset: childhood.offset,
                    address: addr + childhood.offset.to_size(),
                    document: new_doc.clone(),
                    document_host,
                })]).count();

                Some((range.first as u32, count_removed as u32, 1))
            },
            change::ChangeType::Nest { .. } => None,

            /* Was one of our children destructured? */
            change::ChangeType::Destructure { parent, child_index: child, num_grandchildren, offset: _ } if parent[..] == i.path[..] => {
                let document_host = i.document_host.clone();
                
                i.children.splice(child..=child, new_node.children[*child..(*child+*num_grandchildren)].iter().map(|childhood| NodeItem::new(NodeInfo {
                    path: vec![], /* will be fixed up later */
                    node: childhood.node.clone(),
                    props: childhood.node.props.clone(),
                    offset: childhood.offset,
                    address: addr + childhood.offset.to_size(),
                    document: new_doc.clone(),
                    document_host: document_host.clone(),
                }))).count();

                Some((*child as u32, 1, *num_grandchildren as u32))
            },
            change::ChangeType::Destructure { .. } => None,

            /* Were some of our children deleted? */
            change::ChangeType::DeleteRange { range } if range.parent == i.path => {
                let count_removed = i.children.splice(range.indices(), []).count();

                Some((range.first as u32, count_removed as u32, 0))
            },
            change::ChangeType::DeleteRange { .. } => None,

            change::ChangeType::StackFilter(_) => None,
        };

        /* Fixup children's paths and node pointers */
        {
            let i = &mut *i;
            for (index, child) in i.children.iter_mut().enumerate() {
                let mut child_info = child.imp().info.get().unwrap().borrow_mut();
                child_info.path.splice(.., i.path.iter().cloned());
                child_info.path.push(index);
                child_info.document = new_doc.clone();
                child_info.node = new_node.children[index].node.clone();
            }
        }

        std::mem::drop(i);

        items_changed
    }
    
    fn update(&self, new_doc: &sync::Arc<document::Document>) {
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

            for (index, added, removed) in items_changed_records {
                self.items_changed(index, added, removed);
            }
        }
    }
}

impl NodeItem {
    fn new(info: NodeInfo) -> Self {
        let item: Self = glib::Object::builder().build();
        item.imp().info.set(cell::RefCell::new(info)).unwrap();
        item
    }

    fn stage(&self, info: NodeInfo) {
        *self.imp().staged_info.borrow_mut() = Some(info);
    }

    pub fn info(&self) -> cell::Ref<'_, NodeInfo> {
        self.imp().info.get().unwrap().borrow()
    }

    fn update_document(&self, new_doc: &sync::Arc<document::Document>) {
        if let Some(slm) = self.imp().expansion.borrow().upgrade() {
            slm.update(new_doc);
        }
    }
    
    fn trigger_notifies(&self) {
        if let Some(new_info) = self.imp().staged_info.borrow_mut().take() {
            let mut info = self.imp().info.get().unwrap().borrow_mut();

            let changed_name = info.props.name != new_info.props.name;
            let changed_offset = info.offset != new_info.offset;
            let changed_address = info.address != new_info.address;

            *info = new_info;

            /* as soon as we start notifying, callbacks are allowed to try to
             * retrieve the new properties, requiring a reference to our
             * interior. */
            std::mem::drop(info);

            if changed_name { self.notify("name"); }
            if changed_offset { self.notify("offset"); }
            if changed_address { self.notify("address"); }
        }
    }
}

impl RootListModel {
    fn new(document_host: sync::Arc<document::DocumentHost>, document: sync::Arc<document::Document>) -> Self {
        let model: Self = glib::Object::builder().build();

        model.imp().root_item.set(NodeItem::new(NodeInfo {
            path: vec![],
            node: document.root.clone(),
            props: document.root.props.clone(),
            offset: addr::unit::NULL,
            address: addr::unit::NULL,
            document: document.clone(),
            document_host: document_host.clone(),
        })).unwrap();

        model
    }

    pub fn update_document(&self, new_document: &sync::Arc<document::Document>) {
        let root_item = self.imp().root_item.get().unwrap();
        
        root_item.stage(NodeInfo {
            path: vec![],
            node: new_document.root.clone(),
            props: new_document.root.props.clone(),
            offset: addr::unit::NULL,
            address: addr::unit::NULL,
            document: new_document.clone(),
            document_host: root_item.info().document_host.clone(),
        });

        root_item.update_document(new_document);
        root_item.trigger_notifies();
    }
}

struct AlterNodesBulkNodeInfoUpdater<'a>(&'a mut vec::Vec<NodeItem>, &'a structure::Node);

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

#[cfg(test)]
mod tests {
    use super::*;
    use rusty_fork::rusty_fork_test;
    
    fn assert_tlr_node_correct(document: &sync::Arc<document::Document>, node: &sync::Arc<structure::Node>, iter: &mut impl std::iter::Iterator<Item = gtk::TreeListRow>) {
        let tlr = iter.next().expect("tree list model ended early");
        let item = tlr.item().unwrap().downcast::<NodeItem>().unwrap();
        let info = item.imp().info.get().unwrap().borrow();

        assert!(sync::Arc::ptr_eq(&info.document, document), "expected node info document to be up to date");
        assert!(sync::Arc::ptr_eq(&info.node, node), "expected to see '{}', but saw '{}' (path {:?})", node.props.name, info.props.name, info.path);

        if tlr.is_expanded() {
            for childhood in &node.children {
                assert_tlr_node_correct(document, &childhood.node, iter);
            }
        }
    }

    fn assert_tlm_correct(document: &sync::Arc<document::Document>, tlm: &gtk::TreeListModel) {
        let mut iter = tlm.iter().map(Result::unwrap);
        assert_tlr_node_correct(document, &document.root, &mut iter);

        if let Some(_) = iter.next() {
            panic!("tree list model had extra items");
        }
    }
    
    fn tree_list_node_items_iter(tlm: &gtk::TreeListModel) -> impl std::iter::Iterator<Item = NodeItem> + '_ {
        tlm.iter::<gtk::TreeListRow>().map(|tlr| tlr.unwrap().item().unwrap().downcast::<NodeItem>().unwrap())
    }

    /* GTK doesn't like being initialized in a process more than once. */
    rusty_fork_test! {
        #[test]
        fn test_reexpand() {
            gtk::init().unwrap();

            let root = structure::Node::builder()
                .name("root")
                .size(0x40)
                .child(0x0, |b| b
                       .name("container")
                       .size(0x40)
                       .child(0x0, |b| b
                              .name("child0")
                              .size(0x8))
                       .child(0x8, |b| b
                              .name("child1")
                              .size(0x8)))
                .build();
            
            let document_host = sync::Arc::new(document::Builder::new(root).host());
            let mut document = document_host.get();
            
            let tlm = create_tree_list_model(document_host.clone(), document.clone(), true);
            assert_tlm_correct(&document, &tlm);

            document = document_host.change(document.delete_range(structure::SiblingRange::new(vec![0], 0, 0))).unwrap();
            tlm.model().downcast::<RootListModel>().unwrap().update_document(&document);
            assert_tlm_correct(&document, &tlm);

            tlm.item(1).unwrap().downcast::<gtk::TreeListRow>().unwrap().set_expanded(false);
            assert_tlm_correct(&document, &tlm);

            tlm.item(1).unwrap().downcast::<gtk::TreeListRow>().unwrap().set_expanded(true);
            assert_tlm_correct(&document, &tlm);
        }
    }
}
