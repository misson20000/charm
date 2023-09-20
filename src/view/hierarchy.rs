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
use crate::model::versioned::Versioned;

/// Our TreeListModel doesn't update itself. This is so that we can guarantee that the selection model updates first by having it be the one that subscribes to SelectionHost updates and informs us when the document updates. We still need a document_host though so we can change the document when properties are edited.
pub fn create_tree_list_model(document_host: sync::Arc<document::DocumentHost>, document: sync::Arc<document::Document>, autoexpand: bool) -> gtk::TreeListModel {
    let root_model = RootListModel::new(document_host, document);
    
    let model = gtk::TreeListModel::new(root_model, false, autoexpand, |obj| {
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
            super::NodeItem::static_type()
        }

        fn n_items(&self) -> u32 {
            self.interior.get().map_or(0, |i| i.borrow().children.len() as u32)
        }

        fn item(&self, position: u32) -> Option<glib::Object> {
            self.interior.get().and_then(|i| {
                let i = i.borrow();
                i.children.get(position as usize).map(|ch| ch.clone().upcast())
            })
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
            /* we update our local copy of properties immediately. when the
               document update goes through, it will notify us, but we'll see
               that the new properties already match our local properties and we
               won't notify the properties as changed again. */

            println!("property {} is being set", pspec.name());
            
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

        fn property(&self, _id: usize, pspec: &glib::ParamSpec) -> glib::Value {
            let info = self.info.get().unwrap().borrow();
            match pspec.name() {
                "name" => glib::ToValue::to_value(&info.props.name),
                "addr" => glib::ToValue::to_value(&format!("{}", info.address)),
                "size" => glib::ToValue::to_value(&format!("{}", info.node.size)),
                "children-display" => glib::ToValue::to_value(match info.props.children_display {
                    structure::ChildrenDisplay::None => "n",
                    structure::ChildrenDisplay::Summary => "s",
                    structure::ChildrenDisplay::Full => "f",
                }),
                _ => unimplemented!(),
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
            super::NodeItem::static_type()
        }

        fn n_items(&self) -> u32 {
            1
        }

        fn item(&self, position: u32) -> Option<glib::Object> {
            assert_eq!(position, 0);
            self.root_item.get().map(|ch| ch.clone().upcast())
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

    fn port_change(&self, new_doc: &sync::Arc<document::Document>, change: &change::Change) {
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
                return;
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
            change::ChangeType::Nest { parent, first_child, last_child, extent: _, props: _ } if parent[..] == i.path[..] => {
                let childhood = &new_node.children[*first_child];
                let document_host = i.document_host.clone();
                
                let count_removed = i.children.splice(first_child..=last_child, [NodeItem::new(NodeInfo {
                    path: vec![], /* will be fixed up later */
                    node: childhood.node.clone(),
                    props: childhood.node.props.clone(),
                    offset: childhood.offset,
                    address: addr + childhood.offset.to_size(),
                    document: new_doc.clone(),
                    document_host,
                })]).count();

                Some((*first_child as u32, count_removed as u32, 1))
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
            change::ChangeType::DeleteRange { parent, first_child, last_child } if parent[..] == i.path[..] => {
                let count_removed = i.children.splice(first_child..=last_child, []).count();

                Some((*first_child as u32, count_removed as u32, 0))
            },
            change::ChangeType::DeleteRange { .. } => None,
        };

        /* Fixup children's paths */
        {
            let i = &mut *i;
            for (index, child) in i.children.iter_mut().enumerate() {
                let mut child_info = child.imp().info.get().unwrap().borrow_mut();
                child_info.path.splice(.., i.path.iter().cloned());
                child_info.path.push(index);
                child_info.document = new_doc.clone();
            }
        }

        std::mem::drop(i);

        if let Some((index, added, removed)) = items_changed {
            self.items_changed(index, added, removed);
        }
    }
    
    fn update(&self, new_doc: &sync::Arc<document::Document>) {
        if let Some(i) = self.imp().interior.get().map(|i| i.borrow()) {
            let old_doc = i.document.clone();
            drop(i);
            
            new_doc.changes_since(&old_doc, &mut |doc, change| {
                self.port_change(doc, change);
            });

            /* We need to avoid notifying gtk of its own property updates. Our system to do this works by filtering out
             * updates that match what GTK thinks the properties already are. Unfortunately, there are some cases where
             * GTK updates a property twice in quick succession, which breaks this filtering, so we need to coalesce
             * updates before deciding whether to filter them out or not. */
            
            for item in &self.imp().interior.get().unwrap().borrow().children {
                item.update_document(new_doc);
                item.trigger_notifies();                
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
