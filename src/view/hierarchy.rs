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
use crate::view::helpers;

pub fn create_tree_list_model(document_host: sync::Arc<document::DocumentHost>, document: sync::Arc<document::Document>, autoexpand: bool) -> gtk::TreeListModel {
    let root_model = gio::ListStore::new(NodeItem::static_type());

    let root_item = NodeItem::new(NodeInfo {
        path: vec![],
        node: document.root.clone(),
        props: document.root.props.clone(),
        offset: addr::unit::NULL,
        address: addr::unit::NULL,
        document_host: document_host.clone(),
        document: document.clone(),
    });
    
    root_model.append(&root_item);
    
    let model = gtk::TreeListModel::new(&root_model, false, autoexpand, |obj| {
        Some(StructureListModel::from_node_info(
            &obj.downcast_ref::<NodeItem>().unwrap().imp().info.get().unwrap().borrow()
        ).upcast())
    });

    let subscriber = helpers::subscribe_to_updates(root_item.downgrade(), document_host.clone(), document, move |root_item, new_document| {
        root_item.stage(NodeInfo {
            path: vec![],
            node: new_document.root.clone(),
            props: new_document.root.props.clone(),
            offset: addr::unit::NULL,
            address: addr::unit::NULL,
            document: new_document.clone(),
            document_host: document_host.clone(),
        });
        root_item.update();
    });

    /* The root item lasts forever. */
    /* TODO: no it doesn't; you can close and reopen the window. */
    std::mem::forget(subscriber);
    
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
    use crate::view::helpers;

    use super::NodeInfo;

    #[derive(Debug)]
    pub struct StructureListModelInterior {
        pub path: structure::Path,
        pub children: vec::Vec<super::NodeItem>,
        pub address: addr::Address,
        pub document_host: sync::Arc<document::DocumentHost>,
        pub document: sync::Arc<document::Document>,
        pub subscriber: helpers::AsyncSubscriber,
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
                self.obj().update();
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
}

glib::wrapper! {
    pub struct StructureListModel(ObjectSubclass<imp::StructureListModel>)
        @implements gio::ListModel;
}

glib::wrapper! {
    pub struct NodeItem(ObjectSubclass<imp::NodeItem>)
        ;
}

impl StructureListModel {
    fn from_node_info(info: &NodeInfo) -> Self {
        let model: Self = glib::Object::builder().build();

        let subscriber = helpers::subscribe_to_updates(model.downgrade(), info.document_host.clone(), info.document.clone(), move |model, new_document| {
            model.update(new_document);
        });

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
            subscriber,
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
            change::UpdatePathResult::Deleted => {
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
            change::ChangeType::AlterNode(path, new_props) if i.path.len() + 1 == path.len() && path[0..i.path.len()] == i.path[..] => {
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
            change::ChangeType::AlterNode(_, _) => None,

            /* Did we get a new child? */
            change::ChangeType::InsertNode(affected_path, affected_index, _new_node_offset, _new_node) if affected_path[..] == i.path[..] => {
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
            change::ChangeType::InsertNode(_, _, _, _) => None,

            /* Were some of our children nested? */
            change::ChangeType::Nest(parent, first_child, last_child, _props) if parent[..] == i.path[..] => {
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
            change::ChangeType::Nest(_, _, _, _) => None,

            /* Were some of our children deleted? */
            change::ChangeType::DeleteRange(parent, first_child, last_child) if parent[..] == i.path[..] => {
                let count_removed = i.children.splice(first_child..=last_child, []).count();

                Some((*first_child as u32, count_removed as u32, 0))
            },
            change::ChangeType::DeleteRange(_, _, _) => None,
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
                item.update();
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

    fn update(&self) {
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
