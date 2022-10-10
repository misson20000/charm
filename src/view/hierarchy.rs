use std::cell;
use std::sync;
use std::vec;

use gtk::gio;
use gtk::glib;
use gtk::prelude::*;
use gtk::subclass::prelude::*;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::view::helpers;

pub fn create_tree_list_model(document_host: sync::Arc<document::DocumentHost>, autoexpand: bool) -> gtk::TreeListModel {
    let document = document_host.get();
    
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

    helpers::subscribe_to_document_updates(model.downgrade(), document_host.clone(), document, move |_, new_document| {
        root_item.update(NodeInfo {
            path: vec![],
            node: new_document.root.clone(),
            props: new_document.root.props.clone(),
            offset: addr::unit::NULL,
            address: addr::unit::NULL,
            document: new_document.clone(),
            document_host: document_host.clone(),
        });
        //model.model().downcast::<StructureListModel>().unwrap().update(new_document);
    });
    
    model
}

pub fn create_selection_model(document_host: sync::Arc<document::DocumentHost>) -> gtk::SelectionModel {
    gtk::builders::SingleSelectionBuilder::new()
        .model(&create_tree_list_model(document_host, true))
        .build()
        .upcast()
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
    use gtk::prelude::{Cast, StaticType};
    
    use crate::model::addr;
    use crate::model::document;
    use crate::model::document::structure;

    #[derive(Debug)]
    pub struct StructureListModelInterior {
        pub path: structure::Path,
        pub children: vec::Vec<structure::Childhood>,
        pub address: addr::Address,
        pub document_host: sync::Arc<document::DocumentHost>,
        pub document: sync::Arc<document::Document>,
    }

    #[derive(Default)]
    pub struct StructureListModel {
        pub interior: once_cell::unsync::OnceCell<StructureListModelInterior>,
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
        fn item_type(&self, _: &Self::Type) -> glib::Type {
            super::NodeItem::static_type()
        }

        fn n_items(&self, _: &Self::Type) -> u32 {
            self.interior.get().map_or(0, |i| i.children.len() as u32)
        }

        fn item(&self, _: &Self::Type, position: u32) -> Option<glib::Object> {
            self.interior.get().and_then(|i| {
                i.children.get(position as usize).map(|ch| {
                    let mut path = i.path.clone();
                    path.push(position as usize);
                    super::NodeItem::new(super::NodeInfo {
                        path,
                        node: ch.node.clone(),
                        props: ch.node.props.clone(),
                        offset: ch.offset,
                        address: i.address + ch.offset.to_size(),
                        document_host: i.document_host.clone(),
                        document: i.document.clone()
                    }).upcast()
                })
            })
        }
    }
    
    #[derive(Default)]
    pub struct NodeItem {
        pub info: once_cell::unsync::OnceCell<cell::RefCell<super::NodeInfo>>,
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
                ]);
            PROPERTIES.as_ref()
        }

        fn set_property(&self, obj: &Self::Type, _id: usize, value: &glib::Value, pspec: &glib::ParamSpec) {
            /* we update our local copy of properties immediately. when the
               document update goes through, it will notify us, but we'll see
               that the new properties already match our local properties and we
               won't notify the properties as changed again. */

            println!("property {} is being set", pspec.name());
            
            let mut info = self.info.get().unwrap().borrow_mut();
            let old_info = info.clone();
            
            match pspec.name() {
                "name" => info.props.name = value.get().unwrap(),
                _ => unimplemented!(),
            };

            if let Err(e) = info.document_host.alter_node(&info.document, info.path.clone(), info.props.clone()) {
                /* roll back */
                println!("failed to alter node: {:?}", e);
                std::mem::drop(info);
                obj.update(old_info);
            }
        }

        fn property(&self, _obj: &Self::Type, _id: usize, pspec: &glib::ParamSpec) -> glib::Value {
            let info = self.info.get().unwrap().borrow();
            match pspec.name() {
                "name" => glib::ToValue::to_value(&info.props.name),
                "addr" => glib::ToValue::to_value(&format!("{}", info.address)),
                "size" => glib::ToValue::to_value(&format!("{}", info.node.size)),
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
        let model: Self = glib::Object::new(&[]).unwrap();
        model.imp().interior.set(imp::StructureListModelInterior {
            path: info.path.clone(),
            children: info.node.children.clone(),
            address: info.address,
            document_host: info.document_host.clone(),
            document: info.document.clone(),
        }).unwrap();
        model
    }

    fn update(&self, _new: &sync::Arc<structure::Node>) {
    }
}

impl NodeItem {
    fn new(info: NodeInfo) -> Self {
        let item: Self = glib::Object::new(&[]).unwrap();
        item.imp().info.set(cell::RefCell::new(info)).unwrap();
        item
    }

    fn update(&self, new_info: NodeInfo) {
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
