use std::sync;
use std::vec;

use gtk::gio;
use gtk::glib;
use gtk::subclass::prelude::*;
use gtk::prelude::*;

use crate::model::addr;
use crate::model::document::structure;

pub fn create_tree_list_model(root: sync::Arc<structure::Node>, autoexpand: bool) -> gtk::TreeListModel {
    let list_model = StructureListModel::wrap_root(root);
    gtk::TreeListModel::new(&list_model, false, autoexpand, |obj| {
        Some(StructureListModel::from_childhood(obj.clone().downcast::<StructureChildhoodItem>().unwrap().imp().interior.get().unwrap()).upcast())
    })
}

pub fn create_widget(object: &glib::Object) -> gtk::Widget {
    let row = gtk::ListBoxRow::new();
    println!("creating widget for {:?}", object);
    let sci = object.downcast_ref::<gtk::TreeListRow>()
        .unwrap()
        .item()
        .unwrap()
        .downcast::<StructureChildhoodItem>()
        .unwrap();
    let ch = sci
        .imp()
        .interior
        .get()
        .unwrap();
    row.set_child(Some(&gtk::Label::new(Some(&ch.node.props.name))));
    row.upcast()
}

pub fn create_selection_model(root: sync::Arc<structure::Node>) -> gtk::SelectionModel {
    //StructureListModel::wrap_root(root).upcast()
    gtk::builders::SingleSelectionBuilder::new()
        .model(&create_tree_list_model(root, true))
        .build()
        .upcast()
}

#[derive(Debug)]
pub struct AbsoluteChildhood {
    node: sync::Arc<structure::Node>,
    offset: addr::Address,
    address: addr::Address,
}

mod imp {
    use std::vec;
    use gtk::gio;
    use gtk::glib;
    use gtk::subclass::prelude::*;
    use gtk::prelude::{Cast, StaticType};
    use crate::model::{addr, document::structure};

    #[derive(Debug)]
    pub struct StructureListModelInterior {
        pub children: vec::Vec<structure::Childhood>,
        pub address: addr::Address,
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
            super::StructureChildhoodItem::static_type()
        }

        fn n_items(&self, _: &Self::Type) -> u32 {
            self.interior.get().map_or(0, |i| i.children.len() as u32)
        }

        fn item(&self, _: &Self::Type, position: u32) -> Option<glib::Object> {
            self.interior.get().and_then(|i| {
                i.children.get(position as usize).map(|ch| {
                    super::StructureChildhoodItem::new(super::AbsoluteChildhood {
                        node: ch.node.clone(),
                        offset: ch.offset,
                        address: i.address + ch.offset.to_size()
                    }).upcast()
                })
            })
        }
    }

    #[derive(Default)]
    pub struct StructureChildhoodItem {
        pub interior: once_cell::unsync::OnceCell<super::AbsoluteChildhood>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for StructureChildhoodItem {
        const NAME: &'static str = "CharmStructureChildhoodItem";
        type Type = super::StructureChildhoodItem;
        type ParentType = glib::Object;
    }

    impl ObjectImpl for StructureChildhoodItem {
        fn properties() -> &'static [glib::ParamSpec] {
            static PROPERTIES: once_cell::sync::Lazy<Vec<glib::ParamSpec>> =
                once_cell::sync::Lazy::new(|| vec![
                    glib::ParamSpecString::builder("name").build(),
                    glib::ParamSpecString::builder("addr").build(),
                    glib::ParamSpecString::builder("size").build(),
                ]);
            PROPERTIES.as_ref()
        }

        fn set_property(&self, _obj: &Self::Type, _id: usize, _value: &glib::Value, pspec: &glib::ParamSpec) {
            match pspec.name() {
                _ => unimplemented!(),
            }
        }

        fn property(&self, _obj: &Self::Type, _id: usize, pspec: &glib::ParamSpec) -> glib::Value {
            match pspec.name() {
                "name" => glib::ToValue::to_value(&self.interior.get().unwrap().node.props.name),
                "addr" => glib::ToValue::to_value(&format!("{}", self.interior.get().unwrap().address)),
                "size" => glib::ToValue::to_value(&format!("{}", self.interior.get().unwrap().node.size)),
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
    pub struct StructureChildhoodItem(ObjectSubclass<imp::StructureChildhoodItem>)
        ;
}

impl StructureListModel {
    fn wrap_root(root: sync::Arc<structure::Node>) -> Self {
        let model: Self = glib::Object::new(&[]).unwrap();
        model.imp().interior.set(imp::StructureListModelInterior {
            children: vec![structure::Childhood {
                node: root,
                offset: addr::unit::NULL,
            }],
            address: addr::unit::NULL,
        }).unwrap();
        model
    }

    fn from_childhood(ch: &AbsoluteChildhood) -> Self {
        let model: Self = glib::Object::new(&[]).unwrap();
        model.imp().interior.set(imp::StructureListModelInterior {
            children: ch.node.children.clone(),
            address: ch.address,
        }).unwrap();
        model
    }
}

impl StructureChildhoodItem {
    fn new(ch: AbsoluteChildhood) -> Self {
        let item: Self = glib::Object::new(&[]).unwrap();
        item.imp().interior.set(ch).unwrap();
        item
    }
}
