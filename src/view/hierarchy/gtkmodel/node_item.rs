use std::cell;
use std::sync;

use gtk::glib;
use gtk::prelude::*;
use gtk::subclass::prelude::*;

use crate::model::document;

glib::wrapper! {
    /// GLib Object representing a single [structure::Node] within a
    /// document, exposing certain properties related to the structure
    /// node, and allowing them to be modified (causing document
    /// changes). Does not subscribe to document changes and must be
    /// manually updated.
    pub struct NodeItem(ObjectSubclass<imp::NodeItem>)
        ;
}

mod imp {
    use std::cell;
    use std::vec;
    
    use gtk::glib;
    use gtk::subclass::prelude::*;

    use crate::catch_panic;
    use crate::model::document::structure;

    use super::super::NodeInfo;

    #[derive(Default)]
    pub struct NodeItem {
        pub info: once_cell::unsync::OnceCell<cell::RefCell<NodeInfo>>,
        pub staged_info: cell::RefCell<Option<NodeInfo>>,
        pub expansion: cell::RefCell<glib::object::WeakRef<super::super::StructureListModel>>
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
        pub fn expand(&self) -> super::super::StructureListModel {
            let expansion = self.expansion.borrow_mut();
            match expansion.upgrade() {
                Some(slm) => slm,
                None => {
                    let slm = super::super::StructureListModel::from_node_info(&*self.info.get().unwrap().borrow());
                    expansion.set(Some(&slm));
                    slm
                }
            }
        }
    }
}

impl NodeItem {
    pub fn new(info: super::NodeInfo) -> Self {
        let item: Self = glib::Object::builder().build();
        item.imp().info.set(cell::RefCell::new(info)).unwrap();
        item
    }

    pub fn stage(&self, info: super::NodeInfo) {
        *self.imp().staged_info.borrow_mut() = Some(info);
    }

    pub fn info(&self) -> cell::Ref<'_, super::NodeInfo> {
        self.imp().info.get().unwrap().borrow()
    }

    pub fn staged_info(&self) -> super::NodeInfo {
        if let Some(info) = (*self.imp().staged_info.borrow_mut()).clone() {
            return info.clone();
        }

        self.imp().info.get().unwrap().borrow().clone()
    }
    
    /// Propagate changes to [super::StructureListModel], if expanded.
    pub fn update_document(&self, new_doc: &sync::Arc<document::Document>) {
        if let Some(slm) = self.imp().expansion.borrow().upgrade() {
            slm.update(new_doc);
        }
    }
    
    pub fn trigger_notifies(&self) {
        if let Some(new_info) = self.imp().staged_info.borrow_mut().take() {
            let mut info = self.imp().info.get().unwrap().borrow_mut();

            let changed_name = info.props.name != new_info.props.name;
            let changed_offset = info.offset != new_info.offset;
            let changed_address = info.address != new_info.address;
            let changed_size = info.node.size != new_info.node.size;

            *info = new_info;

            /* as soon as we start notifying, callbacks are allowed to try to
             * retrieve the new properties, requiring a reference to our
             * interior. */
            std::mem::drop(info);

            if changed_name { self.notify("name"); }
            if changed_offset { self.notify("offset"); }
            if changed_address { self.notify("addr"); }
            if changed_size { self.notify("size"); }
        }
    }
}
