use std::sync;

use gtk::glib;
use gtk::subclass::prelude::*;

use crate::catch_panic;
use crate::model::datapath;

glib::wrapper! {
    /// GLib Object representing a single [datapath::Filter] within a
    /// document. Does not subscribe to document changes and must be
    /// manually updated.
    pub struct FilterItem(ObjectSubclass<imp::FilterItem>)
        ;
}

mod imp {
    use super::*;

    #[derive(Default)]
    pub struct FilterItem {
        pub filter: once_cell::unsync::OnceCell<sync::Arc<datapath::Filter>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for FilterItem {
        const NAME: &'static str = "CharmFilterItem";
        type Type = super::FilterItem;
        type ParentType = glib::Object;
    }

    impl ObjectImpl for FilterItem {
        fn properties() -> &'static [glib::ParamSpec] {
            /* FFI CALLBACK: panic-safe */
            
            static PROPERTIES: once_cell::sync::Lazy<Vec<glib::ParamSpec>> =
                once_cell::sync::Lazy::new(|| vec![
                    glib::ParamSpecString::builder("type").build(),
                    glib::ParamSpecString::builder("addr").build(),
                    glib::ParamSpecString::builder("size").build(),
                    glib::ParamSpecString::builder("details").build(),
                ]);
            PROPERTIES.as_ref()
        }

        fn property(&self, _id: usize, pspec: &glib::ParamSpec) -> glib::Value {
            catch_panic! {
                @default(glib::Value::from_type(glib::Type::INVALID));
                
                let filter = self.filter.get().unwrap();
                match pspec.name() {
                    "type" => glib::value::ToValue::to_value(filter.human_type()),
                    "addr" => glib::value::ToValue::to_value(&format!("0x{:x}", filter.human_affects_addr())),
                    "size" => glib::value::ToValue::to_value(&filter.human_affects_size().map(|size| format!("0x{:x}", size)).unwrap_or_else(|| "Infinite".to_string())),
                    "details" => glib::value::ToValue::to_value(&filter.human_details()),
                    _ => unimplemented!(),
                }
            }
        }
    }
}

impl FilterItem {
    pub fn new(filter: sync::Arc<datapath::Filter>) -> Self {
        let model: Self = glib::Object::builder().build();

        model.imp().filter.set(filter).unwrap();

        model
    }
}
