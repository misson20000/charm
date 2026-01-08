use std::cell;
use std::sync;

use gtk::gio;
use gtk::glib;
use gtk::prelude::*;
use gtk::subclass::prelude::*;

use crate::catch_panic;
use crate::model::document;
use crate::view::helpers;

mod filter_item;

pub use filter_item::FilterItem;

mod imp {
    use super::*;

    #[derive(Debug)]
    pub struct FilterListModelInterior {
        document_host: sync::Arc<document::DocumentHost>,
        document: sync::Arc<document::Document>,
        filter_items: Vec<FilterItem>,
    }

    impl FilterListModelInterior {
        pub fn new(document_host: sync::Arc<document::DocumentHost>, document: sync::Arc<document::Document>) -> Self {
            let filter_items = document.datapath.iter_filters().cloned().map(FilterItem::new).collect();
            
            Self {
                document_host,
                document,
                filter_items,
            }
        }
    }
    
    #[derive(Default)]
    pub struct FilterListModel {
        pub interior: once_cell::unsync::OnceCell<cell::RefCell<FilterListModelInterior>>,
        pub subscriber: once_cell::unsync::OnceCell<helpers::AsyncSubscriber>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for FilterListModel {
        const NAME: &'static str = "CharmFilterListModel";
        type Type = super::FilterListModel;
        type Interfaces = (gio::ListModel,);
    }

    impl ObjectImpl for FilterListModel {
    }

    impl ListModelImpl for FilterListModel {
        fn item_type(&self) -> glib::Type {
            /* FFI CALLBACK: assumed panic-safe */
            FilterItem::static_type()
        }

        fn n_items(&self) -> u32 {
            /* FFI CALLBACK: trivially panic-safe */
            self.interior.get().unwrap().borrow().filter_items.len() as u32
        }
        
        fn item(&self, position: u32) -> Option<glib::Object> {
            catch_panic! {
                @default(None);

                self.interior.get().unwrap().borrow().filter_items.get(position as usize).map(|i| i.clone().upcast())
            }
        }
    }

    impl FilterListModel {
        pub fn update_document(&self, new_document: &sync::Arc<document::Document>) {
            let mut i = self.interior.get().unwrap().borrow_mut();
            let count_old = i.filter_items.len();
            i.filter_items = new_document.datapath.iter_filters().rev().cloned().map(FilterItem::new).collect();
            let count_new = i.filter_items.len();
            std::mem::drop(i);
            
            self.obj().items_changed(0, count_old as u32, count_new as u32);
        }
    }
}

glib::wrapper! {
    /// A ListModel implementation for datapath filters. Does not
    /// subscribe to document changes and must be manually updated via
    /// [FilterListModel::update_document], but will recursively update
    /// the rest of the tree.
    pub struct FilterListModel(ObjectSubclass<imp::FilterListModel>)
        @implements gio::ListModel;
}

impl FilterListModel {
    pub fn new(document_host: sync::Arc<document::DocumentHost>, document: sync::Arc<document::Document>) -> Self {
        let model: Self = glib::Object::builder().build();

        model.imp().interior.set(cell::RefCell::new(imp::FilterListModelInterior::new(document_host.clone(), document.clone()))).unwrap();

        model.imp().subscriber.set(helpers::subscribe_to_updates(
            model.downgrade(),
            document_host,
            document,
            move |model, new_document| {
                model.update_document(new_document);
            }
        )).unwrap();

        model
    }

    pub fn update_document(&self, new_document: &sync::Arc<document::Document>) {
        self.imp().update_document(new_document);
    }
}
