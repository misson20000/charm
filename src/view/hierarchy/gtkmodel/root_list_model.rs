use std::sync;
use std::vec;

use gtk::gio;
use gtk::glib;
use gtk::subclass::prelude::*;

use crate::model::addr;
use crate::model::document;

mod imp {
    use gtk::gio;
    use gtk::glib;
    use gtk::subclass::prelude::*;
    use gtk::prelude::*;

    use crate::catch_panic;
    
    #[derive(Default)]
    pub struct RootListModel {
        pub root_item: once_cell::unsync::OnceCell<super::super::NodeItem>,
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
            super::super::NodeItem::static_type()
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
    /// A ListModel implementation that contains a single item representing the
    /// document's root node. Does not subscribe to document changes and must be
    /// manually updated via [RootListModel::update_document], but will
    /// recursively update the rest of the tree.
    pub struct RootListModel(ObjectSubclass<imp::RootListModel>)
        @implements gio::ListModel;
}

impl RootListModel {
    pub fn new(document_host: sync::Arc<document::DocumentHost>, document: sync::Arc<document::Document>) -> Self {
        let model: Self = glib::Object::builder().build();

        model.imp().root_item.set(super::NodeItem::new(super::NodeInfo {
            path: vec![],
            node: document.root.clone(),
            props: document.root.props.clone(),
            offset: addr::Offset::NULL,
            address: addr::AbsoluteAddress::NULL,
            document: document.clone(),
            document_host: document_host.clone(),
        })).unwrap();

        model
    }

    pub fn update_document(&self, new_document: &sync::Arc<document::Document>) {
        let root_item = self.imp().root_item.get().unwrap();
        
        root_item.stage(super::NodeInfo {
            path: vec![],
            node: new_document.root.clone(),
            props: new_document.root.props.clone(),
            offset: addr::Offset::NULL,
            address: addr::AbsoluteAddress::NULL,
            document: new_document.clone(),
            document_host: root_item.info().document_host.clone(),
        });

        root_item.update_document(new_document);
        root_item.trigger_notifies();
    }
}
