use std::sync;

use crate::model::datapath;
use crate::model::document;
use crate::view::helpers;

use gtk::prelude::*;
use gtk::glib;

pub fn create_model(document_host: sync::Arc<document::DocumentHost>) -> (gtk::TreeModel, helpers::AsyncSubscriber) {
    let document = document_host.get();

    let store = gtk::ListStore::new(&[
        glib::types::Type::STRING,
        glib::types::Type::STRING,
        glib::types::Type::STRING,
        glib::types::Type::STRING,
    ]);

    update_store(&store, &document);
    
    let subscriber = helpers::subscribe_to_updates(store.downgrade(), document_host, document, move |store, new_document| {
        update_store(&store, &new_document);
    });

    (store.upcast(), subscriber)
}

fn update_store(store: &gtk::ListStore, document: &sync::Arc<document::Document>) {
    store.clear();
    
    // TODO: only adjust difference
    for filter in document.datapath.iter_filters().rev() {
        let i = store.append();
        store.set(&i, &[
            (0, match filter {
                datapath::Filter::LoadSpace(_) => &"Load Address Space",
                datapath::Filter::Overwrite(_) => &"Overwrite Bytes",
                datapath::Filter::Move(_) => &"Move Bytes",
                datapath::Filter::Insert(_) => &"Insert Bytes",
            }),
            (1, &format!("0x{:x}", filter.human_affects_addr())),
            (2, &filter.human_affects_size().map(|size| format!("0x{:x}", size)).unwrap_or_else(|| "Infinite".to_string())),
            (3, &filter.human_details())
        ]);
    }
}
