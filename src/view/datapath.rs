use std::sync;
use std::task;

use crate::util;
use crate::view;
use crate::model::datapath;
use crate::model::document;

use gtk::glib;

pub struct DataPathModel {
    document_host: sync::Arc<document::DocumentHost>,
    document: sync::Arc<document::Document>,
    update_task: Option<tokio::task::JoinHandle<()>>,
    update_notifier: util::Notifier,
    store: send_wrapper::SendWrapper<gtk::ListStore>,
}

impl DataPathModel {
    pub fn new(cw: &view::window::CharmWindow, document_host: &sync::Arc<document::DocumentHost>) -> sync::Arc<parking_lot::RwLock<DataPathModel>> {
        let mut dpm = DataPathModel {
            document_host: document_host.clone(),
            document: sync::Arc::new(document::Document::invalid()),
            update_task: None,
            update_notifier: util::Notifier::new(),

            store: send_wrapper::SendWrapper::new(gtk::ListStore::new(&[
                glib::types::Type::STRING,
                glib::types::Type::STRING,
                glib::types::Type::STRING,
                glib::types::Type::STRING,
            ])),
        };

        dpm.update_store();

        let rc = sync::Arc::new(parking_lot::RwLock::new(dpm));
        let mut dpm = rc.write();
        dpm.update_task = Some(cw.application.rt.spawn(DataPathModelFuture::new(&rc)));
        std::mem::drop(dpm);
        
        rc
    }

    pub fn get_tree_model(&self) -> &gtk::ListStore {
        &self.store
    }
    
    fn update_store(&mut self) {
        let upstream = self.document_host.get().clone();
        self.store.clear();

        // TODO: only adjust difference
        for filter in upstream.datapath.iter().rev() {
            let i = self.store.append();
            self.store.set(&i, &[
                (0, match filter {
                    datapath::Filter::LoadSpace(_) => &"Load Address Space",
                    datapath::Filter::Overwrite(_) => &"Overwrite Bytes",
                    datapath::Filter::Move(_) => &"Move Bytes",
                    datapath::Filter::Insert(_) => &"Insert Bytes",
                }),
                (1, &format!("0x{:x}", filter.human_affects_addr())),
                (2, &filter.human_affects_size().map(|size| format!("0x{:x}", size)).unwrap_or("Infinite".to_string())),
                (3, &filter.human_details())
            ]);
        }
        
        self.document = upstream;
    }

    fn update(dpm_arc: &sync::Arc<parking_lot::RwLock<DataPathModel>>, cx: &mut task::Context) {
        let dpm = dpm_arc.read();
        
        let document = dpm.document_host.borrow();
        dpm.document_host.wait(cx);
        dpm.update_notifier.enroll(cx);

        if document.is_outdated(&dpm.document) {
            let dpm_weak = sync::Arc::downgrade(dpm_arc);
            glib::idle_add(move || {
                match dpm_weak.upgrade() {
                    Some(dpm) => dpm.write().update_store(),
                    None => ()
                }
                glib::Continue(false)
            });
        }
    }
}

struct DataPathModelFuture {
    dpm: sync::Weak<parking_lot::RwLock<DataPathModel>>,
}

impl DataPathModelFuture {
    fn new(dpm: &sync::Arc<parking_lot::RwLock<DataPathModel>>) -> DataPathModelFuture {
        DataPathModelFuture {
            dpm: sync::Arc::downgrade(dpm),
        }
    }
}

impl std::future::Future for DataPathModelFuture {
    type Output = ();

    fn poll(self: std::pin::Pin<&mut Self>, cx: &mut task::Context) -> task::Poll<()> {
        match self.dpm.upgrade() {
            Some(dpm) => {
                DataPathModel::update(&dpm, cx);
                task::Poll::Pending
            }
            None => task::Poll::Ready(())
        }
    }
}
