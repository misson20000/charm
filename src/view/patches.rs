use std::sync;
use std::task;

use crate::util;
use crate::view;
use crate::model::document;

use gtk::prelude::*;

pub struct PatchesModel {
    document_host: sync::Arc<document::DocumentHost>,
    patch_list: document::PatchMap,
    update_task: Option<tokio::task::JoinHandle<()>>,
    update_notifier: util::Notifier,
    store: send_wrapper::SendWrapper<gtk::ListStore>,
}

impl PatchesModel {
    pub fn new(cw: &view::window::CharmWindow, document_host: &sync::Arc<document::DocumentHost>) -> sync::Arc<parking_lot::RwLock<PatchesModel>> {
        let mut pm = PatchesModel {
            document_host: document_host.clone(),
            patch_list: document::PatchMap::new(),
            update_task: None,
            update_notifier: util::Notifier::new(),

            store: send_wrapper::SendWrapper::new(gtk::ListStore::new(&[
                glib::types::Type::U64,
                glib::types::Type::U64,
                glib::types::Type::String,
            ])),
        };

        pm.update_store();

        let rc = sync::Arc::new(parking_lot::RwLock::new(pm));
        let mut pm = rc.write();
        pm.update_task = Some(cw.application.rt.spawn(PatchesModelFuture::new(&rc)));
        std::mem::drop(pm);
        
        rc
    }

    pub fn get_tree_model(&self) -> &gtk::ListStore {
        &self.store
    }
    
    fn update_store(&mut self) {
        let upstream = self.document_host.get_document().get_patches().clone();
        self.store.clear();

        // TODO: only adjust difference
        for (loc, patch) in upstream.iter() {
            let i = self.store.append();
            let s = util::fmt_hex_vec(&patch.bytes);
            self.store.set(&i, &[0, 1, 2], &[loc, &(patch.bytes.len() as u64), &s.as_ref().map(|s| s.as_str()).unwrap_or("FAILED TO FORMAT")]);
        }
        
        self.patch_list = upstream;
    }

    fn update(pm_arc: &sync::Arc<parking_lot::RwLock<PatchesModel>>, cx: &mut task::Context) {
        let pm = pm_arc.read();
        
        let document = pm.document_host.get_document();
        pm.document_host.wait(cx);
        pm.update_notifier.enroll(cx);

        if !document.get_patches().ptr_eq(&pm.patch_list) {
            let pm_weak = sync::Arc::downgrade(pm_arc);
            glib::idle_add(move || {
                match pm_weak.upgrade() {
                    Some(pm) => pm.write().update_store(),
                    None => ()
                }
                glib::Continue(false)
            });
        }
    }
}

struct PatchesModelFuture {
    pm: sync::Weak<parking_lot::RwLock<PatchesModel>>,
}

impl PatchesModelFuture {
    fn new(pm: &sync::Arc<parking_lot::RwLock<PatchesModel>>) -> PatchesModelFuture {
        PatchesModelFuture {
            pm: sync::Arc::downgrade(pm),
        }
    }
}

impl std::future::Future for PatchesModelFuture {
    type Output = ();

    fn poll(self: std::pin::Pin<&mut Self>, cx: &mut task::Context) -> task::Poll<()> {
        match self.pm.upgrade() {
            Some(pm) => {
                PatchesModel::update(&pm, cx);
                task::Poll::Pending
            }
            None => task::Poll::Ready(())
        }
    }
}
