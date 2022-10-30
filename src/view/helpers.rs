use std::future;
use std::rc;
use std::sync;

use gtk::gio;
use gtk::glib;
use gtk::glib::clone;
use gtk::pango;

use crate::model::document;

pub fn bind_simple_action<T, F>(obj: &rc::Rc<T>, map: &impl gio::traits::ActionMapExt, id: &str, cb: F) -> gio::SimpleAction
where F: Fn(rc::Rc<T>) + 'static,
      T: 'static {
    let action = gio::SimpleAction::new(id, None);
    action.connect_activate(clone!(@weak obj => move |_, _| {
        cb(obj)
    }));
    action.set_enabled(true);
    map.add_action(&action);

    action
}

pub fn bind_stateful_action<T, F, S>(obj: &rc::Rc<T>, map: &impl gio::traits::ActionMapExt, id: &str, initial_state: S, cb: F) -> gio::SimpleAction
where F: Fn(&gio::SimpleAction, rc::Rc<T>, Option<S>) + 'static,
      T: 'static,
      S: glib::variant::ToVariant + glib::variant::FromVariant {
    let action = gio::SimpleAction::new_stateful(id, None, &initial_state.to_variant());
    action.connect_change_state(clone!(@weak obj => move |action, state| {
        cb(action, obj, state.and_then(|var| S::from_variant(var)))
    }));
    action.set_enabled(true);
    map.add_action(&action);

    action
}

pub struct AsyncSubscriber(Option<glib::SourceId>);

pub fn subscribe_to_document_updates<Object, F>(
    obj_weak: glib::object::WeakRef<Object>,
    document_host: sync::Arc<document::DocumentHost>,
    initial_document: sync::Arc<document::Document>,
    callback: F) -> AsyncSubscriber where
    Object: glib::object::ObjectType,
    F: Fn(Object, &sync::Arc<document::Document>) + 'static {
    spawn_on_main_context(async move {
        let mut document = initial_document;

        loop {
            let new_document = document_host.wait_for_update(&document).await;

            let obj = match obj_weak.upgrade() {
                Some(obj) => obj,
                None => return
            };

            document = new_document.clone();
            callback(obj, &document);
        };
    })
}

pub fn spawn_on_main_context<F: future::Future<Output = ()> + 'static>(task: F) -> AsyncSubscriber {
    AsyncSubscriber(Some(glib::MainContext::default().spawn_local(task)))
}

impl Drop for AsyncSubscriber {
    fn drop(&mut self) {
        if let Some(source_id) = self.0.take() {
            source_id.remove();
        }
    }
}

pub fn pango_unscale(value: i32) -> f32 {
    value as f32 / pango::SCALE as f32
}
