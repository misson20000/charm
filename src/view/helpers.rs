use std::future;
use std::rc;
use std::sync;

use gtk::gio;
use gtk::glib;
use gtk::glib::clone;
use gtk::pango;

use crate::catch_panic;
use crate::model::versioned;

pub fn create_simple_action_strong<T, F>(obj: rc::Rc<T>, id: &str, cb: F) -> gio::SimpleAction
where F: Fn(&rc::Rc<T>) + 'static,
      T: 'static {
    let action = gio::SimpleAction::new(id, None);
    action.connect_activate(move |_, _| catch_panic! {
        cb(&obj)
    });
    action.set_enabled(true);

    action
}

pub fn create_simple_action<T, F>(obj: &rc::Rc<T>, id: &str, cb: F) -> gio::SimpleAction
where F: Fn(rc::Rc<T>) + 'static,
      T: 'static {
    let action = gio::SimpleAction::new(id, None);
    action.connect_activate(clone!(@weak obj => move |_, _| catch_panic! {
        cb(obj)
    }));
    action.set_enabled(true);

    action
}

pub fn bind_simple_action<T, F>(obj: &rc::Rc<T>, map: &impl gio::prelude::ActionMapExt, id: &str, cb: F) -> gio::SimpleAction
where F: Fn(rc::Rc<T>) + 'static,
      T: 'static {
    let action = create_simple_action(obj, id, cb);
    map.add_action(&action);
    action
}

pub fn bind_stateful_action<T, F, S>(obj: &rc::Rc<T>, map: &impl gio::prelude::ActionMapExt, id: &str, initial_state: S, cb: F) -> gio::SimpleAction
where F: Fn(&gio::SimpleAction, rc::Rc<T>, Option<S>) + 'static,
      T: 'static,
      S: glib::variant::ToVariant + glib::variant::FromVariant {
    let action = gio::SimpleAction::new_stateful(id, None, &initial_state.to_variant());
    action.connect_change_state(clone!(@weak obj => move |action, state| catch_panic! {
        cb(action, obj, state.and_then(|var| S::from_variant(var)))
    }));
    action.set_enabled(true);
    map.add_action(&action);

    action
}

#[derive(Debug)]
pub struct AsyncSubscriber(Option<glib::JoinHandle<()>>);

pub fn subscribe_to_updates<SubscriberRef, F, Object: versioned::Versioned + 'static>(
    subscriber_weak: SubscriberRef,
    host: sync::Arc<versioned::Host<Object>>,
    initial_object: sync::Arc<Object>,
    mut callback: F) -> AsyncSubscriber where
    SubscriberRef: glib::clone::Upgrade + 'static,
    F: FnMut(SubscriberRef::Strong, &sync::Arc<Object>) + 'static {
    spawn_on_main_context(async move {
        let mut object = initial_object;

        loop {
            let new_object = host.wait_for_update(&object).await;
            
            let subscriber = match subscriber_weak.upgrade() {
                Some(subscriber) => subscriber,
                None => {
                    return
                }
            };

            object = new_object.clone();

            catch_panic! {
                callback(subscriber, &object);
            }
        };
    })
}

/// This callback is an FFI boundary.
pub fn spawn_on_main_context<F: future::Future<Output = ()> + 'static>(task: F) -> AsyncSubscriber {
    AsyncSubscriber(Some(glib::MainContext::default().spawn_local(task)))
}

impl Drop for AsyncSubscriber {
    fn drop(&mut self) {
        if let Some(jh) = self.0.take() {
            jh.abort();
        }
    }
}

pub fn pango_unscale(value: i32) -> f32 {
    value as f32 / pango::SCALE as f32
}
