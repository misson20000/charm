use std::cell;
use std::fmt::Write;
use std::rc;
use std::sync;

use crate::catch_panic;
use crate::model::addr;
use crate::model::document;
use crate::model::document::search;
use crate::model::listing::cursor;
use crate::view::helpers;
use crate::view::listing;
use crate::view::window;

use gtk::prelude::*;
use gtk::subclass::prelude::*;
use gtk::glib;
use gtk::glib::clone;
use gtk::gio;

struct GotoAction {
    document_host: sync::Arc<document::DocumentHost>,
    document: cell::RefCell<sync::Arc<document::Document>>,

    lw: listing::ListingWidget,
    
    dialog: gtk::ApplicationWindow,
    entry: gtk::Entry,

    model: gtk::SingleSelection,
    store: gio::ListStore,
    current_addr: cell::Cell<addr::Address>,
    
    subscriber: once_cell::unsync::OnceCell<helpers::AsyncSubscriber>,
}

pub fn add_action(window_context: &window::WindowContext) {
    let builder = gtk::Builder::from_string(include_str!("goto.ui"));

    let entry: gtk::Entry = builder.object("entry").unwrap();
    let list: gtk::ListView = builder.object("list").unwrap();
    //let cancel_button: gtk::Button = builder.object("cancel_button").unwrap();

    let dialog = gtk::ApplicationWindow::builder()
        .application(&window_context.window.upgrade().unwrap().application.application)
        .child(&builder.object::<gtk::Widget>("toplevel").unwrap())
        .resizable(true)
        .title("Goto")
        .transient_for(&window_context.window.upgrade().unwrap().window)
        .hide_on_close(true)
        .destroy_with_parent(true)
        .default_widget(&list)
        .build();

    let store = gio::ListStore::new::<HitItem>();
    
    let action = rc::Rc::new(GotoAction {
        document_host: window_context.project.document_host.clone(),
        document: cell::RefCell::new(window_context.project.document_host.get()),
        lw: window_context.lw.clone(),
        dialog: dialog.clone(),
        entry,

        model: gtk::SingleSelection::new(Some(store.clone())),
        store,
        current_addr: cell::Cell::new(addr::unit::NULL),
        
        subscriber: once_cell::unsync::OnceCell::new(),
    });

    list.set_model(Some(&action.model));
    list.set_factory(Some(&gtk::BuilderListItemFactory::from_bytes(gtk::BuilderScope::NONE, &glib::Bytes::from_static(include_bytes!("goto-item.ui")))));
    list.connect_activate(clone!(#[weak] action, move |_, position| catch_panic! {
        action.do_goto(action.model.item(position));
    }));
    
    action.entry.connect_changed(clone!(#[weak] action, move |_| catch_panic! {
        action.refresh_results(None, false);
    }));

    action.entry.connect_activate(clone!(#[weak] action, move |_| catch_panic! {
        action.do_goto(action.model.item(0));
    }));

    action.subscriber.set(helpers::subscribe_to_updates(rc::Rc::downgrade(&action), action.document_host.clone(), action.document.borrow().clone(), |action, new_document| catch_panic! {
        action.refresh_results(Some(new_document.clone()), false);
    })).unwrap();
    
    helpers::bind_simple_action(&action, &action.dialog, "cancel", |action| {
        action.dialog.hide();
    });

    window_context.action_group.add_action(&helpers::create_simple_action_strong(action, "goto", |ina| ina.activate()));
}

impl GotoAction {
    fn activate(&self) {
        let cursor = self.lw.cursor();

        self.entry.set_text(&format!("{}", cursor.structure_offset()));
        self.entry.grab_focus();
        self.refresh_results(None, true);
        
        self.dialog.present();
    }

    fn refresh_results(&self, new_document: Option<sync::Arc<document::Document>>, force: bool) {
        let new_addr = addr::Address::parse(self.entry.text().as_str()).ok().and_then(|na| if na == self.current_addr.get() { None } else { Some(na) });

        if new_addr.is_some() || new_document.is_some() || force {
            if let Some(new_document) = new_document {
                *self.document.borrow_mut() = new_document;
            }

            if !self.dialog.is_visible() && !force {
                return;
            }
            
            if let Some(new_addr) = new_addr {
                self.current_addr.set(new_addr);
            }
            
            self.store.remove_all();

            let document = self.document.borrow();
            if let Ok(iter) = document.search_addr(self.current_addr.get(), document::search::Traversal::PostOrder) {
                for hit in iter {
                    self.store.append(&HitItem::new(document.clone(), hit));
                }
            }
        }
    }

    fn do_goto(&self, item: Option<glib::Object>) {
        let item = match item.and_then(|item| item.downcast::<HitItem>().ok()) {
            Some(item) => item,
            None => return
        };

        let item_interior = item.imp().interior.get().unwrap();
        
        self.lw.goto(&item_interior.document, &item_interior.hit.path, item_interior.hit.offset.to_addr(), cursor::PlacementHint::Unused);

        self.dialog.hide();

        /* clearing out the textbox helps it always select the entire string when we grab_focus() it */
        self.entry.set_text("");
    }
}

impl Drop for GotoAction {
    fn drop(&mut self) {
        self.dialog.destroy();
    }
}

mod imp {
    use super::*;

    #[derive(Debug)]
    pub struct HitItemInterior {
        pub document: sync::Arc<document::Document>,
        pub hit: search::Hit,
        path_description: String,
    }
    
    #[derive(Default)]
    pub struct HitItem {
        pub interior: once_cell::unsync::OnceCell<HitItemInterior>
    }

    #[glib::object_subclass]
    impl ObjectSubclass for HitItem {
        const NAME: &'static str = "CharmHitItem";
        type Type = super::HitItem;
        type ParentType = glib::Object;
    }

    impl ObjectImpl for HitItem {
        fn properties() -> &'static [glib::ParamSpec] {
            static PROPERTIES: once_cell::sync::Lazy<Vec<glib::ParamSpec>> =
                once_cell::sync::Lazy::new(|| vec![
                    glib::ParamSpecString::builder("path-description").build(),
                ]);
            PROPERTIES.as_ref()
        }

        fn property(&self, _id: usize, pspec: &glib::ParamSpec) -> glib::Value {
            catch_panic! {
                @default(glib::value::Value::from_type(glib::types::Type::INVALID));
                
                let interior = self.interior.get().unwrap();
                match pspec.name() {
                    "path-description" => glib::value::ToValue::to_value(&interior.path_description),
                    _ => unimplemented!()
                }
            }
        }
    }

    impl HitItem {
        pub fn init(&self, document: sync::Arc<document::Document>, hit: search::Hit) {
            let mut path_description = document.describe_path(&hit.path);
            write!(path_description, " + {}", addr::fmt::CompactSize(hit.offset)).unwrap();
            
            self.interior.set(HitItemInterior {
                document, hit, path_description
            }).unwrap();
        }
    }
}

glib::wrapper! {
    pub struct HitItem(ObjectSubclass<imp::HitItem>)
        ;
}

impl HitItem {
    fn new(document: sync::Arc<document::Document>, hit: search::Hit) -> Self {
        let item: Self = glib::Object::builder().build();
        item.imp().init(document, hit);
        item
    }
}
