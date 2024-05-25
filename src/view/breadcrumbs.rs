use crate::catch_panic;
use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::cursor;
use crate::view::listing;

use gtk::prelude::*;
use gtk::subclass::prelude::*;

use std::cell;
use std::sync;

glib::wrapper! {
    pub struct CharmBreadcrumb(ObjectSubclass<imp::CharmBreadcrumb>);
}

glib::wrapper! {
    pub struct CharmBreadcrumbWidget(ObjectSubclass<imp::CharmBreadcrumbWidget>)
        @extends gtk::Button, gtk::Widget,
        @implements gtk::Buildable;
}

mod imp {
    use super::*;

    #[derive(Default)]
    pub struct CharmBreadcrumb {
        pub docpath: cell::RefCell<Option<(sync::Arc<document::Document>, structure::Path)>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for CharmBreadcrumb {
        const NAME: &'static str = "CharmBreadcrumb";
        type Type = super::CharmBreadcrumb;
        type ParentType = glib::Object;
    }

    impl ObjectImpl for CharmBreadcrumb {
        fn properties() -> &'static [glib::ParamSpec] {
            /* FFI CALLBACK: panic-safe */
            
            static PROPERTIES: once_cell::sync::Lazy<Vec<glib::ParamSpec>> =
                once_cell::sync::Lazy::new(|| vec![
                    glib::ParamSpecString::builder("name").build(),
                ]);
            PROPERTIES.as_ref()
        }

        fn property(&self, _id: usize, pspec: &glib::ParamSpec) -> glib::Value {
            catch_panic! {
                @default(glib::Value::from_type(glib::Type::INVALID));
                
                match pspec.name() {
                    "name" => match self.docpath.borrow().as_ref() {
                        Some((doc, path)) => glib::Value::from(&doc.lookup_node(path).0.props.name),
                        None => glib::Value::from_type(glib::Type::INVALID),
                    },
                    x => panic!("access to invalid property: {}", x)
                }
            }
        }
    }

    #[derive(Default)]
    pub struct CharmBreadcrumbWidget {
        pub lw: cell::RefCell<Option<listing::ListingWidget>>,
        pub breadcrumb: cell::RefCell<Option<super::CharmBreadcrumb>>,
        pub label_binding: cell::RefCell<Option<glib::Binding>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for CharmBreadcrumbWidget {
        const NAME: &'static str = "CharmBreadcrumbWidget";
        type Type = super::CharmBreadcrumbWidget;
        type ParentType = gtk::Button;
    }

    impl ObjectImpl for CharmBreadcrumbWidget {
        fn constructed(&self) {
            self.parent_constructed();

            self.obj().set_cursor(gtk::gdk::Cursor::from_name("pointer", None).as_ref());
            self.obj().add_css_class("breadcrumb");
        }
    }
    
    impl WidgetImpl for CharmBreadcrumbWidget {
    }
    
    impl ButtonImpl for CharmBreadcrumbWidget {
        fn clicked(&self) {
            let guard1 = self.lw.borrow();
            let guard2 = self.breadcrumb.borrow();
            let Some(lw) = guard1.as_ref() else { return };
            let Some(bc) = guard2.as_ref() else { return };

            match bc.imp().docpath.borrow().as_ref() {
                Some((doc, path)) => {
                    lw.goto(doc, path, addr::unit::NULL, cursor::PlacementHint::Title);
                    lw.grab_focus();
                },
                None => return,
            };
        }
    }
}

impl CharmBreadcrumb {
    pub fn new(doc: sync::Arc<document::Document>, path: structure::Path) -> Self {
        let bc = glib::Object::builder::<Self>().build();
        *bc.imp().docpath.borrow_mut() = Some((doc, path));
        bc
    }

    pub fn update(&self, new_doc: &sync::Arc<document::Document>, path: structure::PathSlice) {
        let mut guard = self.imp().docpath.borrow_mut();
        match guard.as_mut() {
            Some((current_doc, current_path)) => {
                if !sync::Arc::ptr_eq(current_doc, new_doc) {
                    *current_doc = new_doc.clone();
                }

                current_path.clear();
                current_path.extend_from_slice(path);
            },
            None => *guard = Some((new_doc.clone(), path.iter().cloned().collect())),
        }

        std::mem::drop(guard);
        self.notify("name");
    }
}

impl CharmBreadcrumbWidget {
    pub fn new(lw: listing::ListingWidget) -> Self {
        let o = glib::Object::builder::<Self>().build();
        *o.imp().lw.borrow_mut() = Some(lw);
        o
    }

    pub fn list_item_factory(lw: listing::ListingWidget) -> gtk::SignalListItemFactory {
        let slif = gtk::SignalListItemFactory::new();
        slif.connect_setup(move |_, obj| {
            let lw = lw.clone();
            catch_panic! {
                obj.set_child(Some(&Self::new(lw)));
            }
            obj.set_activatable(false);
        });
        slif.connect_bind(|_, obj| catch_panic! {
            obj.child().unwrap().downcast::<Self>().unwrap().bind(Some(obj.item().unwrap().downcast::<CharmBreadcrumb>().unwrap()));
        });
        slif.connect_unbind(|_, obj| catch_panic! {
            obj.child().unwrap().downcast::<Self>().unwrap().bind(None);
        });
        slif.connect_teardown(|_, obj| catch_panic! {
            obj.set_child(gtk::Widget::NONE);
        });        
        slif
    }

    pub fn bind(&self, breadcrumb: Option<CharmBreadcrumb>) {
        match &breadcrumb {
            Some(b) => {
                if let Some(binding) = self.imp().label_binding.replace(
                    Some(b.bind_property("name", self, "label")
                         .sync_create()
                         .build())) {
                    binding.unbind();
                }
            },
            None => {
                if let Some(binding) = self.imp().label_binding.replace(None) {
                    binding.unbind();
                }
            },
        };

        self.imp().breadcrumb.replace(breadcrumb);
    }
}
