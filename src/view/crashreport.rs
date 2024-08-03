use crate::model;
use crate::model::document;
use crate::model::versioned::Versioned;
use crate::serialization;
use crate::view;
use crate::view::error;
use crate::view::project;
use crate::view::window;

use gtk::prelude::*;
use gtk::gio;
use gtk::glib;
use gtk::glib::clone;
use gtk::glib::subclass::prelude::*;

use lazy_static::lazy_static;

use std::backtrace;
use std::cell;
use std::fmt::Write;
use std::panic;
use std::rc;
use std::sync;
use std::vec;

struct Location {
    file: String,
    line: u32,
    column: u32,
}

struct Panic {
    payload: Option<String>,
    location: Option<Location>,
    backtrace: backtrace::Backtrace,
    circumstances: Vec<Circumstance>,
}

#[derive(Clone)]
pub enum Circumstance {
    InvokingTestCrashAction,
    InWindow(u64),
    TreeSelectionUpdate(sync::Arc<model::selection::tree::Host>, sync::Arc<document::Document>),
    ListingSelectionUpdate(sync::Arc<model::selection::listing::Host>, sync::Arc<document::Document>),
    Goto {
        document: sync::Arc<document::Document>,
        path: document::structure::Path,
        offset: model::addr::Address,
        hint: String,
    },
    LWDocumentUpdate(sync::Arc<document::Document>, sync::Arc<document::Document>),
    LWSelectionUpdate(sync::Arc<model::selection::ListingSelection>, sync::Arc<model::selection::ListingSelection>),
}

pub struct CircumstanceGuard {
    begin: usize,
    end: usize,

    /* Used to make CircumstanceGuard !Send */
    _marker: std::marker::PhantomData<*const()>
}

lazy_static! {
    static ref QUEUED_PANICS: sync::Mutex<vec::Vec<Panic>> = Default::default();
}
static PANIC_DIALOG_OPEN: sync::atomic::AtomicBool = sync::atomic::AtomicBool::new(false);

thread_local! {
    static PANICKING: cell::Cell<bool> = cell::Cell::new(false);
    static CIRCUMSTANCES: cell::RefCell<Vec<Circumstance>> = cell::RefCell::new(vec::Vec::new());
}

pub fn install_hook(charm: rc::Rc<view::CharmApplication>) {
    let charm = sync::Arc::new(glib::thread_guard::ThreadGuard::new(charm));
    std::panic::set_hook(Box::new(move |pi| panic_hook(pi, charm.clone())));
}

pub fn circumstances<I: std::iter::IntoIterator<Item = Circumstance>>(c: I) -> CircumstanceGuard {
    CIRCUMSTANCES.with_borrow_mut(|v| {
        let begin = v.len();
        v.extend(c);
        let end = v.len();

        CircumstanceGuard {
            begin,
            end,
            _marker: std::marker::PhantomData,
        }
    })
}

impl Drop for CircumstanceGuard {
    fn drop(&mut self) {
        CIRCUMSTANCES.with_borrow_mut(|v| {
            assert!(v.len() == self.end);
            v.splice(self.begin..self.end, []);
        });
    }
}

fn panic_hook(pi: &panic::PanicInfo, charm: sync::Arc<glib::thread_guard::ThreadGuard<rc::Rc<view::CharmApplication>>>) {
    if PANICKING.replace(true) {
        println!("Double faulted: {}", pi);
        std::process::abort();
    }

    println!("Caught panic: {}", pi);
    
    let payload = pi.payload();
    
    let panic = Panic {
        payload: match payload.downcast_ref::<String>() {
            Some(s) => Some(s.clone()),
            None => match payload.downcast_ref::<&'static str>() {
                Some(s) => Some(s.to_string()),
                None => None,
            }
        },
        location: pi.location().map(|loc| Location {
            file: loc.file().to_string(),
            line: loc.line(),
            column: loc.column(),
        }),
        backtrace: backtrace::Backtrace::force_capture(),
        circumstances: CIRCUMSTANCES.with_borrow(|v| v.clone()),
    };

    QUEUED_PANICS.lock().unwrap().push(panic);

    glib::MainContext::default().spawn(async move {
        PANICKING.set(true);
        
        if PANIC_DIALOG_OPEN.swap(true, sync::atomic::Ordering::Relaxed) {
            println!("Attempted to open panic dialog when it was already open.");
            std::process::abort();
        }
        
        /* Build and open the crash report dialog first so we don't immediately exit the application when we close the last CharmWindow. */
        let builder = gtk::Builder::from_string(include_str!("crash-dialog.ui"));
        let dialog = gtk::ApplicationWindow::builder()
            .application(&charm.get_ref().application)
            .child(&builder.object::<gtk::Widget>("toplevel").unwrap())
            .resizable(true)
            .title("Charm panic recovery")
            .modal(true)
            .build();

        dialog.present();

        let report_buffer = builder.object::<gtk::TextBuffer>("report_buffer").unwrap();
        let mut report = String::new();

        let panics = std::mem::replace(&mut *QUEUED_PANICS.lock().unwrap(), vec::Vec::new());
        for panic in &panics {
            if let Some(l) = &panic.location {
                write!(report, "{}:{}:{}: ", l.file, l.line, l.column).unwrap();
            }

            if let Some(p) = &panic.payload {
                write!(report, "{}\n", p).unwrap();
            } else {
                write!(report, "<non-String payload>\n").unwrap();
            }

            for c in &panic.circumstances {
                c.describe(&mut report).unwrap();
            }

            write!(report, "\nBacktrace:\n{}\n\n", panic.backtrace).unwrap();
        }
        report_buffer.set_text(&report);

        let store = gio::ListStore::new::<CharmRecoverableDocument>();
        
        {
            /* For every window that is open, try to convert it to a CharmRecoverableDocument and close it. */
            let mut windows = charm.get_ref().windows.borrow_mut();
            
            for w in &*windows {
                if let Some(w) = w.upgrade() {
                    if let Some(ctx) = &*w.context() {
                        let rd = CharmRecoverableDocument::new(dialog.clone(), ctx, w.id);

                        for p in &panics {
                            for c in &p.circumstances {
                                rd.check_affected_by(c);
                            }
                        }
                        
                        store.append(&rd);
                    }
                    w.close_project();
                    w.window.destroy();
                }
            }

            windows.clear();
        }

        let documents = builder.object::<gtk::ListView>("documents").unwrap();

        documents.set_model(Some(&gtk::NoSelection::new(Some(store.clone()))));

        let lif = gtk::SignalListItemFactory::new();
        lif.connect_setup(|_, obj| {
            /* FFI CALLBACK: already panicking, just double-abort */
            obj.downcast_ref::<gtk::ListItem>().unwrap().set_child(Some(&CharmDocumentRecoveryWidget::new()));
        });
        lif.connect_bind(|_, obj| {
            /* FFI CALLBACK: already panicking, just double-abort */
            let li = obj.downcast_ref::<gtk::ListItem>().unwrap();
            li.child().unwrap().downcast::<CharmDocumentRecoveryWidget>().unwrap().bind(li.item());
        });
        lif.connect_unbind(|_, obj| {
            /* FFI CALLBACK: already panicking, just double-abort */
            let li = obj.downcast_ref::<gtk::ListItem>().unwrap();
            li.child().unwrap().downcast::<CharmDocumentRecoveryWidget>().unwrap().bind(li.item());
        });
        lif.connect_teardown(|_, obj| {
            /* FFI CALLBACK: already panicking, just double-abort */
            obj.downcast_ref::<gtk::ListItem>().unwrap().set_child(gtk::Widget::NONE);
        });
        
        documents.set_factory(Some(&lif));

        let reopen_action = gio::SimpleAction::new("reopen", None);
        reopen_action.connect_activate(clone!(#[strong] dialog, #[strong] store, move |_, _| {
            /* FFI CALLBACK: already panicking, just double-abort */
            for obj in &store {
                let Ok(obj) = obj else { continue };
                let Ok(crd) = obj.downcast::<CharmRecoverableDocument>() else { continue };
                
                let Some(project) = crd.create_project() else { continue };

                let window = charm.get_ref().new_window();
                window.open_project(project, true);
                window.present();
            }

            PANIC_DIALOG_OPEN.store(false, sync::atomic::Ordering::Relaxed);
            dialog.destroy();
        }));
        dialog.add_action(&reopen_action);

        let select_all_action = gio::SimpleAction::new("select_all", None);
        select_all_action.connect_activate(clone!(#[strong] store, move |_, _| {
            /* FFI CALLBACK: already panicking, just double-abort */
            for obj in &store {
                let Ok(obj) = obj else { continue };
                obj.set_property("should-recover", true);
            }
        }));
        dialog.add_action(&select_all_action);

        let deselect_all_action = gio::SimpleAction::new("deselect_all", None);
        deselect_all_action.connect_activate(clone!(#[strong] store, move |_, _| {
            /* FFI CALLBACK: already panicking, just double-abort */
            for obj in &store {
                let Ok(obj) = obj else { continue };
                obj.set_property("should-recover", false);
            }
        }));
        dialog.add_action(&deselect_all_action);
        
        PANICKING.set(false);
    });
    
    PANICKING.set(false);
}

glib::wrapper! {
    pub struct CharmDocumentRecoveryWidget(ObjectSubclass<imp::CharmDocumentRecoveryWidget>)
        @extends gtk::Box, gtk::Widget,
        @implements gtk::Buildable;
}


glib::wrapper! {
    pub struct CharmRecoverableDocument(ObjectSubclass<imp::CharmRecoverableDocument>);
}

impl CharmDocumentRecoveryWidget {
    pub fn new() -> Self {
        glib::Object::builder().build()
    }

    pub fn bind(&self, doc: Option<glib::Object>) {
        match doc.and_then(|d| d.downcast::<CharmRecoverableDocument>().ok()) {
            Some(rd) => {
                self.imp().title.get().set_label(&rd.property::<String>("title"));
                
                if let Some(b) = self.imp().should_recover_binding.replace(
                    Some(rd.bind_property("should-recover", &self.imp().should_recover.get(), "active")
                         .bidirectional()
                         .sync_create()
                         .build())) {
                    b.unbind();
                }

                if let Some(b) = self.imp().panic_related_binding.replace(
                    Some(rd.bind_property("panic-related", &self.imp().panic_related.get(), "visible")
                         .sync_create()
                         .build())) {
                    b.unbind();
                }
                
                let version_model = gtk::StringList::new(&[]);

                let mut current_version = rd.imp().document.get().unwrap();
                version_model.append("Most recent");
                for _ in 0..20 {
                    let Some((previous, change)) = current_version.previous() else { break };
                    
                    version_model.append(&format!("Before '{}'", change.summarize(&*previous)));
                    current_version = previous;
                }
                
                self.imp().version.get().set_model(Some(&version_model));

                if let Some(b) = self.imp().rollback_binding.replace(
                    Some(rd.bind_property("rollback", &self.imp().version.get(), "selected")
                         .bidirectional()
                         .sync_create()
                         .build())) {
                    b.unbind();
                }

                *self.imp().bound_document.borrow_mut() = Some(rd);
            },
            None => {
                self.imp().title.get().set_label("<UNBOUND>");
                
                if let Some(b) = self.imp().should_recover_binding.replace(None) {
                    b.unbind();
                }
                
                if let Some(b) = self.imp().rollback_binding.replace(None) {
                    b.unbind();
                }

                self.imp().version.get().set_model(gio::ListModel::NONE);

                *self.imp().bound_document.borrow_mut() = None;
            },
        }
    }
}

impl CharmRecoverableDocument {
    pub fn new(crash_dialog: gtk::ApplicationWindow, wctx: &window::WindowContext, wid: u64) -> Self {
        let obj: Self = glib::Object::builder()
            .property("should-recover", true)
            .build();

        obj.imp().crash_dialog.set(crash_dialog).unwrap();
        obj.imp().window_id.set(wid).unwrap();
        obj.imp().document.set(wctx.project.document_host.get()).unwrap();
        obj.imp().project.set(wctx.project.clone()).unwrap();
        obj
    }

    fn chosen_document(&self) -> Option<&sync::Arc<document::Document>> {
        let Some(mut doc) = self.imp().document.get() else { return None };
        for _ in 0..self.imp().rollback.get() {
            doc = &doc.previous()?.0;
        }

        Some(doc)
    }

    fn create_project(&self) -> Option<project::Project> {
        if !self.imp().should_recover.get() {
            return None;
        }

        let Some(doc) = self.imp().document.get() else { return None };
        let Some(project) = self.imp().project.get() else { return None };
        
        Some(project.revert_from(doc, self.imp().rollback.get()))
    }
    
    fn check_affected_by(&self, c: &Circumstance) {
        let wid = self.imp().window_id.get().unwrap_or(&0);
        
        match c {
            Circumstance::InWindow(id) if id == wid => {
                self.imp().panic_related.set(true);
            },
            _ => {},
        }
    }
    
    fn save_as(&self) {
        let Some(doc) = self.chosen_document().cloned() else { return };

        let crash_dialog = self.imp().crash_dialog.get().unwrap();
        let file_chooser_dialog = view::action::save_project::create_dialog(crash_dialog.upcast_ref());

        if let Some(project) = self.imp().project.get() {
            let save_file_borrow = project.save_file.borrow();
            
            if let Some(save_file) = save_file_borrow.as_ref() {
                let _ = file_chooser_dialog.set_file(save_file);
            }
        }

        file_chooser_dialog.show();

        file_chooser_dialog.connect_response(clone!(#[strong(rename_to=rd)] self, #[strong] crash_dialog, move |dialog, response_type| {
            *rd.imp().file_chooser_dialog.borrow_mut() = None;
            dialog.destroy();
            
            match response_type {
                gtk::ResponseType::Accept => if let Some(file) = dialog.file() {
                    if let Err(e) = serialization::serialize_project(&doc)
                        .map_err(|e| error::Error {
                            while_attempting: error::Action::SaveRecoveredDocument,
                            trouble: error::Trouble::ProjectSerializationFailure(e),
                            level: error::Level::Error,
                            is_bug: true,
                        })
                        .and_then(|bytes| {
                            file.replace_contents(&bytes[..], None, false, gio::FileCreateFlags::REPLACE_DESTINATION, gio::Cancellable::NONE)
                                .map_err(|e| error::Error {
                                    while_attempting: error::Action::SaveRecoveredDocument,
                                    trouble: error::Trouble::GlibIoError(e),
                                    level: error::Level::Error,
                                    is_bug: false,
                                })
                        }) {
                            e.create_dialog(&crash_dialog).present();
                        }
                },
                _ => {},
            }
        }));

        /* Need this object to live. */
        *self.imp().file_chooser_dialog.borrow_mut() = Some(file_chooser_dialog);
    }
}

mod imp {
    use super::*;
    use gtk::subclass::prelude::*;
    use gtk::CompositeTemplate;

    use std::cell;
    
    #[derive(CompositeTemplate, Default)]
    #[template(file = "crash-document-recovery.ui")]
    pub struct CharmDocumentRecoveryWidget {
        #[template_child]
        pub title: gtk::TemplateChild<gtk::Label>,
        #[template_child]
        pub should_recover: gtk::TemplateChild<gtk::CheckButton>,
        #[template_child]
        pub panic_related: gtk::TemplateChild<gtk::Label>,
        #[template_child]
        pub save_as: gtk::TemplateChild<gtk::Button>,
        #[template_child]
        pub version: gtk::TemplateChild<gtk::DropDown>,

        pub bound_document: cell::RefCell<Option<super::CharmRecoverableDocument>>,
        pub should_recover_binding: cell::Cell<Option<glib::Binding>>,
        pub panic_related_binding: cell::Cell<Option<glib::Binding>>,
        pub rollback_binding: cell::Cell<Option<glib::Binding>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for CharmDocumentRecoveryWidget {
        const NAME: &'static str = "CharmDocumentRecoveryWidget";
        type Type = super::CharmDocumentRecoveryWidget;
        type ParentType = gtk::Box;

        fn class_init(klass: &mut Self::Class) {
            /* FFI CALLBACK: assumed panic-safe */
            klass.bind_template();
            klass.bind_template_callbacks();
        }

        fn instance_init(obj: &glib::subclass::InitializingObject<Self>) {
            /* FFI CALLBACK: assumed panic-safe */
            obj.init_template();
        }
    }

    impl ObjectImpl for CharmDocumentRecoveryWidget {
        fn constructed(&self) {
            /* FFI CALLBACK: assumed panic-safe */
            
            self.parent_constructed();

            let css = gtk::CssProvider::new();
            css.load_from_data(include_str!("crashreport.css"));
            self.panic_related.style_context().add_provider(&css, gtk::STYLE_PROVIDER_PRIORITY_APPLICATION);
        }
    }

    impl WidgetImpl for CharmDocumentRecoveryWidget {
    }

    impl BoxImpl for CharmDocumentRecoveryWidget {
    }

    #[gtk::template_callbacks]
    impl CharmDocumentRecoveryWidget {
        #[template_callback]
        fn handle_save_as(&self, _button: &gtk::Button) {
            match &*self.bound_document.borrow() {
                Some(rd) => rd.save_as(),
                None => {},
            }
        }
    }

    #[derive(Default)]
    pub struct CharmRecoverableDocument {
        pub crash_dialog: cell::OnceCell<gtk::ApplicationWindow>,
        pub file_chooser_dialog: cell::RefCell<Option<gtk::FileChooserNative>>,
        pub window_id: cell::OnceCell<u64>,

        pub document: cell::OnceCell<sync::Arc<document::Document>>,
        pub project: cell::OnceCell<rc::Rc<project::Project>>,
        
        pub should_recover: cell::Cell<bool>,
        pub panic_related: cell::Cell<bool>,
        pub rollback: cell::Cell<u32>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for CharmRecoverableDocument {
        const NAME: &'static str = "CharmRecoverableDocument";
        type Type = super::CharmRecoverableDocument;
        type ParentType = glib::Object;
    }

    impl ObjectImpl for CharmRecoverableDocument {
        fn properties() -> &'static [glib::ParamSpec] {
            /* FFI CALLBACK: panic-safe */
            
            static PROPERTIES: once_cell::sync::Lazy<Vec<glib::ParamSpec>> =
                once_cell::sync::Lazy::new(|| vec![
                    glib::ParamSpecString::builder("title").build(),
                    glib::ParamSpecBoolean::builder("should-recover").build(),
                    glib::ParamSpecBoolean::builder("panic-related").build(),
                    glib::ParamSpecUInt::builder("rollback").build(),
                ]);
            PROPERTIES.as_ref()
        }

        fn property(&self, _id: usize, pspec: &glib::ParamSpec) -> glib::Value {
            /* FFI CALLBACK: panic-safe */
            
            match pspec.name() {
                "title" => match self.project.get() {
                    Some(p) => glib::Value::from(&p.title()),
                    None => glib::Value::from(&"<INVALID?>"),
                },
                "should-recover" => glib::Value::from(&self.should_recover.get()),
                "panic-related" => glib::Value::from(&self.panic_related.get()),
                "rollback" => glib::Value::from(self.rollback.get()), // TODO
                _ => glib::Value::from_type(glib::Type::INVALID),
            }
        }

        fn set_property(&self, _id: usize, value: &glib::Value, pspec: &glib::ParamSpec) {
            /* FFI CALLBACK: panic-safe */

            match pspec.name() {
                "should-recover" => if let Ok(v) = value.get() {
                    self.should_recover.set(v)
                },
                "panic-related" => if let Ok(v) = value.get() {
                    self.panic_related.set(v)
                },
                "rollback" => if let Ok(v) = value.get() {
                    self.rollback.set(v)
                },
                _ => {},
            }
        }
    }
}

impl Circumstance {
    fn describe(&self, d: &mut String) -> Result<(), std::fmt::Error> {
        match self {
            Circumstance::InvokingTestCrashAction => write!(d, "While invoking test crash action.\n")?,
            Circumstance::InWindow(_) => { /* used elsewhere */ },
            Circumstance::TreeSelectionUpdate(tsh, doc) => {
                write!(d, "While updating tree selection for new document.\n")?;
                let ts = tsh.get();
                write!(d, "  selection: {:?}\n", ts)?;
                write!(d, "  document changes:\n")?;

                doc.changes_since(&ts.document, &mut |_, change| {
                    write!(d, "    - {:?}\n", change).unwrap();
                });
            },
            Circumstance::ListingSelectionUpdate(lsh, doc) => {
                write!(d, "While updating listing selection for new document.\n")?;
                let ls = lsh.get();
                write!(d, "  selection: {:?}\n", ls)?;
                write!(d, "  document changes:\n")?;

                doc.changes_since(&ls.document, &mut |_, change| {
                    write!(d, "    - {:?}\n", change).unwrap();
                });
            },
            Circumstance::Goto { document: _, path, offset, hint } => {
                write!(d, "While performing goto({:?}, {}, {}).\n", path, offset, hint)?;
            },
            Circumstance::LWDocumentUpdate(old, new) => {
                write!(d, "While updating listing widget for new document.\n")?;
                new.changes_since(&old, &mut |_, change| {
                    write!(d, "  - {:?}\n", change).unwrap();
                });
            },
            Circumstance::LWSelectionUpdate(old, new) => {
                write!(d, "While updating listing widget for new selection.\n")?;
                new.changes_since(&old, &mut |_, change| {
                    write!(d, "  - {:?}\n", change).unwrap();
                });
            },
        };

        Ok(())
    }
}
