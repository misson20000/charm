use crate::catch_panic;
use crate::model::addr;
use crate::model::datapath;
use crate::model::document;
use crate::view::helpers;

use atomig::Atomic;
use atomig::Ordering;
use gtk::gio;
use gtk::glib;
use gtk::glib::clone;
use gtk::prelude::*;
use gtk::subclass::prelude::*;

use std::cell;
use std::sync;

glib::wrapper! {
    pub struct CharmExportDialog(ObjectSubclass<imp::CharmExportDialog>)
        @extends gtk::Widget, gtk::Window, gtk::ApplicationWindow,
        @implements gtk::Buildable;
}

impl CharmExportDialog {
    pub fn new() -> Self {
        glib::Object::builder().build()
    }
}

#[derive(Debug)]
enum ExportError {
    GlibIoError(glib::Error),
    JoinError(tokio::task::JoinError),
    DatapathIoError,
    NoDocumentError,
    NoFileError,
    WriteTaskDied,
    ReadThreadPanic,
}

impl From<glib::Error> for ExportError {
    fn from(e: glib::Error) -> Self {
        ExportError::GlibIoError(e)
    }
}

impl From<tokio::task::JoinError> for ExportError {
    fn from(e: tokio::task::JoinError) -> Self {
        ExportError::JoinError(e)
    }
}

mod imp {
    use super::*;
    use gtk::CompositeTemplate;

    #[derive(CompositeTemplate, Default)]
    #[template(file = "export-dialog.ui")]
    pub struct CharmExportDialog {
        #[template_child]
        address_entry: gtk::TemplateChild<gtk::Entry>,
        #[template_child]
        size_entry: gtk::TemplateChild<gtk::Entry>,
        #[template_child]
        size_display: gtk::TemplateChild<gtk::Label>,
        #[template_child]
        export_button: gtk::TemplateChild<gtk::Button>,
        #[template_child]
        file_display: gtk::TemplateChild<gtk::Entry>,
        #[template_child]
        progress: gtk::TemplateChild<gtk::ProgressBar>,
        #[template_child]
        ignore_edits: gtk::TemplateChild<gtk::CheckButton>,
        #[template_child]
        ignore_read_errors: gtk::TemplateChild<gtk::CheckButton>,
        
        file_chooser: gtk::FileChooserNative,
        document: cell::RefCell<Option<sync::Arc<document::DocumentHost>>>,
        begin_addr: cell::Cell<u64>,
        size: cell::Cell<u64>,
        entries_valid: cell::Cell<bool>,
        task: cell::RefCell<Option<helpers::AsyncSubscriber>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for CharmExportDialog {
        const NAME: &'static str = "CharmExportDialog";
        type Type = super::CharmExportDialog;
        type ParentType = gtk::ApplicationWindow;

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

    impl ObjectImpl for CharmExportDialog {
        fn constructed(&self) {
            /* FFI CALLBACK: assumed panic-safe */
            
            self.parent_constructed();

            self.address_entry.connect_text_notify(clone!(#[weak(rename_to = this)] self.obj(), move |_| catch_panic! {
                this.imp().entry_updated();
            }));

            self.size_entry.connect_text_notify(clone!(#[weak(rename_to = this)] self.obj(), move |_| catch_panic! {
                this.imp().entry_updated();
            }));

            self.file_chooser.connect_response(clone!(#[weak(rename_to = this)] self.obj(), move |fc, r| catch_panic! {
                println!("got file chooser response {:?}, {:?}", r, fc.file());
                
                match r {
                    gtk::ResponseType::Cancel => {
                        this.imp().file_chooser.hide();
                        this.imp().file_chooser.destroy();
                        this.hide();
                        this.destroy();
                    },
                    
                    gtk::ResponseType::Accept => match fc.file() {
                        Some(file) => match file.path().as_ref().and_then(|path| path.to_str()) {
                            Some(path) => this.imp().file_display.buffer().set_text(path),
                            None => this.imp().file_display.buffer().set_text(file.uri()),
                        }
                        None => {
                            this.imp().file_display.buffer().set_text("");
                        }
                    },

                    _ => {},
                }
                
                this.imp().update_enabled();
            }));
            
            self.file_chooser.set_action(gtk::FileChooserAction::Save);
            self.file_chooser.set_transient_for(Some(&*self.obj()));

            self.export_button.set_sensitive(false);
        }
    }

    impl WidgetImpl for CharmExportDialog {
    }

    impl WindowImpl for CharmExportDialog {
    }

    impl ApplicationWindowImpl for CharmExportDialog {
    }

    #[gtk::template_callbacks]
    impl CharmExportDialog {
        #[template_callback]
        fn cancel(&self, _button: &gtk::Button) {
            /* FFI CALLBACK: we catch panics */
            catch_panic! {
                self.file_chooser.hide();
                self.file_chooser.destroy();
                self.obj().hide();
                self.obj().destroy();
            }
        }

        #[template_callback]
        fn export(&self, _button: &gtk::Button) {
            /* FFI CALLBACK: we catch panics */
            catch_panic! {
                let mut task_guard = self.task.borrow_mut();

                if task_guard.is_none() {
                    let obj = self.obj().clone();
                    *task_guard = Some(helpers::spawn_on_main_context(async move {
                        if let Err(e) = obj.imp().perform().await {
                            todo!("handle error: {:?}", e);
                        }

                        obj.imp().task.borrow_mut().take();
                        obj.imp().update_enabled();
                    }));
                }

                std::mem::drop(task_guard);
                
                self.update_enabled();
            }
        }

        #[template_callback]
        fn open_file_chooser(&self, _button: &gtk::Button) {
            /* FFI CALLBACK: we catch panics */
            catch_panic! {
                self.file_chooser.show();
            }
        }
    }

    impl CharmExportDialog {
        pub fn set_document(&self, document: sync::Arc<document::DocumentHost>) {
            *self.document.borrow_mut() = Some(document);
        }

        pub fn set_addr(&self, addr: u64) {
            self.begin_addr.set(addr);
            self.address_entry.buffer().set_text(&format!("0x{:x}", addr));
        }

        pub fn set_size(&self, size: u64) {
            self.size.set(size);
            self.size_entry.buffer().set_text(&format!("0x{:x}", size));

            self.update_size_display();
        }

        fn update_size_display(&self) {
            self.size_display.set_label(&match self.size.get() {
                x if x < 1024 => format!("{} bytes", x),
                x if x < 1024 * 1024 => format!("{} KiB", x / 1024),
                x if x < 1024 * 1024 * 1024 => format!("{:.2} MiB", x as f64 / (1024.0*1024.0)),
                x if x < 1024 * 1024 * 1024 * 1024 => format!("{:.2} GiB", x as f64 / (1024.0*1024.0*1024.0)),
                x => format!("{:.2} TiB", x as f64 / (1024.0*1024.0*1024.0*1024.0)),
            });
        }

        fn entry_updated(&self) {
            let mut all_valid = true;

            match match self.address_entry.buffer().text().as_str() {
                x if x.strip_prefix("0x").is_some() => u64::from_str_radix(&x[2..], 16),
                x => u64::from_str_radix(x, 10)
            } {
                Ok(addr) => {
                    self.begin_addr.set(addr);
                    self.address_entry.remove_css_class("error");
                },
                Err(_e) => {
                    self.address_entry.add_css_class("error");
                    all_valid = false;
                },
            }
            
            match match self.size_entry.buffer().text().as_str() {
                x if x.strip_prefix("0x").is_some() => u64::from_str_radix(&x[2..], 16),
                x => u64::from_str_radix(x, 10)
            } {
                Ok(size) => {
                    self.size.set(size);
                    self.size_entry.remove_css_class("error");
                    self.update_size_display();
                },
                Err(_e) => {
                    self.size_entry.add_css_class("error");
                    all_valid = false;
                },
            }

            self.entries_valid.set(all_valid);

            self.update_enabled();
        }

        fn update_enabled(&self) {
            let task = self.task.borrow();
            self.ignore_edits.set_sensitive(task.is_none());
            self.ignore_read_errors.set_sensitive(task.is_none());
            self.export_button.set_sensitive(self.entries_valid.get() && self.file_chooser.file().is_some() && task.is_none());
        }
        
        async fn perform(&self) -> Result<(), ExportError> {
            const BLOCK_SIZE: usize = 1 * 1024 * 1024;

            let document = self.document.borrow().as_ref().cloned().ok_or(ExportError::NoDocumentError)?.get();

            let file = self.file_chooser.file().ok_or(ExportError::NoFileError)?;
            let fos = file.replace_future(None, false, gio::FileCreateFlags::REPLACE_DESTINATION, glib::Priority::LOW).await?;
            let mut atomic_buffer = Vec::new();
            atomic_buffer.resize_with(BLOCK_SIZE, || Atomic::new(0));

            let (free_data_buffers_tx, mut free_data_buffers_rx) = tokio::sync::mpsc::channel(16);
            let (full_data_buffers_tx, mut full_data_buffers_rx) = tokio::sync::mpsc::channel(16);

            for _ in 0..free_data_buffers_tx.max_capacity() {
                free_data_buffers_tx.send(Vec::new()).await.unwrap();
            }
            
            let begin_addr = self.begin_addr.get();
            let end = begin_addr + self.size.get();

            let ignore_edits = self.ignore_edits.is_active();
            let ignore_read_errors = self.ignore_read_errors.is_active();

            /* Datapath fetching has to be done on a tokio runtime. This also means we get to parallelize the reading and the writing though! */
            let read_thread = std::thread::spawn(move || {
                let rt = tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build().unwrap();
                let _guard = rt.enter();
                
                rt.block_on(async move {
                    let mut addr = begin_addr;
                    
                    while addr < end {
                        atomic_buffer.resize_with(std::cmp::min(end - addr, BLOCK_SIZE as u64) as usize, || Atomic::new(0));
                        
                        let result = document.datapath.fetch(datapath::FetchRequest::new(
                            addr,
                            &atomic_buffer[..],
                            None,
                            ignore_edits
                        )).await;

                        if result.flags.intersects(datapath::FetchFlags::IO_ERROR) && !ignore_read_errors {
                            return Err(ExportError::DatapathIoError);
                       } 

                        let mut data_buffer = match free_data_buffers_rx.recv().await {
                            Some(b) => b,
                            None => return Err(ExportError::WriteTaskDied),
                        };
                        
                        data_buffer.clear();
                        data_buffer.extend(atomic_buffer[0..result.loaded].iter().map(|b| b.load(Ordering::Relaxed)));

                        let progress_fraction = (addr - begin_addr) as f64 / (end - begin_addr) as f64;
                        if let Err(_) = full_data_buffers_tx.send((progress_fraction, data_buffer)).await {
                            return Err(ExportError::WriteTaskDied);
                        }

                        addr+= result.loaded as u64;
                    }
                    
                    Ok(())
                })
            });

            while let Some((progress_fraction, data_buffer)) = full_data_buffers_rx.recv().await {
                self.progress.set_fraction(progress_fraction);
                
                let data_buffer = match fos.write_all_future(data_buffer, glib::Priority::LOW).await {
                    Ok((buffer, _bytes, None)) => buffer,
                    Ok((_buffer, _bytes, Some(err))) => return Err(err.into()),
                    Err((_buffer, err)) => return Err(err.into()),
                };

                if free_data_buffers_tx.send(data_buffer).await.is_err() {
                    break;
                }
            }

            /* Since the channel closed, the thread should exit in a timely manner. */
            read_thread.join().map_err(|_| ExportError::ReadThreadPanic)??;
            
            fos.close_future(glib::Priority::LOW).await?;
            
            self.progress.set_fraction(1.0);
            
            Ok(())
        }
    }
}

impl CharmExportDialog {
    pub fn set(&self, document_host: sync::Arc<document::DocumentHost>, extent: addr::AbsoluteExtent) {
        self.imp().set_document(document_host);
        let (addr, size) = extent.round_out();
        self.imp().set_addr(addr);
        self.imp().set_size(size);
    }
}
