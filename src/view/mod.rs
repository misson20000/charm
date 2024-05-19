use gtk::gio;
use gtk::glib::clone;
use gtk::prelude::*;

pub mod action;
pub mod config;
pub mod error;
pub mod ext;
pub mod window;
pub mod helpers;
pub mod gsc;

pub mod config_editor;
pub mod crashreport;
pub mod datapath;
pub mod hierarchy;
pub mod listing;
pub mod props_editor;
pub mod selection;

use std::cell;
use std::rc;

use crate::catch_panic;

pub struct CharmApplication {
    application: gtk::Application,
    rt: tokio::runtime::Runtime,

    about_dialog: gtk::AboutDialog,
    windows: cell::RefCell<Vec<rc::Weak<window::CharmWindow>>>,
}

impl CharmApplication {
    fn new(application: gtk::Application) -> rc::Rc<CharmApplication> {
        let app = rc::Rc::new(CharmApplication {
            application,
            rt: tokio::runtime::Builder::new_multi_thread()
                .enable_all()
                .build().unwrap(),

            about_dialog: Self::create_about_dialog(),
            windows: Default::default(),
        });

        /* application actions */

        helpers::bind_simple_action(&app, &app.application, "new_window", |app| {
            app.action_new_window();
        });
        
        helpers::bind_simple_action(&app, &app.application, "about", |app| {
            app.action_about();
        });

        helpers::bind_simple_action(&app, &app.application, "force_exit", |app| {
            app.application.quit();
        });
        
        helpers::bind_simple_action(&app, &app.application, "crash", |_| {
            let _circumstances = crashreport::circumstances([crashreport::Circumstance::InvokingTestCrashAction]);
            panic!("Test crash caused by debug action.");
        });

        /* accelerators */
        app.application.set_accels_for_action("app.new_window", &["<Ctrl>N"]);
        app.application.set_accels_for_action("win.open", &["<Ctrl>O"]);
        app.application.set_accels_for_action("win.save_project", &["<Ctrl>S"]);
        app.application.set_accels_for_action("win.cancel", &["Escape"]);
        app.application.set_accels_for_action("ctx.debug.revert_document(uint32 1)", &["<Ctrl>Z"]);

        app
    }

    fn action_new_window(self: &rc::Rc<Self>) {
        let w = self.new_window();
        w.present();
    }
    
    fn action_about(self: &rc::Rc<Self>) {
        self.about_dialog.present();
    }

    fn new_window(self: &rc::Rc<Self>) -> rc::Rc<window::CharmWindow> {
        let window = window::CharmWindow::new(self);
        self.windows.borrow_mut().push(rc::Rc::downgrade(&window));
        window
    }

    fn destroy_window(&self, window: &rc::Rc<window::CharmWindow>) {
        let window = rc::Rc::downgrade(window);
        self.windows.borrow_mut().retain(|w| !rc::Weak::ptr_eq(&w, &window) && w.upgrade().is_some());
    }
    
    fn create_about_dialog() -> gtk::AboutDialog {
        let about_dialog = gtk::AboutDialog::new();

        about_dialog.set_authors(&["misson20000 <xenotoad@xenotoad.net>"]);
        about_dialog.set_comments(Some("A listing editor that tries to be at least not bad."));
        about_dialog.set_copyright(Some("Copyright 2020 misson20000"));
        about_dialog.set_license_type(gtk::License::Gpl20Only);
        about_dialog.set_program_name(Some("Charm"));
        about_dialog.set_version(Some("0.1.0"));
        about_dialog.set_website(Some("https://charm.xenotoad.net"));

        about_dialog
    }
}

fn setup_tracing() {
    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .pretty()
        .with_max_level(tracing::Level::TRACE)
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");
}

pub fn launch_application() {
    setup_tracing();
    
    /* we defer initializing CharmApplication until the startup signal */
    let app_model_for_closures: rc::Rc<once_cell::unsync::OnceCell<rc::Rc<CharmApplication>>> =
        rc::Rc::new(once_cell::unsync::OnceCell::new());
    
    let application =
        gtk::Application::new(
            Some("net.xenotoad.charm"),
            gio::ApplicationFlags::HANDLES_OPEN);

    /* setup signals */
    
    /* startup */
    application.connect_startup(clone!(@strong app_model_for_closures => move |app| catch_panic! {        
        // TODO: figure out gtk4 icon hellscape
        let charm = CharmApplication::new(app.clone());
        
        crashreport::install_hook(charm.clone());
        
        if app_model_for_closures.set(charm).is_err() {
            panic!("started up more than once?");
        }
    }));

    /* activate */
    application.connect_activate(clone!(@strong app_model_for_closures => move |_app| catch_panic! {
        app_model_for_closures.get().unwrap().action_new_window();
    }));

    /* open */
    application.connect_open(clone!(@strong app_model_for_closures => move |_app, files, _hint| catch_panic! {
        for file in files {
            let w = app_model_for_closures.get().unwrap().new_window();
            w.open_file(file);
            w.present();
        }
    }));
    
    application.run();
}
