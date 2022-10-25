use gtk::gio;
use gtk::glib::clone;
use gtk::prelude::*;

pub mod config;
pub mod ext;
pub mod window;
pub mod helpers;
pub mod gsc;

pub mod config_editor;
pub mod datapath;
pub mod hierarchy;
pub mod listing;

use std::rc;

pub struct CharmApplication {
    application: gtk::Application,
    rt: tokio::runtime::Runtime,

    about_dialog: gtk::AboutDialog,
}

impl CharmApplication {
    fn new(application: gtk::Application) -> rc::Rc<CharmApplication> {
        let app = rc::Rc::new(CharmApplication {
            application,
            rt: tokio::runtime::Builder::new_multi_thread()
                .enable_all()
                .build().unwrap(),

            about_dialog: Self::create_about_dialog(),
        });

        /* application actions */

        helpers::bind_simple_action(&app, &app.application, "new_window", |app| {
            app.action_new_window();
        });
        
        helpers::bind_simple_action(&app, &app.application, "about", |app| {
            app.action_about();
        });

        /* accelerators */
        app.application.set_accels_for_action("app.new_window", &["<Ctrl>N"]);
        app.application.set_accels_for_action("win.open", &["<Ctrl>O"]);
        app.application.set_accels_for_action("listing.export_ips", &["<Ctrl><Shift>E"]);
        app.application.set_accels_for_action("listing.goto", &["G"]);
        app.application.set_accels_for_action("listing.goto_start_of_line", &["<Ctrl>A"]);
        app.application.set_accels_for_action("listing.goto_end_of_line", &["<Ctrl>E"]);
        app.application.set_accels_for_action("listing.insert_break", &["B"]);
        app.application.set_accels_for_action("listing.mode::command", &["Escape", "Return"]);
        app.application.set_accels_for_action("listing.mode::entry", &["E"]);
        app.application.set_accels_for_action("listing.mode::utf8", &["T"]);
        app.application.set_accels_for_action("listing.insert_mode", &["Insert"]);

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
        window::CharmWindow::new(self)
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
    application.connect_startup(clone!(@strong app_model_for_closures => move |app| {
        // TODO: figure out gtk4 icon hellscape
        if app_model_for_closures.set(CharmApplication::new(app.clone())).is_err() {
            panic!("started up more than once?");
        }
    }));

    /* activate */
    application.connect_activate(clone!(@strong app_model_for_closures => move |_app| {
        app_model_for_closures.get().unwrap().action_new_window();
    }));

    /* open */
    application.connect_open(clone!(@strong app_model_for_closures => move |_app, files, _hint| {
        for file in files {
            let w = app_model_for_closures.get().unwrap().new_window();
            w.open_file(file);
            w.present();
        }
    }));
    
    application.run();
}
