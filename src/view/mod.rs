use gtk::gio;
use gtk::glib::clone;
use gtk::prelude::*;

pub mod action;
pub mod addr_entry;
pub mod config;
pub mod error;
pub mod ext;
pub mod export;
pub mod window;
pub mod helpers;
pub mod gsc;

pub mod breadcrumbs;
pub mod crashreport;
pub mod datapath;
pub mod hierarchy;
pub mod listing;
pub mod project;
pub mod props_editor;
pub mod selection;

use std::cell;
use std::rc;

use crate::catch_panic;

pub struct CharmApplication {
    pub application: adw::Application,
    rt: tokio::runtime::Runtime,

    persist_config_task: Option<helpers::AsyncSubscriber>,
    sync_dark_mode_task: helpers::AsyncSubscriber,
    about_dialog: gtk::AboutDialog,
    settings_dialog: gtk::Window,
    windows: cell::RefCell<Vec<rc::Weak<window::CharmWindow>>>,
}

impl CharmApplication {
    fn new(application: adw::Application) -> rc::Rc<CharmApplication> {
        addr_entry::AddrEntry::ensure_type();
        action::listing::clipboard::init();
        
        let app = rc::Rc::new(CharmApplication {
            rt: tokio::runtime::Builder::new_multi_thread()
                .enable_all()
                .build().unwrap(),

            persist_config_task: config::create_persist_config_task(),
            sync_dark_mode_task: config::create_sync_dark_mode_task(),
            about_dialog: Self::create_about_dialog(),
            settings_dialog: Self::create_settings_dialog(),
            windows: Default::default(),
            application,
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
        
        /* css */
        let display = gtk::gdk::Display::default().expect("there should be a GDK display");
        let css = gtk::CssProvider::new();
        css.load_from_data(include_str!("charm.css"));
        gtk::style_context_add_provider_for_display(&display, &css, gtk::STYLE_PROVIDER_PRIORITY_APPLICATION);

        /* icons */
        let icon_theme = gtk::IconTheme::for_display(&display);
        icon_theme.add_search_path("src/view/icons/");
        
        app
    }

    pub fn action_new_window(self: &rc::Rc<Self>) {
        let w = self.new_window(false);
        w.present();
    }
    
    pub fn action_about(self: &rc::Rc<Self>) {
        self.about_dialog.present();
    }

    pub fn new_window(self: &rc::Rc<Self>, listing_only: bool) -> rc::Rc<window::CharmWindow> {
        let window = window::CharmWindow::new(self, listing_only);
        self.windows.borrow_mut().push(rc::Rc::downgrade(&window));
        window
    }

    fn destroy_window(&self, window: &window::CharmWindow) {
        self.windows.borrow_mut().retain(|w| match w.upgrade() {
            Some(w) => w.id != window.id,
            None => false,
        });
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

    fn create_settings_dialog() -> gtk::Window {
        let builder = gtk::Builder::from_string(include_str!("settings.ui"));

        std::mem::forget(config::Config::bind(&builder, config::INSTANCE.clone()));
        
        let settings_dialog = gtk::Window::builder()
            .child(&builder.object::<gtk::Widget>("toplevel").unwrap())
            .title("Settings")
            .hide_on_close(true)
            .build();

        settings_dialog
    }
    
    pub fn launch<F: Fn(&rc::Rc<CharmApplication>) + 'static>(flags: gio::ApplicationFlags, activate: F) {
        /* we defer initializing CharmApplication until the startup signal */
        let app_model_for_closures: rc::Rc<once_cell::unsync::OnceCell<rc::Rc<CharmApplication>>> =
            rc::Rc::new(once_cell::unsync::OnceCell::new());

        let activate = rc::Rc::new(activate);
        
        let application =
            adw::Application::new(
                Some("net.xenotoad.charm"),
                flags);

        /* setup signals */
        
        /* startup */
        application.connect_startup(clone!(#[strong] app_model_for_closures, move |app| catch_panic! {        
            // TODO: figure out gtk4 icon hellscape
            let charm = CharmApplication::new(app.clone());
            
            crashreport::install_hook(charm.clone());

            if app_model_for_closures.set(charm).is_err() {
                panic!("started up more than once?");
            }
        }));

        /* activate */
        application.connect_activate(clone!(#[strong] app_model_for_closures, #[strong] activate, move |_app| catch_panic! {
            activate(app_model_for_closures.get().unwrap());
        }));

        /* open */
        application.connect_open(clone!(#[strong] app_model_for_closures, move |_app, files, _hint| catch_panic! {
            for file in files {
                let w = app_model_for_closures.get().unwrap().new_window(false);
                w.present();
                action::open_project::open_project(&w, file.clone());
            }
        }));

        application.run();   
    }
}
