#![allow(dead_code)]

extern crate glib;
extern crate cairo;
extern crate gio;
extern crate gtk;
extern crate gdk;
extern crate gdk_pixbuf;
extern crate futures;
extern crate tokio;
extern crate send_wrapper;
extern crate owning_ref;
extern crate enum_dispatch;

mod ext;
mod util;
mod config;

mod model;
mod view;

use std::rc;
use std::option;
use std::cell;

use gio::prelude::*;
use gdk_pixbuf::prelude::*;
use gtk::prelude::*;

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

        view::helpers::bind_simple_action(&app, &app.application, "new_window", |app| {
            app.action_new_window();
        });
        
        view::helpers::bind_simple_action(&app, &app.application, "about", |app| {
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
        w.show();
    }
    
    fn action_about(self: &rc::Rc<Self>) {
        self.about_dialog.run();
        self.about_dialog.hide();
    }

    fn new_window(self: &rc::Rc<Self>) -> rc::Rc<view::window::CharmWindow> {
        view::window::CharmWindow::new(self)
    }
    
    fn create_about_dialog() -> gtk::AboutDialog {
        let about_dialog = gtk::AboutDialog::new();

        about_dialog.set_authors(&["misson20000 <xenotoad@xenotoad.net>"]);
        about_dialog.set_comments(Some("A listing editor that tries to be at least not bad."));
        about_dialog.set_copyright(Some("Copyright 2020 misson20000"));
        about_dialog.set_license_type(gtk::License::Gpl20Only);
        about_dialog.set_program_name("Charm");
        about_dialog.set_version(Some("0.1.0"));
        about_dialog.set_website(Some("https://charm.xenotoad.net"));

        about_dialog
    }
}

fn main() {
    /* we defer initializing CharmApplication until the startup signal */
    let app_model_for_closures: rc::Rc<cell::RefCell<option::Option<
            rc::Rc<CharmApplication>>>> =
        rc::Rc::new(
            cell::RefCell::new(
                None));
    
    let application =
        gtk::Application::new(
            Some("net.xenotoad.charm"),
            gio::ApplicationFlags::HANDLES_OPEN);

    /* setup signals */

    /* startup */
    { let app_model_clone = app_model_for_closures.clone();
      application.connect_startup(move |app| {
          /* set default icon */
          let charm_icon = include_bytes!("charm.png");
          let pixbuf_loader = gdk_pixbuf::PixbufLoader::new();
          if let Some(pixbuf) = pixbuf_loader.write(charm_icon).ok().and_then(|_| {
              pixbuf_loader.close().ok()
          }).and_then(|_| {
              pixbuf_loader.pixbuf()
          }) {
              gtk::Window::set_default_icon(&pixbuf);
          }
          
          *app_model_clone.borrow_mut() = Some(CharmApplication::new(app.clone()));
      });
    }

    /* shutdown */
    { let app_model_clone = app_model_for_closures.clone();
      application.connect_shutdown(move |_app| {
          *app_model_clone.borrow_mut() = None;
      });
    }

    /* activate */
    { let app_model_clone = app_model_for_closures.clone();
      application.connect_activate(move |_app| {
          let app_model_ptr_ptr = app_model_clone.borrow_mut();
          let app_model_ptr = app_model_ptr_ptr.as_ref().unwrap();

          app_model_ptr.action_new_window();
      });
    }

    /* open */
    { let app_model_clone = app_model_for_closures.clone();
      application.connect_open(move |_app, files, _hint| {
          let app_model_ptr_ptr = app_model_clone.borrow_mut();
          let app_model_ptr = app_model_ptr_ptr.as_ref().unwrap();

          for file in files {
              let w = app_model_ptr.new_window();
              w.open_file(file);
              w.show();
          }
      });
    }
    
    application.run();
}
