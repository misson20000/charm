#![allow(dead_code)]

extern crate cairo;
extern crate gio;
extern crate gtk;
extern crate gdk;
extern crate futures;
extern crate tokio;

#[cfg(feature = "test_listing")]
extern crate ncurses;

mod addr;
mod listing;
mod space;
mod widget;

use std::rc;
use std::sync;
use std::option;
use std::cell;

use gio::prelude::*;
use gtk::prelude::*;

struct CharmUi {
    window: gtk::ApplicationWindow
}

struct CharmApplication {
    rt: tokio::runtime::Runtime,
    ui: option::Option<CharmUi>,
}

impl CharmApplication {
    fn build_ui_if_necessary(
        &mut self,
        application: &gtk::Application) {

        match self.ui {
            Some(_) => (),
            None => {
                let window = gtk::ApplicationWindow::new(application);

                window.set_title("Charm");
                window.set_border_width(10);
                window.set_position(gtk::WindowPosition::Center);
                window.set_default_size(800, 600);

                let pane = gtk::Paned::new(gtk::Orientation::Horizontal);
                let tree = gtk::TreeView::new();
                
                /*
                let da = gtk::DrawingArea::new();
                widget::listing::ListingWidget::new(aspace, rt)
                .attach(&da);
                
                pane.add(&da);
                 */
                
                pane.add(&tree);

                window.add(&pane);

                window.show_all();

                self.ui = Some(CharmUi {
                    window
                });
            }
        }
    }

    fn open(&mut self, files: &[gio::File], hint: &str) {
        println!("charm application opening {:?} hint {}", files, hint);

        /*let fas = std::sync::Arc::new(
        space::file::FileAddressSpace::open(
        rt.handle().clone(),
        "/proc/self/exe").unwrap());*/
    }
}

fn main() {
    println!("Hello, world!");

    // we defer initializing CharmApplication until the startup signal
    let app_model_for_closures: rc::Rc<cell::RefCell<option::Option<
            rc::Rc<cell::RefCell<CharmApplication>>>>> =
        rc::Rc::new(
            cell::RefCell::new(
                None));
    
    let application =
        gtk::Application::new(
            Some("net.xenotoad.charm"),
            gio::ApplicationFlags::HANDLES_OPEN)
        .expect("Initialization failed..");

    /* setup signals */

    /* startup */
    { let app_model_clone = app_model_for_closures.clone();
      application.connect_startup(move |_app| {
          std::mem::replace(
              &mut *app_model_clone.borrow_mut(),
              Some(rc::Rc::new(
                  cell::RefCell::new(CharmApplication {
                      rt: tokio::runtime::Builder::new()
                          .threaded_scheduler()
                          .enable_all()
                          .build().unwrap(),
                      ui: None
                  }))));
      });
    }

    /* shutdown */
    { let app_model_clone = app_model_for_closures.clone();
      application.connect_shutdown(move |_app| {
          std::mem::replace(
              &mut *app_model_clone.borrow_mut(),
              None);
      });
    }

    /* activate */
    { let app_model_clone = app_model_for_closures.clone();
      application.connect_activate(move |app| {
          let app_model_ptr = app_model_clone.borrow_mut();
          app_model_ptr.as_ref().unwrap().borrow_mut().build_ui_if_necessary(app);
      });
    }

    /* open */
    { let app_model_clone = app_model_for_closures.clone();
      application.connect_open(move |app, files, hint| {
          let app_model_ptr_ptr = app_model_clone.borrow_mut();
          let mut app_model_ptr = app_model_ptr_ptr.as_ref().unwrap().borrow_mut();
          app_model_ptr.build_ui_if_necessary(app);
          app_model_ptr.open(files, hint);
      });
    }

    application.run(&std::env::args().collect::<Vec<_>>());
}
