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
    window: gtk::ApplicationWindow,
    notebook: gtk::Notebook,
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

                let main_box = gtk::Box::new(gtk::Orientation::Vertical, 24);

                {
                    let menu_bar = gtk::MenuBar::new();
                    {
                        let file_menu = gtk::Menu::new();
                        let item1 = gtk::MenuItem::new_with_label("File Item #1");
                        let item2 = gtk::MenuItem::new_with_label("File Item #2");
                        file_menu.append(&item1);
                        file_menu.append(&item2);
                        
                        let file_item = gtk::MenuItem::new_with_label("File");
                        file_item.set_submenu(Some(&file_menu));
                        menu_bar.append(&file_item);
                    }
                    {
                        let help_menu = gtk::Menu::new();
                        let item1 = gtk::MenuItem::new_with_label("Help Item #1");
                        let item2 = gtk::MenuItem::new_with_label("Help Item #2");
                        help_menu.append(&item1);
                        help_menu.append(&item2);

                        let help_item = gtk::MenuItem::new_with_label("Help");
                        help_item.set_submenu(Some(&help_menu));
                        menu_bar.append(&help_item);
                    }
                    main_box.pack_start(&menu_bar, false, false, 0);
                }

                let notebook = gtk::Notebook::new();
                main_box.pack_start(&notebook, true, true, 0);                
                
                window.add(&main_box);
                window.show_all();

                self.ui = Some(CharmUi {
                    window,
                    notebook
                });
            }
        }
    }

    fn append_page_for_file(&self, file: &gio::File) {
        let pane = gtk::Paned::new(gtk::Orientation::Horizontal);
        let tree = gtk::TreeView::new();

        let fas = std::sync::Arc::new(
            space::file::FileAddressSpace::open(
                self.rt.handle().clone(),
                &file.get_path().unwrap()).unwrap());
        
        let da = gtk::DrawingArea::new();
        widget::listing::ListingWidget::new(fas, self.rt.handle().clone()).attach(&da);
        
        pane.add(&da);

        let attributes = file.query_info("standard::display-name", gio::FileQueryInfoFlags::NONE, option::Option::<&gio::Cancellable>::None).unwrap();
        let dn = attributes.get_attribute_as_string("standard::display-name").unwrap();
        println!("file display name: {}", dn.as_str());
        
        pane.add(&tree);
        let ui = self.ui.as_ref().unwrap();
        ui.notebook.append_page(&pane, Some(&gtk::Label::new(Some(dn.as_str()))));
        ui.notebook.show_all();
    }

    fn open(&mut self, files: &[gio::File], hint: &str) {
        println!("charm application opening {:?} hint {}", files, hint);

        for f in files {
            self.append_page_for_file(&f);
        }
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
