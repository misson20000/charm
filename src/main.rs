#![feature(arbitrary_self_types)]
#![allow(dead_code)]

extern crate glib;
extern crate cairo;
extern crate gio;
extern crate gtk;
extern crate gdk;
extern crate futures;
extern crate tokio;
extern crate send_wrapper;
extern crate owning_ref;
extern crate enum_dispatch;

#[cfg(feature = "test_listing")]
extern crate ncurses;

mod util;
mod ext;
mod addr;
mod listing;
mod space;
mod widget;
mod config;

use std::rc;
use std::sync;
use std::option;
use std::cell;

use gio::prelude::*;
use gtk::prelude::*;

struct CharmWindow {
    application: rc::Rc<CharmApplication>,
    window: gtk::ApplicationWindow,
    notebook: gtk::Notebook,
}

impl CharmWindow {
    fn new(charm: rc::Rc<CharmApplication>) -> rc::Rc<CharmWindow> {
        let window = gtk::ApplicationWindow::new(&charm.application);

        window.set_title("Charm");
        window.set_border_width(0);
        window.set_position(gtk::WindowPosition::Center);
        window.set_default_size(800, 600);

        let main_box = gtk::Box::new(gtk::Orientation::Vertical, 0);

        {
            let menu_bar = gio::Menu::new();
            {
                let file_menu = gio::Menu::new();
                file_menu.append(Some("New Window"), Some("app.new_window"));
                file_menu.append(Some("Open File..."), Some("win.open"));
                file_menu.freeze();
                menu_bar.append_submenu(Some("File"), &file_menu);
            }
            {
                let edit_menu = gio::Menu::new();
                edit_menu.append(Some("Go to..."), Some("win-listing.goto"));
                edit_menu.append(Some("Insert or edit break..."), Some("win-listing.insert_break"));
                edit_menu.freeze();
                menu_bar.append_submenu(Some("Edit"), &edit_menu);
            }
            {
                let help_menu = gio::Menu::new();
                help_menu.append(Some("About"), Some("app.about"));
                help_menu.freeze();
                menu_bar.append_submenu(Some("Help"), &help_menu);
            }
            menu_bar.freeze();
            
            let menu_bar_widget = gtk::MenuBar::new_from_model(&menu_bar);
            main_box.pack_start(&menu_bar_widget, false, false, 0);
        }

        let paned = gtk::Paned::new(gtk::Orientation::Horizontal);
        
        let notebook = gtk::Notebook::new();
        notebook.popup_enable();
        notebook.set_group_name(Some("listing"));
        notebook.set_scrollable(true);
        notebook.set_show_border(true);
        {
            let charm_clone = charm.clone();
            notebook.connect_create_window(move |_nb, _page, _x, _y| -> gtk::Notebook {
                let w = CharmWindow::new(charm_clone.clone());
                w.show();
                w.notebook.clone()
            });
        }
        paned.pack1(&notebook, true, false);

        {
            let editor = widget::config_editor::build_config_editor();
            editor.set_size_request(400, -1);
            
            let frame = gtk::Frame::new(None);
            frame.set_shadow_type(gtk::ShadowType::In);
            frame.set_margin_top(10);
            frame.set_margin_bottom(10);
            frame.set_margin_start(10);
            frame.set_margin_end(10);
            frame.add(&editor);
            
            paned.pack2(&frame, false, true);
        }

        main_box.pack_start(&paned, true, true, 0);                
        
        window.add(&main_box);

        let w = rc::Rc::new(CharmWindow {
            application: charm,
            window,
            notebook
        });

        {
            let open_action = gio::SimpleAction::new("open", None);
            let win_clone_for_closure = w.clone();
            open_action.set_enabled(true);
            open_action.connect_activate(move |_act, _par| {
                let dialog = gtk::FileChooserDialog::with_buttons( // TODO: use FileChooserNative
                    Some("Charm: Open File"),
                    Some(&win_clone_for_closure.window),
                    gtk::FileChooserAction::Open,
                    &[
                        ("_Cancel", gtk::ResponseType::Cancel),
                        ("_Open", gtk::ResponseType::Accept)
                    ]);
                dialog.set_select_multiple(true);
                match dialog.run() {
                    gtk::ResponseType::Accept => {
                        for file in dialog.get_files() {
                            win_clone_for_closure.open_file(&file);
                        }
                    },
                    _ => {} /* we were cancelled, ignore */
                }
                dialog.destroy();
            });
            w.window.add_action(&open_action);
        }

        util::ActionForwarder::new(&w.window, "listing".to_string());
        
        w
    }

    fn show(&self) {
        self.window.show_all();
    }

    fn open_file(&self, file: &gio::File) {
        let attributes = file.query_info("standard::display-name", gio::FileQueryInfoFlags::NONE, option::Option::<&gio::Cancellable>::None).unwrap();
        let dn = attributes.get_attribute_as_string("standard::display-name").unwrap();

        let fas = std::sync::Arc::new(
            space::file::FileAddressSpace::open(
                self.application.rt.handle().clone(),
                &file.get_path().unwrap(),
                &dn).unwrap(),
        );

        self.append_page_for_space(fas);
    }
    
    fn append_page_for_space(&self, space: sync::Arc<dyn space::AddressSpace + Send + Sync>) {
        let label = space.get_label().to_string();
        let listing = sync::Arc::new(listing::Listing::new(space));
        
        let da = gtk::DrawingArea::new();
        widget::listing::ListingWidget::new(listing, self.application.rt.handle().clone()).attach(&da);
        
        let idx = self.notebook.append_page(&da, Some(&gtk::Label::new(Some(&label))));
        self.notebook.set_tab_detachable(&da, true);
        self.notebook.set_tab_reorderable(&da, true);
        self.notebook.show_all();

        /* it is nice to focus newly opened files for the user */
        self.notebook.set_current_page(Some(idx));
        da.grab_focus();
    }
}

struct CharmApplication {
    application: gtk::Application,
    main_window: cell::RefCell<option::Option<rc::Rc<CharmWindow>>>,
    rt: tokio::runtime::Runtime,
}

impl CharmApplication {
    fn new(application: gtk::Application) -> rc::Rc<CharmApplication> {
        let app = rc::Rc::new(CharmApplication {
            application,
            main_window: cell::RefCell::new(None),
            rt: tokio::runtime::Builder::new()
                .threaded_scheduler()
                .enable_all()
                .build().unwrap(),
        });

        /* application actions */

        /* new_window */
        {
            let new_window_action = gio::SimpleAction::new("new_window", None);
            let app_clone_for_closure = app.clone();
            new_window_action.set_enabled(true);
            new_window_action.connect_activate(move |_act, _par| {
                let w = CharmWindow::new(app_clone_for_closure.clone());
                w.show();
            });
            app.application.add_action(&new_window_action);
        }

        /* about */
        {
            let about_action = gio::SimpleAction::new("about", None);
            let about_dialog = gtk::AboutDialog::new();

            about_dialog.set_authors(&["misson20000 <xenotoad@xenotoad.net>"]);
            about_dialog.set_comments(Some("Charm is a hex, memory, and structure editor designed to be flexible and robust."));
            about_dialog.set_copyright(Some("Copyright 2020 misson20000"));
            about_dialog.set_license_type(gtk::License::Gpl20Only);
            about_dialog.set_program_name("Charm");
            about_dialog.set_version(Some("0.1.0"));
            about_dialog.set_website(Some("https://charm.xenotoad.net"));
            
            about_action.set_enabled(true);
            about_action.connect_activate(move |_act, _par| {
                about_dialog.run();
                about_dialog.hide();
            });
            app.application.add_action(&about_action);
        }

        /* accelerators */
        app.application.set_accels_for_action("app.new_window", &["<Ctrl>N"]);
        app.application.set_accels_for_action("win.open", &["<Ctrl>O"]);
        app.application.set_accels_for_action("listing.goto", &["G"]);
        app.application.set_accels_for_action("listing.goto_start_of_line", &["<Ctrl>A"]);
        app.application.set_accels_for_action("listing.goto_end_of_line", &["<Ctrl>E"]);
        app.application.set_accels_for_action("listing.insert_break", &["B"]);

        app
    }
    
    fn ensure_main_window_exists(
        self: &rc::Rc<Self>) {
        let mut mw = self.main_window.borrow_mut();
        match *mw {
            Some(_) => (),
            None => {
                *mw = Some(CharmWindow::new(self.clone()));
            }
        }
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
            gio::ApplicationFlags::HANDLES_OPEN)
        .expect("Initialization failed..");

    /* setup signals */

    /* startup */
    { let app_model_clone = app_model_for_closures.clone();
      application.connect_startup(move |app| {
          std::mem::replace(
              &mut *app_model_clone.borrow_mut(),
              Some(CharmApplication::new(app.clone())));
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
      application.connect_activate(move |_app| {
          let app_model_ptr_ptr = app_model_clone.borrow_mut();
          let app_model_ptr = app_model_ptr_ptr.as_ref().unwrap();

          app_model_ptr.ensure_main_window_exists();
          app_model_ptr.main_window.borrow().as_ref().unwrap().show();
      });
    }

    /* open */
    { let app_model_clone = app_model_for_closures.clone();
      application.connect_open(move |_app, files, _hint| {
          let app_model_ptr_ptr = app_model_clone.borrow_mut();
          let app_model_ptr = app_model_ptr_ptr.as_ref().unwrap();
          
          app_model_ptr.ensure_main_window_exists();

          let wr = app_model_ptr.main_window.borrow();
          let w = wr.as_ref().unwrap();

          for file in files {
              w.open_file(file);
          }

          w.show();
      });
    }
    
    application.run(&std::env::args().collect::<Vec<_>>());
}
