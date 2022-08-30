use std::cell;
use std::rc;
use std::sync;

use crate::view::CharmApplication;
use crate::model::document;
use crate::model::space;
use crate::model::space::AddressSpace;
use crate::view;

use gtk::gio;
use gtk::prelude::*;

pub struct WindowContext {
    window: rc::Weak<CharmWindow>,
    document_host: sync::Arc<document::DocumentHost>,
    lw: view::listing::ListingWidget,
    datapath: sync::Arc<parking_lot::RwLock<view::datapath::DataPathModel>>,
}

pub struct CharmWindow {
    pub application: rc::Rc<CharmApplication>,
    window: gtk::ApplicationWindow,
    listing_container: gtk::Box,
    datapath_editor: gtk::TreeView,
    config_editor: gtk::ListBox,
    datapath_editor_frame: gtk::Frame,
    config_editor_frame: gtk::Frame,
    context: cell::RefCell<Option<WindowContext>>,
}

impl CharmWindow {
    pub fn new(charm: &rc::Rc<CharmApplication>) -> rc::Rc<CharmWindow> {
        let window = gtk::ApplicationWindow::new(&charm.application);

        window.set_title(Some("Charm"));
        window.set_default_size(800, 600);

        let main_box = gtk::Box::new(gtk::Orientation::Vertical, 0);
        
        {
            let menu_bar = gio::Menu::new();
            {
                let file_menu = gio::Menu::new();
                file_menu.append(Some("New Window"), Some("app.new_window"));
                file_menu.append(Some("Open File..."), Some("win.open"));
                file_menu.append(Some("Export patches (IPS)..."), Some("listing.export_ips"));
                file_menu.freeze();
                menu_bar.append_submenu(Some("File"), &file_menu);
            }
            {
                let edit_menu = gio::Menu::new();
                edit_menu.append(Some("Go to..."), Some("listing.goto"));
                edit_menu.append(Some("Insert or edit break..."), Some("listing.insert_break"));
                {
                    let mode_menu = gio::Menu::new();
                    mode_menu.append(Some("Command mode"), Some("listing.mode::command"));
                    mode_menu.append(Some("Hex entry"), Some("listing.mode::entry"));
                    mode_menu.append(Some("Text entry (UTF8)"), Some("listing.mode::utf8"));
                    mode_menu.append(Some("Insert"), Some("listing.insert_mode"));
                    edit_menu.append_section(Some("Edit Mode"), &mode_menu);
                    mode_menu.freeze();
                }
                edit_menu.freeze();
                menu_bar.append_submenu(Some("Edit"), &edit_menu);
            }
            {
                let view_menu = gio::Menu::new();
                view_menu.append(Some("Datapath Editor"), Some("win.view.datapath_editor"));
                view_menu.append(Some("Internal Configuration Editor"), Some("win.view.config_editor"));
                view_menu.freeze();
                menu_bar.append_submenu(Some("View"), &view_menu);
            }
            {
                let help_menu = gio::Menu::new();
                help_menu.append(Some("About"), Some("app.about"));
                help_menu.freeze();
                menu_bar.append_submenu(Some("Help"), &help_menu);
            }
            menu_bar.freeze();

            let menu_bar_widget = gtk::PopoverMenuBar::from_model(Some(&menu_bar));
            main_box.prepend(&menu_bar_widget);
        }

        let listing_container = gtk::Box::new(gtk::Orientation::Vertical, 0);
        listing_container.set_homogeneous(true);
        listing_container.set_vexpand(true);
        
        let hpaned = gtk::Paned::new(gtk::Orientation::Horizontal);
        let datapath_editor = gtk::TreeView::new();
        {
            {
                let crt = gtk::CellRendererText::new();
                let c = gtk::TreeViewColumn::new();
                c.pack_start(&crt, true);
                c.add_attribute(&crt, "text", 0);
                c.set_title("Type");
                datapath_editor.append_column(&c);
            }
            {
                let crt = gtk::CellRendererText::new();
                let c = gtk::TreeViewColumn::new();
                c.pack_start(&crt, true);
                c.add_attribute(&crt, "text", 1);
                c.set_title("Address");
                datapath_editor.append_column(&c);
            }
            {
                let crt = gtk::CellRendererText::new();
                let c = gtk::TreeViewColumn::new();
                c.pack_start(&crt, true);
                c.add_attribute(&crt, "text", 2);
                c.set_title("Size");
                datapath_editor.append_column(&c);
            }
            {
                let crt = gtk::CellRendererText::new();
                let c = gtk::TreeViewColumn::new();
                c.pack_start(&crt, true);
                c.add_attribute(&crt, "text", 3);
                c.set_title("Description");
                datapath_editor.append_column(&c);
            }
        }

        let datapath_editor_frame = gtk::Frame::new(None);
        {
            let vpaned = gtk::Paned::new(gtk::Orientation::Vertical);
            vpaned.set_resize_start_child(true);
            listing_container.set_vexpand(true);
            listing_container.set_valign(gtk::Align::Fill);
            vpaned.set_start_child(Some(&listing_container));

            { // datapath_editor frame
                // TODO: removed in gtk4, did we need this to look good?
                //datapath_editor_frame.set_shadow_type(gtk::ShadowType::In);
                datapath_editor_frame.set_margin_top(10);
                datapath_editor_frame.set_margin_bottom(10);
                datapath_editor_frame.set_margin_start(10);
                datapath_editor_frame.set_margin_end(10);
                datapath_editor_frame.set_child(Some(&datapath_editor));
                
                datapath_editor_frame.set_vexpand(false);
                vpaned.set_end_child(Some(&datapath_editor_frame));
            }

            /* When window is resized, resize the listing view. */
            vpaned.set_resize_start_child(true);
            vpaned.set_resize_end_child(false);

            /* Respect minimum sizes. */
            vpaned.set_shrink_start_child(false);
            vpaned.set_shrink_end_child(false);
            
            vpaned.set_hexpand(true);
            hpaned.set_start_child(Some(&vpaned));
        }

        let config_editor = view::config_editor::build_config_editor();
        let config_editor_frame = gtk::Frame::new(None);
        {
            config_editor.set_size_request(400, -1);

            // TODO: removed in gtk4, did we need this to look good?
            //config_editor_frame.set_shadow_type(gtk::ShadowType::In);
            config_editor_frame.set_margin_top(10);
            config_editor_frame.set_margin_bottom(10);
            config_editor_frame.set_margin_start(10);
            config_editor_frame.set_margin_end(10);
            config_editor_frame.set_child(Some(&config_editor));
            
            hpaned.set_end_child(Some(&config_editor_frame));
        }

        hpaned.set_resize_start_child(true);
        hpaned.set_resize_end_child(false);
        hpaned.set_shrink_start_child(false);
        hpaned.set_shrink_end_child(false);
        
        hpaned.set_vexpand(true);
        main_box.append(&hpaned);                
        
        window.set_child(Some(&main_box));

        let w = rc::Rc::new(CharmWindow {
            application: charm.clone(),
            window,
            listing_container,
            datapath_editor,
            config_editor,
            datapath_editor_frame,
            config_editor_frame,
            context: cell::RefCell::new(None),
        });

        {
            // extend our lifetime until the window closes
            let w_clone = Some(w.clone());
            w.window.connect_destroy(move |_| {
                let _ = &w_clone;
            });
        }

        // TODO: figure out how to do this in gtk4?
        /*
        w.window.connect_key_press_event(|w, ek| {
            /* key events before accelerators, because we have accelerators like "B" and "G" */
            gtk::Inhibit(w.propagate_key_event(ek) || w.activate_key(ek))
    });
        */

        /* window actions */
        
        view::helpers::bind_simple_action(&w, &w.window, "open", |w| {
            w.action_open();
        });

        view::helpers::bind_stateful_action(&w, &w.window, "view.datapath_editor", true, |act, w, state| {
            if let Some(vis) = state {
                w.datapath_editor_frame.set_visible(vis);
                act.set_state(&vis.to_variant());
            }
        });
        
        view::helpers::bind_stateful_action(&w, &w.window, "view.config_editor", false, |act, w, state| {
            if let Some(vis) = state {
                w.config_editor_frame.set_visible(vis);
                act.set_state(&vis.to_variant());
            }
        });

        w
    }

    pub fn present(&self) {
        self.config_editor_frame.hide();
        self.window.present();
    }

    fn action_open(self: &rc::Rc<Self>) {
        let dialog = gtk::FileChooserDialog::new( // TODO (written pre gtk4): use FileChooserNative
            Some("Charm: Open File"),
            Option::<&gtk::ApplicationWindow>::None, // TODO: set this?
            gtk::FileChooserAction::Open,
            &[
                ("_Cancel", gtk::ResponseType::Cancel),
                ("_Open", gtk::ResponseType::Accept)
            ]);
        dialog.set_select_multiple(true);

        let window = self.clone();
        dialog.connect_response(move |dialog, response_type| {
            match response_type {
                gtk::ResponseType::Accept => {
                    window.close_file();
                    
                    for file in dialog.files() {
                        if window.context.borrow().is_some() {
                            /* open a new window if this window already has something open in it */
                            let window = window.application.new_window();
                            window.open_file(&file.downcast().unwrap());
                            window.present();
                        } else {
                            window.open_file(&file.downcast().unwrap());
                            window.present();
                        }
                    }
                },
                _ => {} /* we were cancelled, ignore */
            }
            dialog.close();
        });
        dialog.present();
    }

    pub fn close_file(self: &rc::Rc<Self>) {
        while let Some(w) = self.listing_container.first_child() {
            self.listing_container.remove(&w);
        }
        
        self.datapath_editor.set_model(Option::<&gtk::TreeModel>::None);
        self.window.insert_action_group("listing", Option::<&gio::ActionGroup>::None);
        *self.context.borrow_mut() = None;
    }
    
    pub fn open_file(self: &rc::Rc<Self>, file: &gio::File) {
        let attributes = file.query_info("standard::display-name", gio::FileQueryInfoFlags::NONE, Option::<&gio::Cancellable>::None).unwrap();
        let dn = attributes.attribute_as_string("standard::display-name").unwrap();

        let space = std::sync::Arc::new(
            space::file::FileAddressSpace::open(
                self.application.rt.handle().clone(),
                &file.path().unwrap(),
                &dn).unwrap(),
        );

        self.window.set_title(Some(format!("Charm: {}", space.get_label()).as_str()));
        WindowContext::new(self, document::Document::new(space)).attach(self);
    }
}

impl WindowContext {
    fn new(window: &rc::Rc<CharmWindow>, document: document::Document) -> WindowContext {
        let document_host = sync::Arc::new(document::DocumentHost::new(document));
        let lw = view::listing::ListingWidget::new();
        lw.init(window, &document_host);
        let datapath = view::datapath::DataPathModel::new(window, &document_host);
        
        WindowContext {
            window: rc::Rc::downgrade(window),
            document_host,
            lw,
            datapath,
        }
    }

    fn attach(self, window: &CharmWindow) {
        while let Some(w) = window.listing_container.first_child() {
            window.listing_container.remove(&w);
        }
        window.listing_container.prepend(&self.lw);
        window.datapath_editor.set_model(Some(self.datapath.read().get_tree_model()));
        self.lw.grab_focus();
        
        *window.context.borrow_mut() = Some(self);
    }
}
