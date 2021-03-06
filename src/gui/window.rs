use std::cell;
use std::rc;
use std::sync;

use crate::CharmApplication;
use crate::space;
use crate::space::AddressSpace;
use crate::listing;
use crate::widget;
use crate::gui;

use gtk::prelude::*;
use gio::prelude::*;

pub struct WindowContext {
    window: rc::Weak<CharmWindow>,
    listing_watch: sync::Arc<listing::ListingWatch>,
    lw: sync::Arc<parking_lot::RwLock<widget::listing::ListingWidget>>,
    patches: sync::Arc<parking_lot::RwLock<widget::patches::PatchesModel>>,
}

pub struct CharmWindow {
    pub application: rc::Rc<CharmApplication>,
    window: gtk::ApplicationWindow,
    listing_container: gtk::Container,
    patch_list: gtk::TreeView,
    config_editor: gtk::ListBox,
    patch_list_frame: gtk::Frame,
    config_editor_frame: gtk::Frame,
    context: cell::RefCell<Option<WindowContext>>,
}

impl CharmWindow {
    pub fn new(charm: &rc::Rc<CharmApplication>) -> rc::Rc<CharmWindow> {
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
                    edit_menu.append_section(Some("Edit Mode"), &mode_menu);
                    mode_menu.freeze();
                }
                edit_menu.freeze();
                menu_bar.append_submenu(Some("Edit"), &edit_menu);
            }
            {
                let view_menu = gio::Menu::new();
                view_menu.append(Some("Patch List"), Some("win.view.patch_list"));
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

            let menu_bar_widget = gtk::MenuBar::from_model(&menu_bar);
            main_box.pack_start(&menu_bar_widget, false, false, 0);
        }

        let listing_container = gtk::Box::new(gtk::Orientation::Vertical, 0);
        listing_container.set_homogeneous(true);
        
        let hpaned = gtk::Paned::new(gtk::Orientation::Horizontal);
        let patch_list = gtk::TreeView::new();
        {
            {
                let crt = gtk::CellRendererText::new();
                let c = gtk::TreeViewColumn::new();
                c.pack_start(&crt, true);
                c.add_attribute(&crt, "text", 0);
                c.set_title("Location");
                patch_list.append_column(&c);
            }
            {
                let crt = gtk::CellRendererText::new();
                let c = gtk::TreeViewColumn::new();
                c.pack_start(&crt, true);
                c.add_attribute(&crt, "text", 1);
                c.set_title("Size");
                patch_list.append_column(&c);
            }
            {
                let crt = gtk::CellRendererText::new();
                let c = gtk::TreeViewColumn::new();
                c.pack_start(&crt, true);
                c.add_attribute(&crt, "text", 2);
                c.set_title("Patched Bytes");
                patch_list.append_column(&c);
            }
        }

        let patch_list_frame = gtk::Frame::new(None);
        {
            let vpaned = gtk::Paned::new(gtk::Orientation::Vertical);
            vpaned.pack1(&listing_container, true, false);

            { // patch_list frame
                patch_list_frame.set_shadow_type(gtk::ShadowType::In);
                patch_list_frame.set_margin_top(10);
                patch_list_frame.set_margin_bottom(10);
                patch_list_frame.set_margin_start(10);
                patch_list_frame.set_margin_end(10);
                patch_list_frame.add(&patch_list);
                
                vpaned.pack2(&patch_list_frame, false, true);
            }
            
            hpaned.pack1(&vpaned, true, false);
        }

        let config_editor = widget::config_editor::build_config_editor();
        let config_editor_frame = gtk::Frame::new(None);
        {
            config_editor.set_size_request(400, -1);

            config_editor_frame.set_shadow_type(gtk::ShadowType::In);
            config_editor_frame.set_margin_top(10);
            config_editor_frame.set_margin_bottom(10);
            config_editor_frame.set_margin_start(10);
            config_editor_frame.set_margin_end(10);
            config_editor_frame.add(&config_editor);
            
            hpaned.pack2(&config_editor_frame, false, true);
        }
        
        main_box.pack_start(&hpaned, true, true, 0);                
        
        window.add(&main_box);

        let w = rc::Rc::new(CharmWindow {
            application: charm.clone(),
            window,
            listing_container: listing_container.upcast::<gtk::Container>(),
            patch_list,
            config_editor,
            patch_list_frame,
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

        w.window.connect_key_press_event(|w, ek| {
            /* key events before accelerators, because we have accelerators like "B" and "G" */
            gtk::Inhibit(w.propagate_key_event(ek) || w.activate_key(ek))
        });

        /* window actions */
        
        gui::helpers::bind_simple_action(&w, &w.window, "open", |w| {
            w.action_open();
        });

        gui::helpers::bind_stateful_action(&w, &w.window, "view.patch_list", true, |act, w, state| {
            if let Some(vis) = state {
                w.patch_list_frame.set_visible(vis);
                act.set_state(&glib::Variant::from(vis));
            }
        });
        
        gui::helpers::bind_stateful_action(&w, &w.window, "view.config_editor", false, |act, w, state| {
            if let Some(vis) = state {
                w.config_editor_frame.set_visible(vis);
                act.set_state(&glib::Variant::from(vis));
            }
        });

        w
    }

    pub fn show(&self) {
        self.window.show_all();
        self.config_editor_frame.hide();
    }

    fn action_open(self: &rc::Rc<Self>) {
        let dialog = gtk::FileChooserDialog::with_buttons::<gtk::ApplicationWindow>( // TODO: use FileChooserNative
            Some("Charm: Open File"),
            None,
            gtk::FileChooserAction::Open,
            &[
                ("_Cancel", gtk::ResponseType::Cancel),
                ("_Open", gtk::ResponseType::Accept)
            ]);
        dialog.set_select_multiple(true);
        match dialog.run() {
            gtk::ResponseType::Accept => {
                self.close_file();
                
                for file in dialog.get_files() {
                    if self.context.borrow().is_some() {
                        let w = self.application.new_window();
                        w.open_file(&file);
                        w.show();
                    } else {
                        self.open_file(&file);
                        self.show();
                    }
                }
            },
            _ => {} /* we were cancelled, ignore */
        }
        dialog.close();
    }

    pub fn close_file(self: &rc::Rc<Self>) {
        self.listing_container.foreach(|w| self.listing_container.remove(w));
        self.patch_list.set_model::<gtk::TreeModel>(None);
        self.window.insert_action_group::<gio::ActionGroup>("listing", None);
        *self.context.borrow_mut() = None;
    }
    
    pub fn open_file(self: &rc::Rc<Self>, file: &gio::File) {
        let attributes = file.query_info("standard::display-name", gio::FileQueryInfoFlags::NONE, Option::<&gio::Cancellable>::None).unwrap();
        let dn = attributes.get_attribute_as_string("standard::display-name").unwrap();

        let space = std::sync::Arc::new(
            space::file::FileAddressSpace::open(
                self.application.rt.handle().clone(),
                &file.get_path().unwrap(),
                &dn).unwrap(),
        );

        self.window.set_title(format!("Charm: {}", space.get_label()).as_str());
        WindowContext::new(self, listing::Listing::new(space)).attach(self);
    }
}

impl WindowContext {
    fn new(window: &rc::Rc<CharmWindow>, listing: listing::Listing) -> WindowContext {
        let listing_watch = sync::Arc::new(listing::ListingWatch::new(listing));
        let lw = widget::listing::ListingWidget::new(window, &listing_watch);
        let patches = widget::patches::PatchesModel::new(window, &listing_watch);
        
        WindowContext {
            window: rc::Rc::downgrade(window),
            listing_watch,
            lw,
            patches,
        }
    }

    fn attach(self, window: &CharmWindow) {
        let lw = self.lw.read();
        let da = lw.get_drawing_area();
        window.listing_container.foreach(|w| window.listing_container.remove(w));
        window.listing_container.add(da);
        window.patch_list.set_model(Some(self.patches.read().get_tree_model()));
        window.window.insert_action_group("listing", lw.get_action_group());
        da.grab_focus();

        std::mem::drop(lw);
        *window.context.borrow_mut() = Some(self);
    }
}
