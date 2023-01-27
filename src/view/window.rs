use std::cell;
use std::rc;
use std::sync;

use crate::view::CharmApplication;
use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::space;
use crate::view;
use crate::view::hierarchy;

use gtk::gio;
use gtk::glib;
use gtk::glib::clone;
use gtk::prelude::*;
use gtk::subclass::prelude::*;

pub struct WindowContext {
    window: rc::Weak<CharmWindow>,
    document_host: sync::Arc<document::DocumentHost>,
    lw: view::listing::ListingWidget,
    datapath_model: sync::Arc<parking_lot::RwLock<view::datapath::DataPathModel>>,
}

pub struct CharmWindow {
    pub application: rc::Rc<CharmApplication>,
    window: gtk::ApplicationWindow,
    listing_frame: gtk::Frame,
    datapath_editor: gtk::TreeView,
    hierarchy_editor: gtk::ColumnView,
    config_editor: gtk::ListBox,
    datapath_editor_frame: gtk::Frame,
    config_editor_frame: gtk::Frame,
    context: cell::RefCell<Option<WindowContext>>,
}

impl CharmWindow {
    pub fn new(charm: &rc::Rc<CharmApplication>) -> rc::Rc<CharmWindow> {
        let builder = gtk::Builder::from_string(include_str!("charm.ui"));

        let window: gtk::ApplicationWindow = builder.object("toplevel").unwrap();
        window.set_application(Some(&charm.application));

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
                edit_menu.append(Some("Nest"), Some("win.hierarchy.structure.nest"));
                edit_menu.append(Some("Edit structure node properties (TEMPORARY)..."), Some("win.edit_properties"));
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
                let struct_menu = gio::Menu::new();
                struct_menu.append(Some("Insert empty node"), Some("win.listing.structure.insert_empty"));
                struct_menu.freeze();
                menu_bar.append_submenu(Some("Structure"), &struct_menu);
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

            let menu_bar_widget: gtk::PopoverMenuBar = builder.object("menu_bar").unwrap();
            menu_bar_widget.set_menu_model(Some(&menu_bar));
        }

        let datapath_editor: gtk::TreeView = builder.object("datapath_editor").unwrap();
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

        let hierarchy_editor: gtk::ColumnView = builder.object("hierarchy_editor").unwrap();
        {
            for (title, ui) in [
                ("Name",    &include_bytes!("column_name.ui")[..]),
                ("Address", &include_bytes!("column_addr.ui")[..]),
                ("Size",    &include_bytes!("column_size.ui")[..])] {
                hierarchy_editor.append_column(
                    &gtk::ColumnViewColumn::builder()
                        .expand(true)
                        .resizable(true)
                        .title(title)
                        .factory(&gtk::BuilderListItemFactory::from_bytes(gtk::BuilderScope::NONE, &glib::Bytes::from_static(ui)))
                        .build());
            }
        }
        
        let config_editor = view::config_editor::build_config_editor();
        let config_editor_frame: gtk::Frame = builder.object("config_editor_frame").unwrap();
        config_editor_frame.set_child(Some(&config_editor));

        let w = rc::Rc::new(CharmWindow {
            application: charm.clone(),
            window,
            listing_frame: builder.object("listing_frame").unwrap(),
            datapath_editor,
            hierarchy_editor,
            config_editor,
            datapath_editor_frame: builder.object("datapath_editor_frame").unwrap(),
            config_editor_frame,
            context: cell::RefCell::new(None),
        });

        // extend our lifetime until the window closes
        w.window.connect_destroy(clone!(@strong w => move |_| {
            let _: &rc::Rc<CharmWindow> = &w;
        }));

        // TODO: figure out how to do this in gtk4?
        /*
        w.window.connect_key_press_event(|w, ek| {
            /* key events before accelerators, because we have accelerators like "B" and "G" */
            gtk::Inhibit(w.propagate_key_event(ek) || w.activate_key(ek))
    });
        */

        /* window actions */
        
        view::helpers::bind_simple_action(&w, &w.window, "open", |w| w.action_open());
        view::helpers::bind_simple_action(&w, &w.window, "edit_properties", |w| w.action_edit_properties());

        w.bind_listing_action("listing.structure.insert", &view::listing::action::action_insert_with_prompt);
        w.bind_listing_action("listing.structure.insert_byte", &view::listing::action::action_insert_byte);
        w.bind_listing_action("listing.structure.insert_word", &view::listing::action::action_insert_word);
        w.bind_listing_action("listing.structure.insert_dword", &view::listing::action::action_insert_dword);
        w.bind_listing_action("listing.structure.insert_qword", &view::listing::action::action_insert_qword);

        view::helpers::bind_simple_action(&w, &w.window, "hierarchy.structure.nest", |w| {
            w.action_nest();
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

    fn bind_listing_action<F: Fn(&view::listing::ListingWidget) + 'static>(self: &rc::Rc<Self>, id: &str, cb: F) {
        view::helpers::bind_simple_action(self, &self.window, id, move |w| {
            if let Some(ctx) = &*w.context.borrow() {
                cb(&ctx.lw)
            }
        });
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

        dialog.connect_response(clone!(@strong self as window => move |dialog, response_type| {
            match response_type {
                gtk::ResponseType::Accept => {
                    window.close_file();
                    
                    for file in &dialog.files() {
                        let file = file.expect("list model should not be modified during iteration");
                        
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
        }));
        dialog.present();
    }

    pub fn action_edit_properties(self: &rc::Rc<Self>) {
        if let Some(item) = self.hierarchy_editor.model().and_then(|model| {
            let selection = model.selection();
            if selection.size() == 0 {
                None
            } else {
                model.item(selection.nth(0))
            }
        }).map(|object| {
            object.downcast::<gtk::TreeListRow>().unwrap().item().unwrap().downcast::<hierarchy::NodeItem>().unwrap()
        }) {
            let node_info = item.imp().info.get().unwrap().borrow();

            let builder = gtk::Builder::from_string(include_str!("display-editor.ui"));

            let name_entry: gtk::Entry = builder.object("name_entry").unwrap();
            let children_display: gtk::Entry = builder.object("children_display").unwrap();
            
            let dialog = gtk::Dialog::builder()
                .application(&self.application.application)
                .child(&builder.object::<gtk::Widget>("toplevel").unwrap())
                .resizable(true)
                .title(&format!("Editing properties for '{}'", node_info.props.name))
                .transient_for(&self.window)
                .build();

            std::mem::drop(node_info);
            
            let name_binding = item.bind_property("name", &name_entry.buffer(), "text").flags(glib::BindingFlags::BIDIRECTIONAL | glib::BindingFlags::SYNC_CREATE).build();
            let children_display_binding = item.bind_property("children-display", &children_display.buffer(), "text").flags(glib::BindingFlags::BIDIRECTIONAL | glib::BindingFlags::SYNC_CREATE).build();
            
            dialog.show();

            /* extend lifetimes of binding and reference until window closes */
            dialog.connect_destroy(move |_| {
                let _ = item;
                let _ = name_binding;
                let _ = children_display_binding;
            });
        }
    }

    pub fn action_nest(self: &rc::Rc<Self>) {
        // TODO: disable action when no model is attached

        if let Some(ctx) = &*self.context.borrow() {
            let (selection, document) = match self.hierarchy_editor.model() {
                Some(model) => model.downcast::<hierarchy::StructureSelectionModel>().unwrap(),
                None => return
            }.selection_mode();

            let (parent, first_sibling, last_sibling) = match &selection {
                hierarchy::SelectionMode::Empty => return,
                hierarchy::SelectionMode::Single(path) if !path.is_empty() => (&path[0..path.len()-1], *path.last().unwrap(), *path.last().unwrap()),
                hierarchy::SelectionMode::SiblingRange(path, begin, end) => (&path[..], *begin, *end),
                hierarchy::SelectionMode::All | hierarchy::SelectionMode::Single(_) => {
                    // TODO: find a way to issue a warning for this
                    return;
                }
            };

            // TODO: failure feedback
            let _ = ctx.document_host.nest(&document, parent.to_vec(), first_sibling, last_sibling, structure::Properties {
                name: "nest".to_string(),
                title_display: structure::TitleDisplay::Minor,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::Hexdump(addr::Size::from(16)),
                locked: false,
            });
        }
    }
    
    pub fn close_file(self: &rc::Rc<Self>) {
        self.listing_frame.set_child(gtk::Widget::NONE);
        self.datapath_editor.set_model(Option::<&gtk::TreeModel>::None);
        self.hierarchy_editor.set_model(Option::<&gtk::SelectionModel>::None);
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

        self.window.set_title(Some(format!("Charm: {}", dn).as_str()));
        // TODO: error handling
        WindowContext::new(self, document::Document::new(space)).attach(self);
    }
}

impl WindowContext {
    fn new(window: &rc::Rc<CharmWindow>, document: document::Document) -> WindowContext {
        let document_host = sync::Arc::new(document::DocumentHost::new(document));
        let lw = view::listing::ListingWidget::new();
        lw.init(window, document_host.clone());
        let datapath_model = view::datapath::DataPathModel::new(window, &document_host);
        
        WindowContext {
            window: rc::Rc::downgrade(window),
            document_host,
            lw,
            datapath_model,
        }
    }

    fn attach(self, window: &CharmWindow) {
        window.listing_frame.set_child(Some(&self.lw));
        window.datapath_editor.set_model(Some(self.datapath_model.read().get_tree_model()));
        window.hierarchy_editor.set_model(Some(&view::hierarchy::StructureSelectionModel::new(self.document_host.clone())));
        self.lw.grab_focus();
        
        *window.context.borrow_mut() = Some(self);
    }
}
