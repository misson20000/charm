use std::cell;
use std::rc;
use std::sync;

use crate::view::CharmApplication;
use crate::model::document;
use crate::model::space;
use crate::view;
use crate::view::action;
use crate::view::helpers;
use crate::view::hierarchy;
use crate::view::props_editor;

use gtk::gio;
use gtk::glib;
use gtk::glib::clone;
use gtk::prelude::*;

pub struct CharmWindow {
    pub application: rc::Rc<CharmApplication>,
    pub window: gtk::ApplicationWindow,
    listing_frame: gtk::Frame,
    datapath_editor: gtk::TreeView,
    hierarchy_editor: gtk::ColumnView,
    config_editor: gtk::ListBox,
    datapath_editor_frame: gtk::Frame,
    config_editor_frame: gtk::Frame,
    props_editor: rc::Rc<props_editor::PropsEditor>,
    context: cell::RefCell<Option<WindowContext>>,
}

pub struct WindowContext {
    pub document_host: sync::Arc<document::DocumentHost>,
    
    pub window: rc::Weak<CharmWindow>,
    
    /* Widgets and models */
    pub lw: view::listing::ListingWidget,
    pub datapath_model: gtk::TreeModel,
    pub hierarchy_model: hierarchy::StructureSelectionModel,

    action_group: gio::SimpleActionGroup,
    
    /* Misc. subscribers and such that need to be kept around */
    datapath_subscriber: helpers::AsyncSubscriber,
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
                edit_menu.append(Some("Navigate..."), Some("ctx.navigate"));
                edit_menu.append(Some("Nest"), Some("ctx.nest"));
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
                struct_menu.append(Some("Insert byte at cursor"), Some("ctx.insert_byte"));
                struct_menu.append(Some("Insert word at cursor"), Some("ctx.insert_word"));
                struct_menu.append(Some("Insert dword at cursor"), Some("ctx.insert_dword"));
                struct_menu.append(Some("Insert qword at cursor"), Some("ctx.insert_qword"));
                struct_menu.append(Some("Insert node..."), Some("ctx.insert_node"));
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
            {
                let debug_menu = gio::Menu::new();
                debug_menu.append(Some("Reset this window's UI by reopening current document state"), Some("ctx.debug.reopen_current_document"));
                debug_menu.freeze();
                menu_bar.append_submenu(Some("Debug"), &debug_menu);
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

            let menu = gio::Menu::new();
            menu.append(Some("Nest"), Some("ctx.nest"));
            menu.append(Some("Delete"), Some("ctx.delete_node"));
            let popover = gtk::PopoverMenu::from_model(Some(&menu));
            popover.set_parent(&hierarchy_editor);
            
            let gesture = gtk::GestureClick::new();
            gesture.connect_pressed(move |gesture, n_press, x, y| {
                let seq = gesture.current_sequence();
                let event = gesture.last_event(seq.as_ref()).unwrap();

                if n_press != 1 {
                    return;
                }

                if !event.triggers_context_menu() {
                    return;
                }

                gesture.set_state(gtk::EventSequenceState::Claimed);

                popover.set_pointing_to(Some(&gtk::gdk::Rectangle::new(x as i32, y as i32, 1, 1)));
                popover.popup();
            });
            gesture.set_exclusive(true);
            gesture.set_button(0);
            hierarchy_editor.add_controller(&gesture);
        }
        
        let config_editor = view::config_editor::build_config_editor();
        let config_editor_frame: gtk::Frame = builder.object("config_editor_frame").unwrap();
        config_editor_frame.set_child(Some(&config_editor));

        let props_editor_frame: gtk::Frame = builder.object("props_editor_frame").unwrap();
        let props_editor = props_editor::PropsEditor::new();
        props_editor_frame.set_child(Some(props_editor.toplevel()));
        
        let w = rc::Rc::new(CharmWindow {
            application: charm.clone(),
            window,
            listing_frame: builder.object("listing_frame").unwrap(),
            datapath_editor,
            hierarchy_editor,
            config_editor,
            datapath_editor_frame: builder.object("datapath_editor_frame").unwrap(),
            config_editor_frame,
            props_editor,
            context: cell::RefCell::new(None),
        });

        w.window.connect_close_request(clone!(@strong w => move |_| {
            /* This is especially important because it destroys actions which might have their own toplevel windows that
             * would otherwise keep the process alive. */
            w.close_file();

            gtk::Inhibit(false)
        }));

        /* window actions */

        helpers::bind_simple_action(&w, &w.window, "open", |w| w.action_open());
        helpers::bind_stateful_action(&w, &w.window, "view.datapath_editor", true, |act, w, state| {
            if let Some(vis) = state {
                w.datapath_editor_frame.set_visible(vis);
                act.set_state(&vis.to_variant());
            }
        });
        
        helpers::bind_stateful_action(&w, &w.window, "view.config_editor", false, |act, w, state| {
            if let Some(vis) = state {
                w.config_editor_frame.set_visible(vis);
                act.set_state(&vis.to_variant());
            }
        });

        w
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
    
    pub fn present(&self) {
        self.config_editor_frame.hide();
        self.window.present();
    }

    /* This is THE ONLY place allowed to modify context */
    pub fn attach_context(&self, context: Option<WindowContext>) {
        self.listing_frame.set_child(gtk::Widget::NONE);
        self.datapath_editor.set_model(Option::<&gtk::TreeModel>::None);
        self.hierarchy_editor.set_model(Option::<&gtk::SelectionModel>::None);
        self.window.insert_action_group("ctx", gio::ActionGroup::NONE);
        self.props_editor.unbind();

        *self.context.borrow_mut() = context;

        if let Some(new_context) = &*self.context.borrow() {
            self.listing_frame.set_child(Some(&new_context.lw));
            self.datapath_editor.set_model(Some(&new_context.datapath_model));
            self.hierarchy_editor.set_model(Some(&new_context.hierarchy_model));
            self.window.insert_action_group("ctx", Some(&new_context.action_group));
            self.props_editor.bind(&new_context);
            
            new_context.lw.grab_focus();
        }
    }
    
    pub fn close_file(&self) {
        self.attach_context(None);
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
        self.attach_context(Some(WindowContext::new(self, document::Document::new(space))));
    }
}

impl WindowContext {
    pub fn new(window: &rc::Rc<CharmWindow>, document: document::Document) -> WindowContext {
        let document_host = sync::Arc::new(document::DocumentHost::new(document));
        
        let lw = view::listing::ListingWidget::new();
        lw.init(window, document_host.clone());
        
        let (datapath_model, datapath_subscriber) = view::datapath::create_model(document_host.clone());
        let hierarchy_model = hierarchy::StructureSelectionModel::new(document_host.clone());
        
        let wc = WindowContext {
            document_host,
            
            window: rc::Rc::downgrade(window),
            
            lw,
            datapath_model,
            hierarchy_model,
            action_group: gio::SimpleActionGroup::new(),

            datapath_subscriber,
        };

        wc.action_group.add_action(&action::insert_node::create_action(&wc));
        wc.action_group.add_action(&action::insert_node::create_insert_fixed_size_node_at_cursor_action(&wc, "byte", 1));
        wc.action_group.add_action(&action::insert_node::create_insert_fixed_size_node_at_cursor_action(&wc, "word", 2));
        wc.action_group.add_action(&action::insert_node::create_insert_fixed_size_node_at_cursor_action(&wc, "dword", 4));
        wc.action_group.add_action(&action::insert_node::create_insert_fixed_size_node_at_cursor_action(&wc, "qword", 8));
        wc.action_group.add_action(&action::delete_node::create_action(&wc));
        //wc.action_group.add_action(&action::edit_props::create_action(&wc));
        wc.action_group.add_action(&action::navigate::create_action(&wc));
        wc.action_group.add_action(&action::nest::create_action(&wc));
        wc.action_group.add_action(&action::debug::reopen_current_document::create_action(&wc));
        
        wc
    }
}
