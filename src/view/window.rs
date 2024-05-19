use std::cell;
use std::rc;
use std::sync;

use crate::catch_panic;
use crate::model::addr;
use crate::model::datapath;
use crate::model::document;
use crate::model::listing::cursor;
use crate::model::selection as selection_model;
use crate::model::space;
use crate::model::versioned::Versioned;
use crate::view;
use crate::view::CharmApplication;
use crate::view::action;
use crate::view::error;
use crate::view::helpers;
use crate::view::hierarchy;
use crate::view::selection;
use crate::view::props_editor;
use crate::serialization;

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
    pub props_editor: rc::Rc<props_editor::PropsEditor>,

    debug_revert_menu: gio::Menu,
    
    context: cell::RefCell<Option<WindowContext>>,
}

pub struct WindowContext {
    /// The window this context belongs to.
    pub window: rc::Weak<CharmWindow>,

    /* Information about the file that is open. */
    pub document_host: sync::Arc<document::DocumentHost>,
    pub tree_selection_host: sync::Arc<selection_model::tree::Host>,
    pub listing_selection_host: sync::Arc<selection_model::listing::Host>,
    pub project_file: cell::RefCell<Option<gio::File>>,
    
    /* Widgets and models */
    pub lw: view::listing::ListingWidget,
    pub datapath_model: gtk::TreeModel,
    pub tree_selection_model: selection::TreeSelectionModel,

    action_group: gio::SimpleActionGroup,
    
    /* Misc. subscribers and such that need to be kept around */
    document_subscriber_for_tree_selection_update: helpers::AsyncSubscriber,
    document_subscriber_for_listing_selection_update: helpers::AsyncSubscriber,
    document_subscriber_for_debug_revert_menu_update: helpers::AsyncSubscriber,
    datapath_subscriber: helpers::AsyncSubscriber,
}

impl CharmWindow {
    pub fn new(charm: &rc::Rc<CharmApplication>) -> rc::Rc<CharmWindow> {
        let builder = gtk::Builder::from_string(include_str!("charm.ui"));

        let window: gtk::ApplicationWindow = builder.object("toplevel").unwrap();
        window.set_application(Some(&charm.application));

        let debug_revert_menu = gio::Menu::new();
        
        {
            let menu_bar = gio::Menu::new();
            {
                let file_menu = gio::Menu::new();
                file_menu.append(Some("New Window"), Some("app.new_window"));
                file_menu.append(Some("Open File..."), Some("win.open"));
                file_menu.append(Some("Open Project..."), Some("win.open_project"));
                file_menu.append(Some("Save Project"), Some("win.save_project"));
                file_menu.append(Some("Save Project As..."), Some("win.save_project_as"));
                file_menu.append(Some("Export patches (IPS)..."), Some("listing.export_ips"));
                file_menu.freeze();
                menu_bar.append_submenu(Some("File"), &file_menu);
            }
            {
                let edit_menu = gio::Menu::new();
                edit_menu.append(Some("Navigate..."), Some("ctx.navigate"));
                edit_menu.append(Some("Nest"), Some("ctx.nest"));
                edit_menu.append(Some("Destructure"), Some("ctx.destructure"));
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
                debug_menu.append(Some("Reset UI for document"), Some("ctx.debug.reopen_current_document"));
                debug_menu.append_submenu(Some("Revert document"), &debug_revert_menu);
                debug_menu.append(Some("Crash"), Some("app.crash"));
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
            menu.append(Some("Destructure"), Some("ctx.destructure"));
            menu.append(Some("Delete"), Some("ctx.delete_node"));
            let popover = gtk::PopoverMenu::from_model(Some(&menu));
            popover.set_parent(&hierarchy_editor);
            
            let gesture = gtk::GestureClick::new();
            gesture.connect_pressed(move |gesture, n_press, x, y| catch_panic! {
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
            hierarchy_editor.add_controller(gesture);
        }
        
        let config_editor = view::config_editor::build_config_editor(view::config::INSTANCE.clone());
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
            debug_revert_menu,
            context: cell::RefCell::new(None),
        });

        w.props_editor.bind_window(&w);

        w.window.connect_close_request(clone!(@strong charm, @strong w => move |_| catch_panic! {
            @default(glib::Propagation::Proceed);
            
            /* This is especially important because it destroys actions which might have their own toplevel windows that
             * would otherwise keep the process alive. */
            w.close_file();
            charm.destroy_window(&w);

            glib::Propagation::Proceed
        }));

        w.hierarchy_editor.connect_activate(clone!(@weak w => move |he, pos| catch_panic! {
            let guard = w.context.borrow();
            let Some(ctx) = guard.as_ref() else { return };
            let Some(model) = he.model() else { return };
            let Some(object) = model.item(pos) else { return };
            let Ok(tlr) = object.downcast::<gtk::TreeListRow>() else { return };
            let Some(node_item_object) = tlr.item() else { return };
            let Ok(node_item) = node_item_object.downcast::<hierarchy::NodeItem>() else { return };

            let info = node_item.info();
            
            ctx.lw.goto(&info.document, &info.path, addr::unit::NULL, cursor::PlacementHint::Title);
        }));
        
        /* window actions */

        w.window.add_action(&action::open_file::create_action(&w));

        let (action_save, action_save_as) = action::save_project::create_actions(&w);
        w.window.add_action(&action_save);
        w.window.add_action(&action_save_as);

        w.window.add_action(&action::open_project::create_action(&w));
        
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
        self.debug_revert_menu.remove_all();

        *self.context.borrow_mut() = context;

        if let Some(new_context) = &*self.context.borrow() {
            self.listing_frame.set_child(Some(&new_context.lw));
            self.datapath_editor.set_model(Some(&new_context.datapath_model));
            self.hierarchy_editor.set_model(Some(&new_context.tree_selection_model));
            self.window.insert_action_group("ctx", Some(&new_context.action_group));
            self.props_editor.bind(&new_context);
            
            new_context.lw.grab_focus();
        }
    }

    pub fn has_file_open(&self) -> bool {
        self.context.borrow().is_some()
    }
    
    pub fn context(&self) -> cell::Ref<Option<WindowContext>> {
        self.context.borrow()
    }

    pub fn close_file(&self) {
        self.attach_context(None);
    }
    
    pub fn open_file(self: &rc::Rc<Self>, file: &gio::File) {
        let attributes = file.query_info("standard::display-name", gio::FileQueryInfoFlags::NONE, Option::<&gio::Cancellable>::None).unwrap();
        let dn = attributes.attribute_as_string("standard::display-name").unwrap();

        let fas = space::file::FileAddressSpace::new(file.path().unwrap(), &dn);
        fas.open();
        
        let space = std::sync::Arc::new(fas.into());

        self.window.set_title(Some(format!("Charm: {}", dn).as_str()));
        // TODO: error handling

        let doc = document::Builder::default()
            .load_space(space)
            .build();
        
        self.attach_context(Some(WindowContext::new(self, doc, None)));
    }

    pub fn open_project(self: &rc::Rc<Self>, project_file: gio::File) {
        let e = match self.try_open_project(project_file) {
            Ok(()) => return,
            Err(e) => e
        };

        match e {
            OpenProjectError::IoError(e) => self.report_error(error::Error {
                while_attempting: error::Action::OpenProject,
                trouble: error::Trouble::GlibIoError(e),
                level: error::Level::Error,
                is_bug: false,
            }),
            OpenProjectError::DeserializationError(e) => self.report_error(error::Error {
                while_attempting: error::Action::OpenProject,
                trouble: error::Trouble::ProjectDeserializationFailure(e),
                level: error::Level::Error,
                is_bug: false,
            }),
        };
    }

    fn try_open_project(self: &rc::Rc<Self>, project_file: gio::File) -> Result<(), OpenProjectError> {
        let (bytes, _string) = project_file.load_bytes(gio::Cancellable::NONE)?;
        let document = serialization::deserialize_project(bytes.as_ref())?;

        /* Open any FileAddressSpaces that don't try to get opened during deserialization. */
        for filter in &document.datapath {
            match filter {
                datapath::Filter::LoadSpace(lsf) => match &**lsf.space() {
                    space::AddressSpace::File(f) => f.open(),
                },
                _ => {}
            }
        }
        
        self.attach_context(Some(WindowContext::new(self, document, Some(project_file))));

        Ok(())
    }
    
    pub fn report_error(&self, error: error::Error) {
        let dialog = error.create_dialog(&self.window);
        dialog.present();
    }
}

impl WindowContext {
    pub fn new(window: &rc::Rc<CharmWindow>, document: document::Document, project_file: Option<gio::File>) -> WindowContext {
        let document_host = sync::Arc::new(document::DocumentHost::new(document));
        let document = document_host.get();
        let tree_selection_host = sync::Arc::new(selection_model::tree::Host::new(selection_model::TreeSelection::new(document.clone())));
        let listing_selection_host = sync::Arc::new(selection_model::listing::Host::new(selection_model::ListingSelection::new(document.clone())));

        let document_subscriber_for_tree_selection_update = helpers::subscribe_to_updates(
            sync::Arc::downgrade(&tree_selection_host),
            document_host.clone(),
            document.clone(),
            clone!(@weak window => move |tree_selection_host, new_document| {
                // TODO: catch panics, rescue by reopening document
                if let Err((error, attempted_version)) = tree_selection_host.change(selection_model::tree::Change::DocumentUpdated(new_document.clone())) {
                    match error {
                        selection_model::tree::ApplyError::WasUpToDate => { /* ignore */ },

                        /* DocumentUpdated shouldn't fail in any other way */
                        error => window.report_error(error::Error {
                            while_attempting: error::Action::TreeSelectionDocumentUpdate,
                            trouble: error::Trouble::TreeSelectionUpdateFailure {
                                error,
                                attempted_version,
                            },
                            level: error::Level::Warning,
                            is_bug: true,
                        }),
                    }
                }
            }));

        let document_subscriber_for_listing_selection_update = helpers::subscribe_to_updates(
            sync::Arc::downgrade(&listing_selection_host),
            document_host.clone(),
            document.clone(),
            clone!(@weak window => move |listing_selection_host, new_document| {
                // TODO: catch panics, rescue by reopening document
                if let Err((error, attempted_version)) = listing_selection_host.change(selection_model::listing::Change::DocumentUpdated(new_document.clone())) {
                    /* listing::Change::DocumentUpdated should never fail, but if it ever does, report it as a bug. */
                    window.report_error(error::Error {
                        while_attempting: error::Action::ListingSelectionDocumentUpdate,
                        trouble: error::Trouble::ListingSelectionUpdateFailure {
                            error,
                            attempted_version,
                        },
                        level: error::Level::Warning,
                        is_bug: true,
                    });
                }
            }));

        update_debug_revert_menu(&window.debug_revert_menu, &document);
        let document_subscriber_for_debug_revert_menu_update = helpers::subscribe_to_updates(
            window.debug_revert_menu.downgrade(),
            document_host.clone(),
            document.clone(),
            move |drm, new_document| {
                update_debug_revert_menu(&drm, &new_document);
            });
        
        let lw = view::listing::ListingWidget::new();
        lw.init(
            window,
            document_host.clone(),
            listing_selection_host.clone()
        );
        
        let (datapath_model, datapath_subscriber) = view::datapath::create_model(document_host.clone());
        let tree_selection_model = selection::TreeSelectionModel::new(window, tree_selection_host.clone(), document_host.clone());
        
        let wc = WindowContext {
            window: rc::Rc::downgrade(window),
            
            document_host,
            tree_selection_host,
            listing_selection_host,
            project_file: cell::RefCell::new(project_file),
            
            lw,
            datapath_model,
            tree_selection_model,
            action_group: gio::SimpleActionGroup::new(),

            document_subscriber_for_tree_selection_update,
            document_subscriber_for_listing_selection_update,
            document_subscriber_for_debug_revert_menu_update,
            datapath_subscriber,
        };

        let (insert, nest) = action::listing::insert_node::create_actions(&wc);
        wc.action_group.add_action(&insert);
        wc.action_group.add_action(&nest);
        wc.action_group.add_action(&action::listing::insert_node::create_insert_fixed_size_node_at_cursor_action(&wc, "byte", 1));
        wc.action_group.add_action(&action::listing::insert_node::create_insert_fixed_size_node_at_cursor_action(&wc, "word", 2));
        wc.action_group.add_action(&action::listing::insert_node::create_insert_fixed_size_node_at_cursor_action(&wc, "dword", 4));
        wc.action_group.add_action(&action::listing::insert_node::create_insert_fixed_size_node_at_cursor_action(&wc, "qword", 8));
        wc.action_group.add_action(&action::listing::navigate::create_action(&wc));
        wc.action_group.add_action(&action::tree::delete_node::create_action(&wc));
        wc.action_group.add_action(&action::tree::nest::create_action(&wc));
        wc.action_group.add_action(&action::tree::destructure::create_action(&wc));
        wc.action_group.add_action(&action::debug::reopen_current_document::create_action(&wc));
        wc.action_group.add_action(&action::debug::revert_document::create_action(&wc));
        
        wc
    }

    pub fn save_project(&self, project_file: gio::File) {
        let e = match self.try_save_project(project_file) {
            Ok(()) => return,
            Err(e) => e
        };

        let window = match self.window.upgrade() {
            Some(window) => window,
            None => return /* vanish this error into the ether... */
        };
        
        match e {
            SaveProjectError::IoError(e) => window.report_error(error::Error {
                while_attempting: error::Action::SaveProject,
                trouble: error::Trouble::GlibIoError(e),
                level: error::Level::Error,
                is_bug: false,
            }),
            SaveProjectError::SerializationError(e) => window.report_error(error::Error {
                while_attempting: error::Action::SaveProject,
                trouble: error::Trouble::ProjectSerializationFailure(e),
                level: error::Level::Error,
                /* projects shouldn't fail to serialize... this indicates a bug */
                is_bug: true,
            }),
        };
    }

    fn try_save_project(&self, project_file: gio::File) -> Result<(), SaveProjectError> {
        let bytes = serialization::serialize_project(&**self.document_host.borrow())?;

        project_file.replace_contents(&bytes[..], None, false, gio::FileCreateFlags::REPLACE_DESTINATION, gio::Cancellable::NONE)?;

        let mut guard = self.project_file.borrow_mut();
        
        if guard.as_ref() != Some(&project_file) {
            *guard = Some(project_file);
        }
        
        Ok(())
    }
}

fn update_debug_revert_menu(menu: &gio::Menu, mut document: &sync::Arc<document::Document>) {
    menu.remove_all();

    let submenu = gio::Menu::new();
    let mut current_menu = menu;

    for i in 0u32..10 {
        let (prev_doc, change) = match document.previous() {
            Some((document, change)) => (document, change),
            None => return
        };
        document = prev_doc;

        let item = gio::MenuItem::new(Some(&change.summarize(document)), None);
        item.set_action_and_target_value(Some("ctx.debug.revert_document"), Some(&i.to_variant()));

        current_menu.append_item(&item);

        if i == 0 {
            menu.append_section(Some("Previous versions"), &submenu);
            current_menu = &submenu;
        }
    }
}

pub enum SaveProjectError {
    IoError(glib::error::Error),
    SerializationError(serialization::SerializationError),
}

impl From<glib::error::Error> for SaveProjectError {
    fn from(e: glib::error::Error) -> SaveProjectError {
        SaveProjectError::IoError(e)
    }
}

impl From<serialization::SerializationError> for SaveProjectError {
    fn from(e: serialization::SerializationError) -> SaveProjectError {
        SaveProjectError::SerializationError(e)
    }
}

pub enum OpenProjectError {
    IoError(glib::error::Error),
    DeserializationError(serialization::DeserializationError),
}

impl From<glib::error::Error> for OpenProjectError {
    fn from(e: glib::error::Error) -> OpenProjectError {
        OpenProjectError::IoError(e)
    }
}

impl From<serialization::DeserializationError> for OpenProjectError {
    fn from(e: serialization::DeserializationError) -> OpenProjectError {
        OpenProjectError::DeserializationError(e)
    }
}
