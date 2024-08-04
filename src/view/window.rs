use std::cell;
use std::rc;
use std::sync;

use crate::catch_panic;
use crate::model::addr;
use crate::model::document;
use crate::model::listing::cursor;
use crate::model::selection as selection_model;
use crate::model::versioned::Versioned;
use crate::view;
use crate::view::CharmApplication;
use crate::view::action;
use crate::view::breadcrumbs;
use crate::view::crashreport;
use crate::view::error;
use crate::view::helpers;
use crate::view::hierarchy;
use crate::view::selection;
use crate::view::project;
use crate::view::props_editor;

use gtk::gio;
use gtk::glib;
use gtk::glib::clone;
use gtk::prelude::*;

pub struct CharmWindow {
    pub application: rc::Rc<CharmApplication>,
    pub window: gtk::ApplicationWindow,
    pub id: u64,
    listing_container: gtk::Overlay,
    breadcrumbs: gtk::ListView,
    datapath_editor: gtk::TreeView,
    hierarchy_editor: gtk::ColumnView,
    pub props_editor: rc::Rc<props_editor::PropsEditor>,

    debug_revert_menu: gio::Menu,
    
    context: cell::RefCell<Option<WindowContext>>,
}

pub struct WindowContext {
    /// The window this context belongs to.
    pub window: rc::Weak<CharmWindow>,

    pub project: rc::Rc<project::Project>,
    pub tree_selection_host: sync::Arc<selection_model::tree::Host>,
    pub listing_selection_host: sync::Arc<selection_model::listing::Host>,
    
    /* Widgets and models */
    pub lw: view::listing::ListingWidget,
    pub datapath_model: gtk::TreeModel,
    pub tree_selection_model: selection::TreeSelectionModel,

    pub action_group: gio::SimpleActionGroup,
    
    /* Misc. subscribers and such that need to be kept around */
    document_subscriber_for_tree_selection_update: helpers::AsyncSubscriber,
    document_subscriber_for_listing_selection_update: helpers::AsyncSubscriber,
    document_subscriber_for_debug_revert_menu_update: helpers::AsyncSubscriber,
    document_subscriber_for_title_update: helpers::AsyncSubscriber,
    datapath_subscriber: helpers::AsyncSubscriber,
}

static NEXT_WINDOW_ID: sync::atomic::AtomicU64 = sync::atomic::AtomicU64::new(1);

impl CharmWindow {
    pub fn new(charm: &rc::Rc<CharmApplication>) -> rc::Rc<CharmWindow> {
        let id = NEXT_WINDOW_ID.fetch_add(1, sync::atomic::Ordering::Relaxed);
        let builder = gtk::Builder::from_string(include_str!("charm.ui"));

        let window: gtk::ApplicationWindow = builder.object("toplevel").unwrap();
        window.set_application(Some(&charm.application));

        let debug_revert_menu = gio::Menu::new();
        
        {
            let menu_bar = gio::Menu::new();
            {
                let file_menu = gio::Menu::new();
                {
                    let new_project_menu = gio::Menu::new();
                    new_project_menu.append(Some("Empty"), Some("win.new_project.empty"));
                    new_project_menu.append(Some("From File..."), Some("win.new_project.from_file"));
                    new_project_menu.freeze();
                    file_menu.append_submenu(Some("New Project"), &new_project_menu);
                }
                file_menu.append(Some("Open Project..."), Some("win.open_project"));
                file_menu.append(Some("Save Project"), Some("win.save_project"));
                file_menu.append(Some("Save Project As..."), Some("win.save_project_as"));
                file_menu.freeze();
                menu_bar.append_submenu(Some("File"), &file_menu);
            }
            {
                let edit_menu = gio::Menu::new();
                edit_menu.append(Some("Goto..."), Some("ctx.goto"));
                edit_menu.append(Some("Settings..."), Some("win.settings"));
                /*
                {
                    let mode_menu = gio::Menu::new();
                    mode_menu.append(Some("Command mode"), Some("listing.mode::command"));
                    mode_menu.append(Some("Hex entry"), Some("listing.mode::entry"));
                    mode_menu.append(Some("Text entry (UTF8)"), Some("listing.mode::utf8"));
                    mode_menu.append(Some("Insert"), Some("listing.insert_mode"));
                    edit_menu.append_section(Some("Edit Mode"), &mode_menu);
                    mode_menu.freeze();
            }
                */
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
                debug_menu.append(Some("Reset UI"), Some("win.debug.reopen_current_project"));
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
        
        let hierarchy_box: gtk::Box = builder.object("hierarchy_box").unwrap();
        let props_editor = props_editor::PropsEditor::new();
        hierarchy_box.append(props_editor.toplevel());
        
        let w = rc::Rc::new(CharmWindow {
            application: charm.clone(),
            window,
            id,
            listing_container: builder.object("listing_overlay").unwrap(),
            breadcrumbs: builder.object("breadcrumbs").unwrap(),
            datapath_editor,
            hierarchy_editor,
            props_editor,
            debug_revert_menu,
            context: cell::RefCell::new(None),
        });

        w.props_editor.bind_window(&w);

        w.window.connect_close_request(clone!(#[strong] w, move |_| catch_panic! {
            @default(glib::Propagation::Proceed);

            if let Some(context) = w.context.borrow().as_ref() {
                if context.project.has_unsaved_changes() {
                    let builder = gtk::Builder::from_string(include_str!("unsaved-dialog.ui"));
                    let dialog = gtk::ApplicationWindow::builder()
                        .application(&w.application.application)
                        .child(&builder.object::<gtk::Widget>("toplevel").unwrap())
                        .title("Unsaved changes")
                        .transient_for(&w.window)
                        .destroy_with_parent(true)
                        .modal(true)
                        .build();

                    helpers::bind_simple_action(&w, &dialog, "exit", clone!(#[strong] dialog, move |w| catch_panic! {
                        dialog.destroy();
                        w.close_window();
                        w.window.destroy();
                    }));

                    helpers::bind_simple_action(&w, &dialog, "cancel", clone!(#[strong] dialog, move |_| catch_panic! {
                        dialog.destroy();
                    }));

                    helpers::bind_simple_action(&w, &dialog, "save", clone!(#[strong] dialog, move |w| catch_panic! {
                        dialog.destroy();
                        w.window.lookup_action("save_project").expect("Expected save_project action to have been registered").activate(None);
                        w.close_window();
                        w.window.destroy();
                    }));
                    
                    dialog.present();
                    
                    return glib::Propagation::Stop;
                }
            }
            
            w.close_window();
            glib::Propagation::Proceed
        }));

        w.hierarchy_editor.connect_activate(clone!(#[weak] w, move |he, pos| catch_panic! {
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

        action::new_project::add_actions(&w);
        action::save_project::add_actions(&w);
        action::open_project::add_action(&w);
        action::debug::reopen_current_project::add_action(&w);
        action::settings::add_action(&w);
    
        helpers::bind_stateful_action(&w, &w.window, "view.datapath_editor", true, |act, w, state| {
            if let Some(vis) = state {
                w.datapath_editor.set_visible(vis);
                act.set_state(&vis.to_variant());
            }
        });
        
        w
    }
    
    pub fn present(&self) {
        self.window.present();
    }

    pub fn close_window(&self) {
        /* This is especially important because it destroys actions which might have their own toplevel windows that
         * would otherwise keep the process alive. */
        self.close_project();
        self.application.destroy_window(self);
    }
    
    /* This is THE ONLY place allowed to modify context */
    fn set_context(&self, context: Option<WindowContext>) {
        let mut guard = self.context.borrow_mut();

        self.listing_container.set_child(gtk::Widget::NONE);
        self.datapath_editor.set_model(Option::<&gtk::TreeModel>::None);
        self.hierarchy_editor.set_model(Option::<&gtk::SelectionModel>::None);
        self.window.insert_action_group("ctx", gio::ActionGroup::NONE);
        self.props_editor.unbind();
        self.breadcrumbs.set_factory(gtk::ListItemFactory::NONE);
        self.breadcrumbs.set_model(gtk::SelectionModel::NONE);
        self.debug_revert_menu.remove_all();

        *guard = context;

        if let Some(new_context) = guard.as_ref() {
            self.listing_container.set_child(Some(&new_context.lw));
            self.datapath_editor.set_model(Some(&new_context.datapath_model));
            self.hierarchy_editor.set_model(Some(&new_context.tree_selection_model));
            self.window.insert_action_group("ctx", Some(&new_context.action_group));
            self.props_editor.bind(&new_context);
            self.breadcrumbs.set_factory(Some(&breadcrumbs::CharmBreadcrumbWidget::list_item_factory(new_context.lw.clone())));
            self.breadcrumbs.set_model(Some(&gtk::NoSelection::new(Some(new_context.lw.breadcrumbs()))));
            
            new_context.lw.grab_focus();
        }

        self.update_title(guard.as_ref().map(|ctx| &ctx.project));
    }

    fn update_title(&self, project: Option<&rc::Rc<project::Project>>) {
        if let Some(project) = project {
            let has_unsaved = match project.has_unsaved_changes() {
                true => "*",
                false => "",
            };
            
            self.window.set_title(Some(&format!("Charm: {}{}", has_unsaved, project.title())));
        } else {
            self.window.set_title(Some("Charm (no project)"));
        }
    }
    
    pub fn has_project_open(&self) -> bool {
        self.context.borrow().is_some()
    }
    
    pub fn context(&self) -> cell::Ref<Option<WindowContext>> {
        self.context.borrow()
    }

    pub fn close_project(&self) {
        self.set_context(None);
    }

    pub fn open_project(self: &rc::Rc<Self>, project: project::Project, force: bool) {
        if self.has_project_open()  && !force {
            /* open a new window if this window already has something open in it */
            let window = self.application.new_window();
            window.set_context(Some(WindowContext::new(&window, project)));
            window.present();
        } else {
            self.set_context(Some(WindowContext::new(self, project)));
            self.present();
        }
    }
        
    pub fn report_error(&self, error: error::Error) {
        let dialog = error.create_dialog(&self.window);
        dialog.present();
    }
}

impl WindowContext {
    pub fn new(window: &rc::Rc<CharmWindow>, project: project::Project) -> WindowContext {
        let project = rc::Rc::new(project);
        let document_host = &project.document_host;
        let document = document_host.get();
        let tree_selection_host = sync::Arc::new(selection_model::tree::Host::new(selection_model::TreeSelection::new(document.clone())));
        let listing_selection_host = sync::Arc::new(selection_model::listing::Host::new(selection_model::ListingSelection::new(document.clone())));

        let document_subscriber_for_tree_selection_update = helpers::subscribe_to_updates(
            sync::Arc::downgrade(&tree_selection_host),
            document_host.clone(),
            document.clone(),
            clone!(#[weak] window, move |tree_selection_host, new_document| {
                let _circumstances = crashreport::circumstances([
                    crashreport::Circumstance::InWindow(window.id),
                    crashreport::Circumstance::TreeSelectionUpdate(tree_selection_host.clone(), new_document.clone()),
                ]);
                
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
            clone!(#[weak] window, move |listing_selection_host, new_document| {
                let _circumstances = crashreport::circumstances([
                    crashreport::Circumstance::InWindow(window.id),
                    crashreport::Circumstance::ListingSelectionUpdate(listing_selection_host.clone(), new_document.clone()),
                ]);

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

        let document_subscriber_for_title_update = helpers::subscribe_to_updates(
            rc::Rc::downgrade(window),
            document_host.clone(),
            document.clone(),
            clone!(#[strong] project, move |w, _| {
                w.update_title(Some(&project));
            }));
        
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
            
            project,
            tree_selection_host,
            listing_selection_host,
            
            lw,
            datapath_model,
            tree_selection_model,
            action_group: gio::SimpleActionGroup::new(),

            document_subscriber_for_tree_selection_update,
            document_subscriber_for_listing_selection_update,
            document_subscriber_for_debug_revert_menu_update,
            document_subscriber_for_title_update,
            datapath_subscriber,
        };

        action::listing::delete_selected_nodes::add_action(&wc);
        action::listing::insert_node::add_actions(&wc);
        action::listing::insert_node::add_insert_fixed_size_node_at_cursor_action(&wc, "byte", 1);
        action::listing::insert_node::add_insert_fixed_size_node_at_cursor_action(&wc, "word", 2);
        action::listing::insert_node::add_insert_fixed_size_node_at_cursor_action(&wc, "dword", 4);
        action::listing::insert_node::add_insert_fixed_size_node_at_cursor_action(&wc, "qword", 8);
        action::listing::goto::add_action(&wc);
        action::tree::delete_node::add_action(&wc);
        action::tree::nest::add_action(&wc);
        action::tree::destructure::add_action(&wc);
        action::debug::revert_document::add_action(&wc);
        
        wc
    }

    pub fn save_project(&self, project_file: gio::File) {
        let Some(window) = self.window.upgrade() else { return };
        
        let e = match self.project.try_save_to(project_file) {
            Ok(()) => {
                window.update_title(Some(&self.project));
                return;
            },
            Err(e) => e
        };
        
        match e {
            project::SaveProjectError::IoError(e) => window.report_error(error::Error {
                while_attempting: error::Action::SaveProject,
                trouble: error::Trouble::GlibIoError(e),
                level: error::Level::Error,
                is_bug: false,
            }),
            project::SaveProjectError::SerializationError(e) => window.report_error(error::Error {
                while_attempting: error::Action::SaveProject,
                trouble: error::Trouble::ProjectSerializationFailure(e),
                level: error::Level::Error,
                /* projects shouldn't fail to serialize... this indicates a bug */
                is_bug: true,
            }),
        };
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
