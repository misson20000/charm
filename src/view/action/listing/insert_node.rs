use std::cell;
use std::rc;
use std::sync;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::selection;
use crate::view::error;
use crate::view::helpers;
use crate::view::listing;
use crate::view::window;

use gtk::prelude::*;
use gtk::glib;
use gtk::glib::clone;
use gtk::gio;

struct InsertNodeAction {
    document_host: sync::Arc<document::DocumentHost>,
    activation: cell::RefCell<Option<InsertActivation>>,

    lw: listing::ListingWidget,
    window: rc::Weak<window::CharmWindow>,
    
    dialog: gtk::ApplicationWindow,
    
    name_entry: gtk::Entry,
    size_entry: gtk::Entry,
    offset_entry: gtk::Entry,
    order_entry: gtk::DropDown,
    path_display: gtk::Entry,
}

struct NestNodesAction {
    document_host: sync::Arc<document::DocumentHost>,
    activation: cell::RefCell<Option<NestActivation>>,

    lw: listing::ListingWidget,
    window: rc::Weak<window::CharmWindow>,
    
    dialog: gtk::ApplicationWindow,
    
    name_entry: gtk::Entry,
    path_display: gtk::Entry,
}

struct InsertActivation {
    document: sync::Arc<document::Document>,
    path: structure::Path,
}

struct NestActivation {
    document: sync::Arc<document::Document>,
    range: selection::listing::StructureRange,
}

pub fn create_actions(window_context: &window::WindowContext) -> (gio::SimpleAction, gio::SimpleAction) {
    let insert_action = InsertNodeAction::new(window_context);
    let nest_action   =  NestNodesAction::new(window_context);
    let insert_action_clone = insert_action.clone();

    let insert_gio_action = gio::SimpleAction::new("insert_node", None);

    insert_gio_action.connect_activate(move |_, _| {
        if nest_action.activate() {
        } else {
            insert_action.activate();
        }
    });
    
    insert_gio_action.set_enabled(true);

    (insert_gio_action,
     helpers::create_simple_action_strong(insert_action_clone, "force_insert_node", |ina| { ina.activate(); }))
}

impl InsertNodeAction {
    fn new(window_context: &window::WindowContext) -> rc::Rc<Self> {
        let builder = gtk::Builder::from_string(include_str!("insert-node.ui"));

        let name_entry: gtk::Entry = builder.object("name_entry").unwrap();
        let size_entry: gtk::Entry = builder.object("size_entry").unwrap();
        let offset_entry: gtk::Entry = builder.object("offset_entry").unwrap();
        let order_entry: gtk::DropDown = builder.object("order_entry").unwrap();
        let path_display: gtk::Entry = builder.object("path_display").unwrap();
        let insert_button: gtk::Button = builder.object("insert_button").unwrap();

        let dialog = gtk::ApplicationWindow::builder()
            .application(&window_context.window.upgrade().unwrap().application.application)
            .child(&builder.object::<gtk::Widget>("toplevel").unwrap())
            .resizable(true)
            .title("Insert node")
            .transient_for(&window_context.window.upgrade().unwrap().window)
            .hide_on_close(true)
            .destroy_with_parent(true)
            .default_widget(&insert_button)
            .build();
        
        let action = rc::Rc::new(InsertNodeAction {
            document_host: window_context.document_host.clone(),
            activation: cell::RefCell::new(None),
            lw: window_context.lw.clone(),
            window: window_context.window.clone(),
            dialog: dialog.clone(),
            
            name_entry,
            size_entry,
            offset_entry,
            order_entry,
            path_display,
        });

        helpers::bind_simple_action(&action, &action.dialog, "cancel", |action| {
            action.deactivate();
        });

        helpers::bind_simple_action(&action, &action.dialog, "insert", |action| {
            action.do_insert();
            action.deactivate();
        });

        dialog.connect_close_request(clone!(@weak action => @default-return glib::Propagation::Proceed, move |_| {
            action.deactivate();
            glib::Propagation::Proceed
        }));

        action
    }
    
    fn do_insert(&self) {
        if let Some(window) = self.window.upgrade() {
            let name = self.name_entry.text().as_str().to_string();

            let size_text = self.size_entry.text();
            let size = match addr::Address::parse(size_text.as_str()) {
                Ok(a) => a,
                Err(e) => {
                    window.report_error(error::Error {
                        while_attempting: error::Action::InsertNodeParseSize,
                        trouble: error::Trouble::AddressParseFailed {
                            error: e,
                            address: size_text.to_string(),
                        },
                        level: error::Level::Error,
                        is_bug: false,
                    });
                    return;
                }
            }.to_size();

            let offset_text = self.offset_entry.text();
            let offset = match addr::Address::parse(offset_text.as_str()) {
                Ok(a) => a,
                Err(e) => {
                    window.report_error(error::Error {
                        while_attempting: error::Action::InsertNodeParseOffset,
                        trouble: error::Trouble::AddressParseFailed {
                            error: e,
                            address: size_text.to_string(),
                        },
                        level: error::Level::Error,
                        is_bug: false,
                    });
                    return;
                }
            };

            let activation = match self.activation.take() {
                Some(a) => a,
                None => {
                    /* This shouldn't happen. */
                    return;
                }
            };
            
            let parent_node = activation.document.lookup_node(&activation.path).0;
            let index = self.order_entry.selected() as usize;

            if let Err((error, attempted_version)) = self.document_host.change(activation.document.insert_node(
                activation.path,
                index,
                structure::Childhood::new(
                    sync::Arc::new(structure::Node {
                        props: parent_node.props.clone_rename(name),
                        children: Vec::new(),
                        size,
                    }),
                    offset
                )
            )) {
                /* Inform the user that their action failed. */
                window.report_error(error::Error {
                    while_attempting: error::Action::InsertNode,
                    trouble: error::Trouble::DocumentUpdateFailure {
                        error,
                        attempted_version
                    },
                    level: error::Level::Error,
                    is_bug: false,
                });
            }
        }
    }
    
    pub fn activate(&self) -> bool {
        let selection = self.lw.selection();

        match &selection.mode {
            selection::listing::Mode::Structure(selection::listing::StructureMode::Range(range)) => {
                self.activate_impl(
                    selection.document.clone(),
                    range.path.clone(),
                    range.begin.0,
                    range.begin.1,
                    Some(range.extent().length()))
            },

            _ => {
                std::mem::drop(selection);
                let cursor = self.lw.cursor();
                self.activate_impl(
                    cursor.document(),
                    cursor.structure_path(),
                    cursor.structure_offset(),
                    cursor.structure_child_index(),
                    None)
            }
        }

        true
    }

    fn activate_impl(&self, document: sync::Arc<document::Document>, path: structure::Path, offset: addr::Address, index: usize, size: Option<addr::Size>) {
        let activation = InsertActivation {
            document,
            path,
        };

        self.offset_entry.set_text(&format!("{}", offset));
        self.size_entry.set_text(&size.map(|s| format!("{}", s.to_addr())).unwrap_or(String::new()));
        self.path_display.set_text(&activation.document.describe_path(&activation.path));

        let (node, _) = activation.document.lookup_node(&activation.path);
        let model = gtk::StringList::new(&[]);
        for i in &node.children {
            model.append(&i.node.props.name);
        }
        model.append("<end>");
        self.order_entry.set_model(Some(&model));
        self.order_entry.set_selected(index as u32);
        
        self.activation.replace(Some(activation));
        
        self.name_entry.grab_focus();
        self.dialog.present();
    }
    
    fn deactivate(&self) {
        self.order_entry.set_model(gio::ListModel::NONE);
        self.activation.take();
        self.dialog.hide();
    }
}

impl Drop for InsertNodeAction {
    fn drop(&mut self) {
        self.dialog.destroy();
    }
}

impl NestNodesAction {
    fn new(window_context: &window::WindowContext) -> rc::Rc<Self> {
        let builder = gtk::Builder::from_string(include_str!("nest-nodes.ui"));

        let name_entry: gtk::Entry = builder.object("name_entry").unwrap();
        let path_display: gtk::Entry = builder.object("path_display").unwrap();
        let nest_button: gtk::Button = builder.object("nest_button").unwrap();

        let dialog = gtk::ApplicationWindow::builder()
            .application(&window_context.window.upgrade().unwrap().application.application)
            .child(&builder.object::<gtk::Widget>("toplevel").unwrap())
            .resizable(true)
            .title("Nest nodes")
            .transient_for(&window_context.window.upgrade().unwrap().window)
            .hide_on_close(true)
            .destroy_with_parent(true)
            .default_widget(&nest_button)
            .build();
        
        let action = rc::Rc::new(NestNodesAction {
            document_host: window_context.document_host.clone(),
            activation: cell::RefCell::new(None),
            lw: window_context.lw.clone(),
            window: window_context.window.clone(),
            dialog: dialog.clone(),
            
            name_entry,
            path_display,
        });
        
        helpers::bind_simple_action(&action, &action.dialog, "cancel", |action| {
            action.deactivate();
        });

        helpers::bind_simple_action(&action, &action.dialog, "nest", |action| {
            action.do_nest();
            action.deactivate();
        });

        dialog.connect_close_request(clone!(@weak action => @default-return glib::Propagation::Proceed, move |_| {
            action.deactivate();
            glib::Propagation::Proceed
        }));

        action
    }
    
    fn do_nest(&self) {
        if let Some(window) = self.window.upgrade() {
            let name = self.name_entry.text().as_str().to_string();

            let activation = match self.activation.take() {
                Some(a) => a,
                None => {
                    /* This shouldn't happen. */
                    return;
                }
            };
            
            let parent_node = activation.document.lookup_node(&activation.range.path).0;

            let change = match activation.range.to_sibling_range_and_extent() {
                Ok((sibling_range, extent)) => activation.document.nest(
                    sibling_range,
                    extent,
                    parent_node.props.clone_rename(name)
                ),
                Err(sr) => {
                    let extent = sr.extent();
                    activation.document.insert_node(
                        sr.path,
                        sr.begin.1,
                        structure::Childhood::new(
                            sync::Arc::new(structure::Node {
                                props: parent_node.props.clone_rename(name),
                                children: Vec::new(),
                                size: extent.length(),
                            }),
                            sr.begin.0
                        )
                    )
                }
            };
            
            if let Err((error, attempted_version)) = self.document_host.change(change) {
                /* Inform the user that their action failed. */
                window.report_error(error::Error {
                    while_attempting: error::Action::NestNodesInListing,
                    trouble: error::Trouble::DocumentUpdateFailure {
                        error,
                        attempted_version
                    },
                    level: error::Level::Error,
                    is_bug: false,
                });
            }
        }
    }
    
    pub fn activate(&self) -> bool {
        let selection = self.lw.selection();

        match &selection.mode {
            selection::listing::Mode::Structure(selection::listing::StructureMode::Range(range)) => {
                self.activate_impl(selection.document.clone(), range.clone());
                true
            },

            _ => false
        }
    }

    fn activate_impl(&self, document: sync::Arc<document::Document>, range: selection::listing::StructureRange) {
        let activation = NestActivation {
            document,
            range,
        };

        self.path_display.set_text(&activation.document.describe_path(&activation.range.path));
        self.activation.replace(Some(activation));
        
        self.name_entry.grab_focus();
        self.dialog.present();
    }
    
    fn deactivate(&self) {
        self.activation.take();
        self.dialog.hide();
    }
}

impl Drop for NestNodesAction {
    fn drop(&mut self) {
        self.dialog.destroy();
    }
}

struct InsertFixedSizeNodeAtCursorAction {
    document_host: sync::Arc<document::DocumentHost>,
    lw: listing::ListingWidget
}

pub fn create_insert_fixed_size_node_at_cursor_action<S: Into<addr::Size>>(window_context: &window::WindowContext, name: &str, size: S) -> gio::SimpleAction {
    let document_host = window_context.document_host.clone();
    let lw = window_context.lw.clone();
    let action = gio::SimpleAction::new(&format!("insert_{}", name), None);
    let size = size.into();
    let name = name.to_string();
    let window = window_context.window.clone();

    action.connect_activate(move |_, _| {
        let mut cursor = lw.cursor_mut();
        
        match cursor.insert_node(&document_host, sync::Arc::new(structure::Node {
            props: structure::Properties {
                name: name.clone(),
                title_display: structure::TitleDisplay::Minor,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::default_hexdump(),
                locked: false,
            },
            children: Vec::new(),
            size,
        })) {
            Ok(()) => {},
            Err((error, attempted_version)) => {
                let report_error = match &error.ty {
                    /* don't bother popping up the dialog for this */
                    document::change::ApplyErrorType::InvalidParameters(_) => false,
                    _ => true
                };

                if report_error {
                    if let Some(window) = window.upgrade() {
                        window.report_error(error::Error {
                            while_attempting: error::Action::InsertNode,
                            trouble: error::Trouble::DocumentUpdateFailure {
                                error,
                                attempted_version
                            },
                            level: error::Level::Error,
                            is_bug: false,
                        });
                    }
                }
                
                std::mem::drop(cursor);
                lw.bonk();
            },
        }
    });

    action.set_enabled(true);

    action
}
