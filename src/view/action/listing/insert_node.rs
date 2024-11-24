use std::cell;
use std::rc;
use std::sync;

use crate::catch_panic;
use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::selection;
use crate::view::error;
use crate::view::helpers;
use crate::view::listing;
use crate::view::window;
use crate::view::window::ErrorReporter; 

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
    nest_enable: gtk::CheckButton,
    nest_entry: gtk::DropDown,
}

struct InsertActivation {
    document: sync::Arc<document::Document>,
    path: structure::Path,
}

pub fn add_actions(window_context: &window::WindowContext) {
    let insert_action = InsertNodeAction::new(window_context);
    let insert_gio_action = gio::SimpleAction::new("insert_node", None);

    insert_gio_action.connect_activate(move |_, _| catch_panic! {
        insert_action.activate();
    });
    
    insert_gio_action.set_enabled(true);
    window_context.action_group.add_action(&insert_gio_action);
}

impl InsertNodeAction {
    fn new(window_context: &window::WindowContext) -> rc::Rc<Self> {
        let builder = gtk::Builder::from_string(include_str!("insert-node.ui"));

        let name_entry: gtk::Entry = builder.object("name_entry").unwrap();
        let size_entry: gtk::Entry = builder.object("size_entry").unwrap();
        let offset_entry: gtk::Entry = builder.object("offset_entry").unwrap();
        let order_entry: gtk::DropDown = builder.object("order_entry").unwrap();
        let path_display: gtk::Entry = builder.object("path_display").unwrap();
        let nest_enable: gtk::CheckButton = builder.object("nest_enable").unwrap();
        let nest_entry: gtk::DropDown = builder.object("nest_entry").unwrap();
        let insert_button: gtk::Button = builder.object("insert_button").unwrap();

        nest_enable.bind_property("active", &nest_entry, "sensitive").sync_create().build();
        
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
            document_host: window_context.project.document_host.clone(),
            activation: cell::RefCell::new(None),
            lw: window_context.lw.clone(),
            window: window_context.window.clone(),
            dialog: dialog.clone(),
            
            name_entry,
            size_entry,
            offset_entry,
            order_entry,
            path_display,
            nest_enable,
            nest_entry,
        });

        helpers::bind_simple_action(&action, &action.dialog, "cancel", |action| {
            action.deactivate();
        });

        helpers::bind_simple_action(&action, &action.dialog, "insert", |action| {
            action.do_insert();
            action.deactivate();
        });

        dialog.connect_close_request(clone!(#[weak] action, #[upgrade_or] glib::Propagation::Proceed, move |_| {
            catch_panic! {
                action.deactivate();
            };
            glib::Propagation::Proceed
        }));

        action
    }

    fn change(&self) -> Result<document::change::Change, error::Error> {
        let name = self.name_entry.text().as_str().to_string();

        let size_text = self.size_entry.text();
        let size = match addr::Offset::parse(size_text.as_str(), false) {
            Ok(a) => a,
            Err(e) => return Err(error::Error {
                while_attempting: error::Action::InsertNodeParseSize,
                trouble: error::Trouble::AddressParseFailed {
                    error: e,
                    address: size_text.to_string(),
                },
                level: error::Level::Error,
                is_bug: false,
            })
        };

        let offset_text = self.offset_entry.text();
        let offset = match addr::Offset::parse(offset_text.as_str(), false) {
            Ok(a) => a,
            Err(e) => return Err(error::Error {
                while_attempting: error::Action::InsertNodeParseOffset,
                trouble: error::Trouble::AddressParseFailed {
                    error: e,
                    address: offset_text.to_string(),
                },
                level: error::Level::Error,
                is_bug: false,
            })
        };

        let activation = match self.activation.take() {
            Some(a) => a,
            None => {
                /* This shouldn't happen. */
                panic!("Insert node action running without activation");
            }
        };
        
        let parent_node = activation.document.lookup_node(&activation.path).0;
        let props = parent_node.props.clone_rename(name);
        let index = self.order_entry.selected() as usize;

        if self.nest_enable.is_active() {
            Ok(activation.document.nest(
                structure::SiblingRange::new(activation.path.clone(), index, self.nest_entry.selected() as usize),
                addr::Extent::sized(offset, size),
                props,
            ))
        } else {
            Ok(activation.document.insert_node(
                activation.path,
                index,
                structure::Childhood::new(
                    sync::Arc::new(structure::Node {
                        props,
                        children: Vec::new(),
                        size,
                    }),
                    offset
                )
            ))
        }
    }
    
    fn do_insert(&self) {
        if let Some(window) = self.window.upgrade() {
            let change = match self.change() {
                Ok(c) => c,
                Err(e) => return window.report_error(e)
            };
            
            if let Err((error, attempted_version)) = self.document_host.change(change) {
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
    
    pub fn activate(&self) {
        let selection = self.lw.selection();

        match &selection.mode {
            selection::listing::Mode::Structure(selection::listing::StructureMode::Range(range)) => {
                self.activate_impl(
                    selection.document.clone(),
                    range.path.clone(),
                    range.begin.0,
                    range.begin.1,
                    Some(range.extent().len()),

                    /* convert StructureRange's [begin, end) interval to inclusive Option<[begin, end]> interval */
                    if range.begin.1 == range.end.1 {
                        None
                    } else {
                        Some(range.end.1 - 1)
                    })
            },

            _ => {
                std::mem::drop(selection);
                let cursor = self.lw.cursor();
                self.activate_impl(
                    cursor.document(),
                    cursor.structure_path(),
                    cursor.structure_offset(),
                    cursor.structure_child_index(),
                    None,
                    None)
            }
        }
    }

    fn activate_impl(
        &self,
        document: sync::Arc<document::Document>,
        path: structure::Path,
        offset: addr::Offset,
        index: usize,
        size: Option<addr::Offset>,
        nest_through: Option<usize>) {
        
        let activation = InsertActivation {
            document,
            path,
        };

        self.offset_entry.set_text(&format!("{}", offset));
        self.size_entry.set_text(&size.map(|s| format!("{}", s)).unwrap_or(String::new()));
        self.path_display.set_text(&activation.document.describe_path(&activation.path));

        let (node, _) = activation.document.lookup_node(&activation.path);

        {
            let model = gtk::StringList::new(&[]);
            for i in &node.children {
                model.append(&i.node.props.name);
            }
            model.append("<end>");
            self.order_entry.set_model(Some(&model));
            self.order_entry.set_selected(index as u32);
        }

        self.nest_enable.set_active(nest_through.is_some());
        {
            let model = gtk::StringList::new(&[]);
            for i in &node.children {
                model.append(&i.node.props.name);
            }
            
            self.nest_entry.set_model(Some(&model));
            if let Some(nt) = nest_through {
                self.nest_entry.set_selected(nt as u32);
            } else {
                self.nest_entry.set_selected(node.children.len() as u32);
            }
        }
        
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

struct InsertFixedSizeNodeAtCursorAction {
    document_host: sync::Arc<document::DocumentHost>,
    lw: listing::ListingWidget
}

pub fn add_insert_fixed_size_node_at_cursor_action<S: Into<addr::Offset>>(window_context: &window::WindowContext, name: &str, size: S) {
    let document_host = window_context.project.document_host.clone();
    let lw = window_context.lw.clone();
    let action = gio::SimpleAction::new(&format!("insert_{}", name), None);
    let size = size.into();
    let name = name.to_string();
    let window = window_context.window.clone();

    action.connect_activate(move |_, _| catch_panic! {
        let mut cursor = lw.cursor_mut();
        
        match cursor.insert_node(&document_host, sync::Arc::new(structure::Node {
            props: structure::Properties {
                name: name.clone(),
                title_display: structure::TitleDisplay::Inline,
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

    window_context.action_group.add_action(&action);
}
