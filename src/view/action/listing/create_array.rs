use std::cell;
use std::rc;
use std::sync;

use crate::catch_panic;
use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::cursor;
use crate::model::listing::cursor::CursorClassExt;
use crate::model::listing::token::TokenKind;
use crate::model::selection;
use crate::view::addr_entry;
use crate::view::error;
use crate::view::helpers;
use crate::view::listing;
use crate::view::window;
use crate::view::window::ErrorReporter; 

use gtk::prelude::*;
use gtk::glib;
use gtk::glib::clone;
use gtk::gio;

struct CreateArrayAction {
    document_host: sync::Arc<document::DocumentHost>,
    activation: cell::RefCell<Option<CreateArrayActivation>>,

    lw: listing::ListingWidget,
    window: rc::Weak<window::CharmWindow>,
    
    dialog: gtk::ApplicationWindow,

    array_name_entry: gtk::Entry,
    item_size_display: addr_entry::AddrEntry,
    item_count_spinner: gtk::SpinButton,
    total_size_entry: addr_entry::AddrEntry,
    name_prefix: gtk::Entry,
    name_postfix: gtk::Entry,
    path_display: gtk::Entry,
    
    create_button: gtk::Button,

    high_count_warning: gtk::Label,
}

enum Mode {
    Range(selection::listing::StructureRange),
    Node {
        path: structure::Path,
        node: sync::Arc<structure::Node>
    },
}

struct CreateArrayActivation {
    document: sync::Arc<document::Document>,
    props: structure::Properties,
    mode: Mode,
}

pub fn add_action(window_context: &window::WindowContext) {
    let create_array_action = CreateArrayAction::new(window_context);
    let create_array_gio_action = gio::SimpleAction::new("create_array", None);

    create_array_gio_action.connect_activate(move |_, _| catch_panic! {
        create_array_action.activate();
    });
    
    create_array_gio_action.set_enabled(true);
    window_context.action_group.add_action(&create_array_gio_action);
}

impl CreateArrayAction {
    fn new(window_context: &window::WindowContext) -> rc::Rc<Self> {
        let builder = gtk::Builder::from_string(include_str!("array.ui"));

        let array_name_entry: gtk::Entry = builder.object("array_name_entry").unwrap();
        let item_size_display: addr_entry::AddrEntry = builder.object("item_size_display").unwrap();
        let item_count_spinner: gtk::SpinButton = builder.object("item_count_spinner").unwrap();
        let total_size_entry: addr_entry::AddrEntry = builder.object("total_size_entry").unwrap();
        let path_display: gtk::Entry = builder.object("path_display").unwrap();
        let name_prefix: gtk::Entry = builder.object("name_prefix").unwrap();
        let name_postfix: gtk::Entry = builder.object("name_postfix").unwrap();
        let create_button: gtk::Button = builder.object("create_button").unwrap();
        let high_count_warning: gtk::Label = builder.object("high_count_warning").unwrap();

        let dialog = gtk::ApplicationWindow::builder()
            .application(&window_context.window.upgrade().unwrap().application.application)
            .child(&builder.object::<gtk::Widget>("toplevel").unwrap())
            .resizable(true)
            .title("Create array")
            .transient_for(&window_context.window.upgrade().unwrap().window)
            .hide_on_close(true)
            .destroy_with_parent(true)
            .default_widget(&create_button)
            .build();
        
        let action = rc::Rc::new(CreateArrayAction {
            document_host: window_context.project.document_host.clone(),
            activation: cell::RefCell::new(None),
            lw: window_context.lw.clone(),
            window: window_context.window.clone(),
            dialog: dialog.clone(),

            array_name_entry,
            item_size_display,
            item_count_spinner,
            total_size_entry,
            name_prefix,
            name_postfix,
            path_display,
            create_button,
            high_count_warning,
        });

        helpers::bind_simple_action(&action, &action.dialog, "cancel", |action| {
            action.deactivate();
        });

        helpers::bind_simple_action(&action, &action.dialog, "create", |action| {
            action.do_create();
            action.deactivate();
        });

        action.total_size_entry.connect_activate(clone!(#[weak] action, move |entry| catch_panic! {
            match (entry.addr(), action.activation.borrow().as_ref()) {
                (Ok(offset), Some(activation)) => {
                    if activation.item_size() == addr::Offset::ZERO && offset != addr::Offset::ZERO {
                        entry.set_addr(addr::Offset::ZERO);
                    } else {
                        action.maybe_update_item_count(offset / activation.item_size());
                    }
                },
                _ => { },
            };

            action.update_dialog_preconditions();
        }));

        action.item_count_spinner.connect_value_changed(clone!(#[weak] action, move |spinner| catch_panic! {
            let v = spinner.value_as_int();

            if v < 0 {
                action.maybe_update_item_count(0);
            } else {
                action.maybe_update_item_count(v as u64);
            }
            
            action.update_dialog_preconditions();
        }));

        dialog.connect_close_request(clone!(#[weak] action, #[upgrade_or] glib::Propagation::Proceed, move |_| {
            catch_panic! {
                action.deactivate();
            };
            glib::Propagation::Proceed
        }));

        action
    }

    fn maybe_update_item_count(&self, count: u64) {
        let current_value = self.item_count_spinner.value_as_int();

        if current_value < 0 || current_value as u64 != count {
            self.item_count_spinner.set_value(count as f64);
        }

        self.high_count_warning.set_visible(count > 1000);
        
        if let Some(a) = self.activation.borrow().as_ref() {
            let new_total_size = a.item_size() * count;

            if Ok(new_total_size) != self.total_size_entry.addr() {
                self.total_size_entry.set_addr(new_total_size);
            }
        }
    }
    
    fn update_dialog_preconditions(&self) {
        let ok = self.total_size_entry.addr().is_ok()
            && self.item_count_spinner.value() >= 1.0;

        self.create_button.set_sensitive(ok);
    }
    
    fn do_create(&self) {
        if let Some(window) = self.window.upgrade() {
            let Some(activation) = self.activation.take() else { return };
            let item_count = self.item_count_spinner.value_as_int();
            let name_prefix = self.name_prefix.text().as_str().to_string();
            let name_postfix = self.name_postfix.text().as_str().to_string();

            let mut array_props = structure::Properties::default();
            array_props.name = self.array_name_entry.text().as_str().to_string();
            
            if item_count < 1 {
                return;
            }

            let item_count = item_count as usize;

            let (document, path, item_size) = match activation.mode {
                /* First we need to nest this into a node before we can Change::Repeat */
                Mode::Range(range) => {
                    let mut path = range.path.clone();
                    path.push(range.begin.1);

                    let size = range.extent().len();

                    let mut props = activation.props.clone();
                    props.title_display = structure::TitleDisplay::Inline;
                    props.name = format!("{}0{}", name_prefix, name_postfix);
                    
                    let change = match range.to_sibling_range_and_extent() {
                        Ok((sr, extent)) => activation.document.nest(sr, extent, props),
                        Err(sr) => activation.document.insert_node(sr.path, sr.begin.1, structure::Childhood::new(
                            sync::Arc::new(structure::Node {
                                props,
                                children: Vec::new(),
                                size,
                            }),
                            sr.begin.0
                        )),
                    };

                    let document = match self.document_host.change(change) {
                        Ok(document) => document,
                        Err((error, attempted_version)) => {
                            window.report_error(error::Error {
                                while_attempting: error::Action::CreateArray,
                                trouble: error::Trouble::DocumentUpdateFailure {
                                    error,
                                    attempted_version
                                },
                                level: error::Level::Error,
                                is_bug: false,
                            });
                            return;
                        }
                    };

                    (document, path, size)
                },

                Mode::Node { path, node } => (activation.document.clone(), path, node.size)
            };
            
            if let Err((error, attempted_version)) = self.document_host.change(document.repeat(path, item_size, item_count, name_prefix, name_postfix, array_props)) {
                /* Inform the user that their action failed. */
                window.report_error(error::Error {
                    while_attempting: error::Action::CreateArray,
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

        let activation = match &selection.mode {
            selection::listing::Mode::Structure(selection::listing::StructureMode::Range(range)) => {
                /* Create array from selection */

                CreateArrayActivation {
                    document: selection.document.clone(),
                    props: selection.document.lookup_node(&range.path[..]).0.props.clone(),
                    mode: Mode::Range(range.clone()),
                }
            },

            selection::listing::Mode::Structure(selection::listing::StructureMode::Empty) => {
                /* Create array from the node the cursor is on, if it's on a title */
                std::mem::drop(selection);

                let cursor = self.lw.cursor();
                match &(*cursor).class {
                    /* Explicitly forbid the root node. */
                    cursor::CursorClass::Title(tc) if tc.get_token().node_path().len() > 0 => CreateArrayActivation {
                        document: cursor.document(),
                        props: tc.get_token().node().props.clone(),
                        mode: Mode::Node {
                            path: tc.get_token().node_path().clone(),
                            node: tc.get_token().node().clone(),
                        },
                    },
                    
                    _ => {
                        std::mem::drop(cursor);
                        self.lw.bonk();
                        return;
                    }
                }
            },

            _ => return,
        };

        self.activation.take();
        
        let (prefix, postfix) = activation.suggest_naming_scheme();
        self.name_prefix.set_text(&prefix);
        self.name_postfix.set_text(&postfix);
        self.item_size_display.set_addr(activation.item_size());
        self.item_count_spinner.set_value(1.0);
        self.high_count_warning.set_visible(false);
        self.total_size_entry.set_addr(activation.item_size());
        self.path_display.set_text(&activation.document.describe_path(activation.path()));
        
        self.activation.replace(Some(activation));
        self.update_dialog_preconditions();
        self.array_name_entry.grab_focus();
        self.dialog.present();
    }

    fn deactivate(&self) {
        self.activation.take();
        self.update_dialog_preconditions();
        self.dialog.hide();
    }
}

impl Drop for CreateArrayAction {
    fn drop(&mut self) {
        self.dialog.destroy();
    }
}

impl CreateArrayActivation {
    fn item_size(&self) -> addr::Offset {
        match &self.mode {
            Mode::Range(range) => range.extent().len(),
            Mode::Node { node, .. } => node.size
        }
    }

    fn path(&self) -> structure::PathSlice {
        match &self.mode {
            Mode::Range(range) => &range.path[..],
            Mode::Node { path, .. } => &path[0..path.len()-1],
        }
    }

    fn suggest_naming_scheme(&self) -> (String, String) {
        match &self.mode {
            Mode::Range(_) => ("[".to_string(), "]".to_string()),
            Mode::Node { node, .. } => {
                let mut name = node.props.name.strip_suffix("[0]").map(str::to_string).unwrap_or(node.props.name.clone());
                name+= "[";
                (name, "]".to_string())
            },
        }
    }
}
