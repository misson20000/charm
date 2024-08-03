use std::cell;
use std::rc;
use std::rc::Rc;
use std::sync;

use gtk::prelude::*;
use gtk::gio;
use gtk::glib;
use gtk::glib::clone;

use crate::catch_panic;
use crate::model::document;
use crate::model::document::structure;
use crate::model::selection;
use crate::model::versioned::Versioned;
use crate::view::error;
use crate::view::helpers;
use crate::view::window;

enum PropsEditorMode {
    Deactivated,
    Single {
        path: structure::Path,
        props: structure::Properties,
    },
    Many {
        selection: sync::Arc<selection::TreeSelection>,
        props: structure::MaybeProperties,
    },
}

struct PropsInterior {
    document_host: sync::Arc<document::DocumentHost>,
    selection_host: sync::Arc<selection::tree::Host>,
    selection: sync::Arc<selection::TreeSelection>,

    mode: PropsEditorMode,
    
    subscriber: helpers::AsyncSubscriber,    
}

pub struct PropsEditor {
    pub toplevel: gtk::Widget,

    name_entry: gtk::Entry,
    size_entry: gtk::Entry,
    
    title_display: gtk::DropDown,
    children_display: gtk::DropDown,
    content_display: gtk::DropDown,

    title_model: gtk::StringList,
    children_model: gtk::StringList,
    content_model: gtk::StringList,
    
    locked: gtk::CheckButton,
    path_display: gtk::Entry,

    in_update: cell::Cell<bool>,
    interior: cell::RefCell<Option<PropsInterior>>,
    window: cell::RefCell<rc::Weak<window::CharmWindow>>,
}

impl PropsEditor {
    pub fn new() -> Rc<PropsEditor> {
        let builder = gtk::Builder::from_string(include_str!("props-editor.ui"));

        let toplevel: gtk::Widget = builder.object("toplevel").unwrap();
        
        let name_entry: gtk::Entry = builder.object("name_entry").unwrap();
        let size_entry: gtk::Entry = builder.object("size_entry").unwrap();
        let title_display: gtk::DropDown = builder.object("title_display").unwrap();
        let children_display: gtk::DropDown = builder.object("children_display").unwrap();
        let content_display: gtk::DropDown = builder.object("content_display").unwrap();
        let locked: gtk::CheckButton = builder.object("locked").unwrap();
        let path_display: gtk::Entry = builder.object("path_display").unwrap();

        let title_model = gtk::StringList::new(&["Inline", "Major", "Minor"]);
        let children_model = gtk::StringList::new(&["Hidden", "Summary", "Full"]);
        let content_model = gtk::StringList::new(&["Hidden", "Hexdump", "Hexstring"]);
        
        let pe = PropsEditor {
            toplevel,
            name_entry,
            size_entry,
            
            title_display,
            children_display,
            content_display,

            title_model,
            children_model,
            content_model,
            
            locked,
            path_display,
            in_update: cell::Cell::new(false),
            interior: cell::RefCell::new(None),
            window: Default::default(),
        };
        
        pe.unbind();

        let pe = Rc::new(pe);

        pe.name_entry.buffer().connect_text_notify(clone!(#[weak] pe, move |buffer| catch_panic! {
            pe.apply_props(structure::MaybeProperties::new_name(buffer.text().to_string()));
        }));

        pe.title_display.connect_selected_notify(clone!(#[weak] pe, move |dd| catch_panic! {
            pe.apply_props(structure::MaybeProperties::new_title_display(match dd.selected() {
                0 => structure::TitleDisplay::Inline,
                1 => structure::TitleDisplay::Major,
                2 => structure::TitleDisplay::Minor,
                gtk::INVALID_LIST_POSITION => return,
                x => panic!("unexpected selected index: {}", x)
            }));
        }));

        pe.children_display.connect_selected_notify(clone!(#[weak] pe, move |dd| catch_panic! {
            pe.apply_props(structure::MaybeProperties::new_children_display(match dd.selected() {
                0 => structure::ChildrenDisplay::None,
                1 => structure::ChildrenDisplay::Summary,
                2 => structure::ChildrenDisplay::Full,
                gtk::INVALID_LIST_POSITION => return,
                x => panic!("unexpected selected index: {}", x)
            }));
        }));

        pe.content_display.connect_selected_notify(clone!(#[weak] pe, move |dd| catch_panic! {
            pe.apply_props(structure::MaybeProperties::new_content_display(match dd.selected() {
                0 => structure::ContentDisplay::None,
                1 => structure::ContentDisplay::default_hexdump(),
                2 => structure::ContentDisplay::Hexstring,
                gtk::INVALID_LIST_POSITION => return,
                x => panic!("unexpected selected index: {}", x)
            }));
        }));
        
        pe
    }

    pub fn bind_window(&self, window: &Rc<window::CharmWindow>) {
        *self.window.borrow_mut() = Rc::downgrade(window);
    }
    
    fn apply_props(&self, prop_changes: structure::MaybeProperties) {
        if !self.in_update.get() {
            let mut interior_guard = self.interior.borrow_mut();
            if let Some(interior) = interior_guard.as_mut() {
                let window = match self.window.borrow().upgrade() {
                    Some(window) => window,
                    None => return,
                };
                
                match match &mut interior.mode {
                    PropsEditorMode::Deactivated => { /* silently discard this error */ return; },
                    
                    PropsEditorMode::Single { path, props } => {
                        props.apply_changes(prop_changes);
                        
                        interior.document_host.change(interior.selection.document.alter_node(path.clone(), props.clone()))
                    },

                    PropsEditorMode::Many { selection, props } => {
                        props.apply_changes(prop_changes.clone());

                        interior.document_host.change(interior.selection.document.alter_nodes_bulk(selection.clone(), prop_changes))
                    },
                } {
                    Ok(new_document) => {
                        /* Sometimes gtk will cause us to issue two property updates in
                         * quick succession without a chance for the selection to get
                         * updated and for us to pick that up, which would cause the second
                         * update to get issued against an old version of the selection
                         * referencing an old version of the document. We can't have that,
                         * so update the selection synchronously right here right now. */

                        let new_selection = match interior.selection_host.change(selection::tree::Change::DocumentUpdated(new_document)) {
                            Ok(new) => new,

                            Err((selection::tree::ApplyError::WasUpToDate, _)) => return,

                            Err((error, attempted_version)) => {
                                window.report_error(error::Error {
                                    while_attempting: error::Action::EditProperties,
                                    trouble: error::Trouble::TreeSelectionUpdateFailure {
                                        error,
                                        attempted_version
                                    },
                                    level: error::Level::Warning,
                                    is_bug: true,
                                });

                                return
                            },
                        };
                        
                        self.in_update.set(true);
                        new_selection.changes_since(&interior.selection.clone(), &mut |selection, record| self.update_selection_internal(interior, selection.clone(), Some(record)));
                        self.in_update.set(false);
                    },
                    
                    Err((error, attempted_version)) => {
                        window.report_error(error::Error {
                            while_attempting: error::Action::EditProperties,
                            trouble: error::Trouble::DocumentUpdateFailure {
                                error,
                                attempted_version
                            },
                            level: error::Level::Error,
                            is_bug: false,
                        });

                        return;
                    },
                }
            }
        }        
    }
    
    pub fn unbind(&self) {
        self.interior.take();
        self.name_entry.set_sensitive(false);
        self.children_display.set_sensitive(false);
    }
    
    pub fn bind(self: &Rc<Self>, ctx: &window::WindowContext) {
        let selection_host = ctx.tree_selection_host.clone();
        let selection = ctx.tree_selection_host.get();

        let mut interior = PropsInterior {
            document_host: ctx.project.document_host.clone(),
            selection_host: selection_host.clone(),
            selection: selection.clone(),

            mode: PropsEditorMode::Deactivated,

            subscriber: helpers::subscribe_to_updates(Rc::downgrade(self), selection_host, selection.clone(), |pe, new_sel| pe.update_selection(new_sel)),
        };
        
        self.update_selection_internal(&mut interior, selection, None);
        *self.interior.borrow_mut() = Some(interior);
    }

    pub fn update_selection(&self, selection: &sync::Arc<selection::TreeSelection>) {
        self.in_update.set(true);
        let mut interior_guard = self.interior.borrow_mut();
        if let Some(interior) = interior_guard.as_mut() {
            selection.changes_since(&interior.selection.clone(), &mut |selection, record| self.update_selection_internal(interior, selection.clone(), Some(record)));
        }
        drop(interior_guard);
        self.in_update.set(false);
    }

    fn selection_to_mode(selection: &sync::Arc<selection::TreeSelection>) -> PropsEditorMode {
        if selection.any_selected() {
            if let Some(path) = selection.single_selected() {
                let (node, _addr) = selection.document.lookup_node(&path);
                PropsEditorMode::Single { path, props: node.props.clone() }
            } else if let Some(props) = structure::MaybeProperties::common_between(selection.node_iter()) {
                PropsEditorMode::Many {
                    selection: selection.clone(),
                    props
                }
            } else {
                PropsEditorMode::Deactivated
            }
        } else {
            PropsEditorMode::Deactivated
        }
    }
    
    fn update_selection_internal(&self, interior: &mut PropsInterior, selection: sync::Arc<selection::TreeSelection>, record: Option<&selection::tree::ChangeRecord>) {
        let new_mode = Self::selection_to_mode(&selection);
        let changed = record.map_or(true, |record| record.selection_changed);

        interior.selection = selection.clone();
        
        match (&interior.mode, &new_mode) {
            (_, PropsEditorMode::Deactivated) => {
                self.deactivate_controls();
            },
            (PropsEditorMode::Single { props: props_old, .. }, PropsEditorMode::Single { props: props_new, .. }) if props_old == props_new && !changed => {
                /* If the selection didn't change and the properties don't disagree with what we think they are, DON'T update the interactive controls. This resets text box cursor positions. */
            },
            (PropsEditorMode::Many { props: props_old, .. }, PropsEditorMode::Many { props: props_new, .. }) if props_old == props_new && !changed => {
                /* If the selection didn't change and the properties don't disagree with what we think they are, DON'T update the interactive controls. This resets text box cursor positions. */
            },
            (_, PropsEditorMode::Single { props, .. }) => self.update_controls(&structure::MaybeProperties::new(props.clone())),
            (_, PropsEditorMode::Many { props, .. }) => self.update_controls(&props),
        }

        match &new_mode {
            PropsEditorMode::Single { path, .. } => {
                self.update_path_control(&selection.document, Some(path));
            },
            _ => {
                self.update_path_control(&selection.document, None);
            }
        }

        interior.mode = new_mode;
    }
        
    fn update_controls(&self, props: &structure::MaybeProperties) {
        match &props.name {
            Some(name) => self.name_entry.set_text(name),
            None => self.name_entry.set_text(""),
        }

        self.title_display.set_model(Some(&self.title_model));
        self.title_display.set_selected(match props.title_display {
            Some(structure::TitleDisplay::Inline) => 0,
            Some(structure::TitleDisplay::Major) => 1,
            Some(structure::TitleDisplay::Minor) => 2,
            None => gtk::INVALID_LIST_POSITION,
        });

        self.children_display.set_model(Some(&self.children_model));
        self.children_display.set_selected(match &props.children_display {
            Some(structure::ChildrenDisplay::None) => 0,
            Some(structure::ChildrenDisplay::Summary) => 1,
            Some(structure::ChildrenDisplay::Full) => 2,
            None => gtk::INVALID_LIST_POSITION,
        });

        self.content_display.set_model(Some(&self.content_model));
        self.content_display.set_selected(match &props.content_display {
            Some(structure::ContentDisplay::None) => 0,
            Some(structure::ContentDisplay::Hexdump { .. }) => 1,
            Some(structure::ContentDisplay::Hexstring) => 2,
            None => gtk::INVALID_LIST_POSITION,
        });

        match &props.locked {
            Some(x) => {
                self.locked.set_inconsistent(false);
                self.locked.set_active(*x);
            },
            None => self.locked.set_inconsistent(true),
        }
        
        self.name_entry.set_sensitive(true);
        self.size_entry.set_sensitive(true);
        self.title_display.set_sensitive(true);
        self.children_display.set_sensitive(true);
        self.content_display.set_sensitive(true);
        self.locked.set_sensitive(true);
    }

    fn deactivate_controls(&self) {
        self.name_entry.set_text("");
        self.title_display.set_model(gio::ListModel::NONE);
        self.children_display.set_model(gio::ListModel::NONE);
        self.content_display.set_model(gio::ListModel::NONE);
        self.locked.set_inconsistent(false);
        self.locked.set_active(false);
            
        self.name_entry.set_sensitive(false);
        self.size_entry.set_sensitive(false);
        self.title_display.set_sensitive(false);
        self.children_display.set_sensitive(false);
        self.content_display.set_sensitive(false);
        self.locked.set_sensitive(false);
    }

    fn update_path_control(&self, document: &document::Document, path: Option<&structure::Path>) {
        if let Some(path) = path {
            self.path_display.set_text(&document.describe_path(path));
        } else {
            self.path_display.set_text("");
        }
        self.path_display.set_sensitive(false);
    }
    
    pub fn toplevel(&self) -> &gtk::Widget {
        &self.toplevel
    }

    pub fn focus_name(&self) {
        self.name_entry.grab_focus();
    }
}
