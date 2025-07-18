use std::cell;
use std::rc;
use std::rc::Rc;
use std::sync;

use gtk::prelude::*;
use gtk::glib;
use gtk::glib::clone;

use crate::catch_panic;
use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::selection;
use crate::model::versioned::Versioned;
use crate::view::addr_entry;
use crate::view::error;
use crate::view::helpers;
use crate::view::window;
use crate::view::window::ErrorReporter;

enum PropsEditorMode {
    Deactivated,
    Single {
        path: structure::Path,
        props: structure::Properties,
        size: addr::Offset,
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
    size_entry: addr_entry::AddrEntry,
    
    title_display: gtk::Stack,
    title_display_switcher: gtk::StackSwitcher,
    children_display: gtk::Stack,
    children_display_switcher: gtk::StackSwitcher,
    content_display: gtk::Stack,
    content_display_switcher: gtk::StackSwitcher,

    hexdump_line_pitch_entry: addr_entry::AddrEntry,
    hexdump_gutter_pitch_entry: addr_entry::AddrEntry,
    bindump_line_pitch_entry: addr_entry::AddrEntry,
    bindump_word_size_entry: addr_entry::AddrEntry,

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
        let size_entry: addr_entry::AddrEntry = builder.object("size_entry").unwrap();
        let title_display: gtk::Stack = builder.object("title_display").unwrap();
        let title_display_switcher: gtk::StackSwitcher = builder.object("title_display_switcher").unwrap();
        let children_display: gtk::Stack = builder.object("children_display").unwrap();
        let children_display_switcher: gtk::StackSwitcher = builder.object("children_display_switcher").unwrap();
        let content_display: gtk::Stack = builder.object("content_display").unwrap();
        let content_display_switcher: gtk::StackSwitcher = builder.object("content_display_switcher").unwrap();
        let hexdump_line_pitch_entry: addr_entry::AddrEntry = builder.object("hexdump_line_pitch").unwrap();
        let hexdump_gutter_pitch_entry: addr_entry::AddrEntry = builder.object("hexdump_gutter_pitch").unwrap();
        let bindump_line_pitch_entry: addr_entry::AddrEntry = builder.object("bindump_line_pitch").unwrap();
        let bindump_word_size_entry: addr_entry::AddrEntry = builder.object("bindump_word_size").unwrap();
        let locked: gtk::CheckButton = builder.object("locked").unwrap();
        let path_display: gtk::Entry = builder.object("path_display").unwrap();

        let pe = PropsEditor {
            toplevel,
            name_entry,
            size_entry,
            
            title_display,
            title_display_switcher,
            children_display,
            children_display_switcher,
            content_display,
            content_display_switcher,
            
            hexdump_line_pitch_entry,
            hexdump_gutter_pitch_entry,
            bindump_line_pitch_entry,
            bindump_word_size_entry,

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

        pe.name_entry.connect_activate(clone!(#[weak] pe, move |entry| catch_panic! {
            pe.select_successor();
            entry.grab_focus();
        }));

        pe.size_entry.connect_activate(clone!(#[weak] pe, move |entry| catch_panic! {
            let size = match entry.addr() {
                Ok(size) => size,
                Err(_) => { /* TODO: make a sound or something? */ return },
            };

            match pe.apply_size(size) {
                Ok(_) => { /* yay! */ },
                Err(_) => { /* TODO: make a sound or something? */ return },
            }
        }));
        
        pe.title_display.connect_visible_child_name_notify(clone!(#[weak] pe, move |st| catch_panic! {
            pe.apply_props(structure::MaybeProperties::new_title_display(match st.visible_child_name() {
                Some(x) if x == "inline" => structure::TitleDisplay::Inline,
                Some(x) if x == "major" => structure::TitleDisplay::Major,
                Some(x) if x == "minor" => structure::TitleDisplay::Minor,
                _ => return,
            }));
        }));

        pe.children_display.connect_visible_child_name_notify(clone!(#[weak] pe, move |st| catch_panic! {
            pe.apply_props(structure::MaybeProperties::new_children_display(match st.visible_child_name() {
                Some(x) if x == "full" => structure::ChildrenDisplay::Full,
                Some(x) if x == "summary" => structure::ChildrenDisplay::Summary,
                _ => return,
            }));
        }));

        pe.content_display.connect_visible_child_name_notify(clone!(#[weak] pe, move |_| catch_panic! {
            pe.apply_content_display()
        }));

        pe.hexdump_line_pitch_entry.set_addr(16.into());
        pe.hexdump_line_pitch_entry.connect_activate(clone!(#[weak] pe, move |_| catch_panic! {
            pe.apply_content_display();
        }));

        pe.hexdump_gutter_pitch_entry.set_addr(8.into());
        pe.hexdump_gutter_pitch_entry.connect_activate(clone!(#[weak] pe, move |_| catch_panic! {
            pe.apply_content_display();
        }));

        pe.bindump_line_pitch_entry.set_addr(4.into());
        pe.bindump_line_pitch_entry.connect_activate(clone!(#[weak] pe, move |_| catch_panic! {
            pe.apply_content_display();
        }));

        pe.bindump_word_size_entry.set_addr(1.into());
        pe.bindump_word_size_entry.connect_activate(clone!(#[weak] pe, move |_| catch_panic! {
            pe.apply_content_display();
        }));
        
        pe
    }

    pub fn bind_window(&self, window: &Rc<window::CharmWindow>) {
        *self.window.borrow_mut() = Rc::downgrade(window);
    }

    fn apply_content_display(&self) {
        self.apply_props(structure::MaybeProperties::new_content_display(match self.content_display.visible_child_name() {
            Some(x) if x == "none" => structure::ContentDisplay::None,
            Some(x) if x == "hexdump" => structure::ContentDisplay::Hexdump {
                line_pitch: match self.hexdump_line_pitch_entry.addr() {
                    Ok(p) => p,
                    _ => return,
                },
                gutter_pitch: match self.hexdump_gutter_pitch_entry.addr() {
                    Ok(p) => p,
                    _ => return,
                },
            },
            Some(x) if x == "hexstring" => structure::ContentDisplay::Hexstring,
            Some(x) if x == "bindump" => structure::ContentDisplay::Bindump {
                line_pitch: match self.bindump_line_pitch_entry.addr() {
                    Ok(p) => p,
                    _ => return,
                },
                word_size: match self.bindump_word_size_entry.addr() {
                    Ok(p) => p,
                    _ => return,
                },
            },
            _ => return,
        }));
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
                    
                    PropsEditorMode::Single { path, props, size: _ } => {
                        props.apply_changes(prop_changes);
                        
                        interior.document_host.change(interior.selection.document.alter_node(path.clone(), props.clone()))
                    },

                    PropsEditorMode::Many { selection, props } => {
                        props.apply_changes(prop_changes.clone());

                        interior.document_host.change(interior.selection.document.alter_nodes_bulk(selection.clone(), prop_changes))
                    },
                } {
                    Ok(new_document) => {
                        self.update_document(interior, &window, new_document);
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
    
    fn apply_size(&self, size: addr::Offset) -> Result<(), ()> {
        if !self.in_update.get() {
            let mut interior_guard = self.interior.borrow_mut();
            if let Some(interior) = interior_guard.as_mut() {
                let window = match self.window.borrow().upgrade() {
                    Some(window) => window,
                    None => return Err(()),
                };
                
                match match &mut interior.mode {
                    PropsEditorMode::Deactivated => { /* silently discard this error */ return Ok(()); },
                    
                    PropsEditorMode::Single { path, .. } => {
                        interior.document_host.change(interior.selection.document.resize_node(path.clone(), size, true, false))
                    },

                    PropsEditorMode::Many { .. } => return Err(()),
                } {
                    Ok(new_document) => {
                        self.update_document(interior, &window, new_document);
                    },
                    
                    Err((error, attempted_version)) => {
                        window.report_error(error::Error {
                            while_attempting: error::Action::ResizeNodeInPropertyEditor,
                            trouble: error::Trouble::DocumentUpdateFailure {
                                error,
                                attempted_version
                            },
                            level: error::Level::Error,
                            is_bug: false,
                        });
                    },
                }

                return Ok(());
            }
        }

        /* Failed in a mysterious, internal, invisible way. */
        Err(())
    }
    
    pub fn unbind(&self) {
        self.interior.take();
        self.name_entry.set_sensitive(false);
        self.size_entry.set_sensitive(false);
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

    /* Sometimes gtk will cause us to issue two property updates in
     * quick succession without a chance for the selection to get
     * updated asynchronously and for us to pick that up, which would
     * cause the second update to get issued against an old version of
     * the selection referencing an old version of the document. We
     * can't have that, so any time we update the document, we need to
     * synchronously and forcibly update the selection to the new
     * version of the document before GTK notifies us again and we try
     * to make another change. */
    fn update_document(&self, interior: &mut PropsInterior, window: &window::CharmWindow, document: sync::Arc<document::Document>) {
        let new_selection = match interior.selection_host.change(selection::tree::Change::DocumentUpdated(document)) {
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
    }

    fn selection_to_mode(selection: &sync::Arc<selection::TreeSelection>) -> PropsEditorMode {
        if selection.any_selected() {
            if let Some(path) = selection.single_selected() {
                let (node, _addr) = selection.document.lookup_node(&path);
                PropsEditorMode::Single { path, props: node.props.clone(), size: node.size }
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
            (PropsEditorMode::Single { props: props_old, size: size_old, .. }, PropsEditorMode::Single { props: props_new, size: size_new, .. }) if props_old == props_new && size_old == size_new && !changed => {
                /* If the selection didn't change and the properties don't disagree with what we think they are, DON'T update the interactive controls. This resets text box cursor positions. */
            },
            (PropsEditorMode::Many { props: props_old, .. }, PropsEditorMode::Many { props: props_new, .. }) if props_old == props_new && !changed => {
                /* If the selection didn't change and the properties don't disagree with what we think they are, DON'T update the interactive controls. This resets text box cursor positions. */
            },
            (_, PropsEditorMode::Single { props, size, .. }) => self.update_controls(&structure::MaybeProperties::new(props.clone()), Some(*size)),
            (_, PropsEditorMode::Many { props, .. }) => self.update_controls(&props, None),
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
        
    fn update_controls(&self, props: &structure::MaybeProperties, size: Option<addr::Offset>) {
        match &props.name {
            Some(name) => self.name_entry.set_text(name),
            None => self.name_entry.set_text(""),
        }

        match size {
            Some(size) => self.size_entry.set_text(&format!("{}", size)),
            None => self.size_entry.set_text(""),
        }

        self.title_display_switcher.set_sensitive(true);
        self.title_display.set_visible_child_name(match props.title_display {
            Some(structure::TitleDisplay::Inline) => "inline",
            Some(structure::TitleDisplay::Major) => "major",
            Some(structure::TitleDisplay::Minor) => "minor",
            None => "inconsistent",
        });

        self.children_display_switcher.set_sensitive(true);
        self.children_display.set_visible_child_name(match &props.children_display {
            Some(structure::ChildrenDisplay::Full) => "full",
            Some(structure::ChildrenDisplay::Summary) => "summary",
            Some(structure::ChildrenDisplay::None) => "inconsistent",
            None => "inconsistent",
        });

        self.content_display_switcher.set_sensitive(true);
        self.content_display.set_visible_child_name(match &props.content_display {
            Some(structure::ContentDisplay::None) => "none",
            Some(structure::ContentDisplay::Hexdump { line_pitch, gutter_pitch }) => {
                self.hexdump_line_pitch_entry.set_addr(*line_pitch);
                self.hexdump_gutter_pitch_entry.set_addr(*gutter_pitch);
                
                "hexdump"
            },
            Some(structure::ContentDisplay::Hexstring) => "hexstring",
            Some(structure::ContentDisplay::Bindump { line_pitch, word_size }) => {
                self.bindump_line_pitch_entry.set_addr(*line_pitch);
                self.bindump_word_size_entry.set_addr(*word_size);
                
                "bindump"
            },
            None => "inconsistent",
        });

        match &props.locked {
            Some(x) => {
                self.locked.set_inconsistent(false);
                self.locked.set_active(*x);
            },
            None => self.locked.set_inconsistent(true),
        }
        
        self.name_entry.set_sensitive(true);
        self.size_entry.set_sensitive(size.is_some());
        self.title_display.set_sensitive(true);
        self.children_display.set_sensitive(true);
        self.content_display.set_sensitive(true);
        self.locked.set_sensitive(true);
    }

    fn deactivate_controls(&self) {
        self.name_entry.set_text("");
        self.size_entry.set_text("");
        self.title_display_switcher.set_sensitive(false);
        self.title_display.set_visible_child_name("inconsistent");
        self.children_display_switcher.set_sensitive(false);
        self.children_display.set_visible_child_name("inconsistent");
        self.content_display_switcher.set_sensitive(false);
        self.content_display.set_visible_child_name("inconsistent");
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

    fn select_successor(&self) {
        let mut interior_guard = self.interior.borrow_mut();
        if let Some(interior) = interior_guard.as_mut() {
            let window = match self.window.borrow().upgrade() {
                Some(window) => window,
                None => return,
            };
            
            match &mut interior.mode {
                PropsEditorMode::Single { path, .. } => {
                    let mut next_path = path.clone();
                    let document = interior.selection.document.clone();
                    if document.path_successor(&mut next_path) {
                        let new_selection = match interior.selection_host.change(selection::tree::Change::SetSingle(document, next_path)) {
                            Ok(new) => new,
                            Err((error, attempted_version)) => {
                                window.report_error(error::Error {
                                    while_attempting: error::Action::SelectNext,
                                    trouble: error::Trouble::TreeSelectionUpdateFailure {
                                        error,
                                        attempted_version
                                    },
                                    level: error::Level::Warning,
                                    is_bug: true,
                                });
                                return;
                            },
                        };

                        self.in_update.set(true);
                        new_selection.changes_since(&interior.selection.clone(), &mut |selection, record| self.update_selection_internal(interior, selection.clone(), Some(record)));
                        self.in_update.set(false);
                    }
                },

                _ => return,
            }
        }
    }
}
