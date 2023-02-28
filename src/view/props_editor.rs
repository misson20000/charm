use std::cell;
use std::rc::Rc;
use std::sync;

use gtk::prelude::*;
use gtk::gio;
use gtk::glib;
use gtk::glib::clone;

use crate::model::document;
use crate::model::document::structure;
use crate::model::selection;
use crate::model::versioned::Versioned;
use crate::view::helpers;
use crate::view::window;

struct PropsInterior {
    document_host: sync::Arc<document::DocumentHost>,
    selection_host: sync::Arc<selection::Host>,
    selection: sync::Arc<selection::Selection>,

    current: Option<(structure::Path, structure::Properties)>,
    
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
        };
        
        pe.unbind();

        let pe = Rc::new(pe);

        pe.name_entry.buffer().connect_text_notify(clone!(@weak pe => move |buffer| {
            pe.change_prop(|props| props.name = buffer.text());
        }));

        pe.title_display.connect_selected_notify(clone!(@weak pe => move |dd| {
            pe.change_prop(|props| props.title_display = match dd.selected() {
                0 => structure::TitleDisplay::Inline,
                1 => structure::TitleDisplay::Major,
                2 => structure::TitleDisplay::Minor,
                x => panic!("unexpected selected index: {}", x)
            });
        }));

        pe.children_display.connect_selected_notify(clone!(@weak pe => move |dd| {
            pe.change_prop(|props| props.children_display = match dd.selected() {
                0 => structure::ChildrenDisplay::None,
                1 => structure::ChildrenDisplay::Summary,
                2 => structure::ChildrenDisplay::Full,
                x => panic!("unexpected selected index: {}", x)
            });
        }));

        pe.content_display.connect_selected_notify(clone!(@weak pe => move |dd| {
            pe.change_prop(|props| props.content_display = match dd.selected() {
                0 => structure::ContentDisplay::None,
                1 => structure::ContentDisplay::Hexdump(16.into()),
                2 => structure::ContentDisplay::Hexstring,
                x => panic!("unexpected selected index: {}", x)
            });
        }));
        
        pe
    }

    fn change_prop<F: FnOnce(&mut structure::Properties)>(&self, cb: F) {
        if !self.in_update.get() {
            let mut interior_guard = self.interior.borrow_mut();
            if let Some(interior) = interior_guard.as_mut() {
                if let Some((path, props)) = interior.current.as_mut() {
                    cb(props);
                    
                    if let Err(e) = interior.document_host.change(interior.selection.document.alter_node(path.clone(), props.clone())) {
                        println!("failed to alter node: {:?}", e);
                    }
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
        let selection_host = ctx.selection_host.clone();
        let selection = ctx.selection_host.get();

        let mut interior = PropsInterior {
            document_host: ctx.document_host.clone(),
            selection_host: selection_host.clone(),
            selection: selection.clone(),

            current: None,

            subscriber: helpers::subscribe_to_updates(Rc::downgrade(self), selection_host, selection.clone(), |pe, new_sel| pe.selection_updated_bulk(new_sel)),
        };
        
        self.selection_updated(&mut interior, selection, true);
        *self.interior.borrow_mut() = Some(interior);
    }

    fn selection_updated_bulk(&self, selection: &sync::Arc<selection::Selection>) {
        self.in_update.set(true);
        let mut interior_guard = self.interior.borrow_mut();
        if let Some(interior) = interior_guard.as_mut() {
            selection.changes_since(&interior.selection.clone(), &mut |selection, record| self.selection_updated(interior, selection.clone(), record.selection_changed));
        }
        drop(interior_guard);
        self.in_update.set(false);
    }

    fn selection_updated(&self, interior: &mut PropsInterior, selection: sync::Arc<selection::Selection>, changed: bool) {
        let path = match &selection.mode {
            selection::Mode::Single(path) => Some(path),
            _ => None
        };

        interior.selection = selection.clone();
        
        if let Some(path) = &path {
            let (node, _addr) = selection.document.lookup_node(&path);

            if changed || interior.current.as_ref().map(|(_path, props)| props) != Some(&node.props) {
                interior.current = Some(((*path).clone(), node.props.clone()));
                self.update_controls(Some(&node.props));
            } else {
                /* If the selection didn't change and the properties don't disagree with what we think they are, DON'T update the interactive controls. This resets text box cursor positions. */
            }
            self.update_path_control(&selection.document, Some(path));
        } else {
            interior.current = None;
            self.update_controls(None);
            self.update_path_control(&selection.document, None);
        }
    }

    fn update_controls(&self, props: Option<&structure::Properties>) {
        if let Some(props) = props {
            self.name_entry.set_text(&props.name);

            self.title_display.set_model(Some(&self.title_model));
            self.title_display.set_selected(match props.title_display {
                structure::TitleDisplay::Inline => 0,
                structure::TitleDisplay::Major => 1,
                structure::TitleDisplay::Minor => 2,
            });

            self.children_display.set_model(Some(&self.children_model));
            self.children_display.set_selected(match props.children_display {
                structure::ChildrenDisplay::None => 0,
                structure::ChildrenDisplay::Summary => 1,
                structure::ChildrenDisplay::Full => 2,
            });

            self.content_display.set_model(Some(&self.content_model));
            self.content_display.set_selected(match props.content_display {
                structure::ContentDisplay::None => 0,
                structure::ContentDisplay::Hexdump(_) => 1,
                structure::ContentDisplay::Hexstring => 2,
            });
            
            self.locked.set_active(props.locked);
            
            self.name_entry.set_sensitive(true);
            self.size_entry.set_sensitive(true);
            self.title_display.set_sensitive(true);
            self.children_display.set_sensitive(true);
            self.content_display.set_sensitive(true);
            self.locked.set_sensitive(true);
        } else {
            self.name_entry.set_text("");
            self.title_display.set_model(gio::ListModel::NONE);
            self.children_display.set_model(gio::ListModel::NONE);
            self.content_display.set_model(gio::ListModel::NONE);
            self.locked.set_active(false);
            
            self.name_entry.set_sensitive(false);
            self.size_entry.set_sensitive(false);
            self.title_display.set_sensitive(false);
            self.children_display.set_sensitive(false);
            self.content_display.set_sensitive(false);
            self.locked.set_sensitive(false);
        }
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
}
