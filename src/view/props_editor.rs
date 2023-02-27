use std::cell;
use std::rc::Rc;
use std::sync;

use gtk::prelude::*;

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
    children_display: gtk::ComboBox,
    path_display: gtk::Entry,

    interior: cell::RefCell<Option<PropsInterior>>,
}

impl PropsEditor {
    pub fn new() -> Rc<PropsEditor> {
        let builder = gtk::Builder::from_string(include_str!("props-editor.ui"));

        let toplevel: gtk::Widget = builder.object("toplevel").unwrap();
        let name_entry: gtk::Entry = builder.object("name_entry").unwrap();
        let children_display: gtk::ComboBox = builder.object("children_display").unwrap();
        let path_display: gtk::Entry = builder.object("path_display").unwrap();

        let pe = PropsEditor {
            toplevel,
            name_entry,
            children_display,
            path_display,
            interior: cell::RefCell::new(None),
        };
        
        pe.unbind();

        Rc::new(pe)
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
        let mut interior_guard = self.interior.borrow_mut();
        if let Some(interior) = interior_guard.as_mut() {
            selection.changes_since(&interior.selection.clone(), &mut |selection, record| self.selection_updated(interior, selection.clone(), record.selection_changed));
        }
    }

    fn selection_updated(&self, interior: &mut PropsInterior, selection: sync::Arc<selection::Selection>, changed: bool) {
        let path = match &selection.mode {
            selection::Mode::Single(path) => Some(path),
            _ => None
        };
        
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

        interior.selection = selection;
    }

    fn update_controls(&self, props: Option<&structure::Properties>) {
        if let Some(props) = props {
            self.name_entry.set_text(&props.name);
            
            self.name_entry.set_sensitive(true);
            self.children_display.set_sensitive(true);
        } else {
            self.name_entry.set_text("");
            
            self.name_entry.set_sensitive(false);
            self.children_display.set_sensitive(false);
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
