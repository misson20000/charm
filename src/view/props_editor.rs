use std::cell;
use std::rc::Rc;
use std::sync;

use gtk::prelude::*;
//use gtk::glib;
//use gtk::glib::clone;

use crate::model::document;
//use crate::model::document::structure;
use crate::model::selection;
use crate::view::window;

struct PropsInterior {
    document_host: sync::Arc<document::DocumentHost>,
    selection_host: sync::Arc<selection::Host>,
    selection: sync::Arc<selection::Selection>,

    /* TODO: subscriber */
}

pub struct PropsEditor {
    pub toplevel: gtk::Widget,

    name_entry: gtk::Entry,
    children_display: gtk::ComboBox,

    interior: cell::RefCell<Option<PropsInterior>>,
}

impl PropsEditor {
    pub fn new() -> Rc<PropsEditor> {
        let builder = gtk::Builder::from_string(include_str!("props-editor.ui"));

        let toplevel: gtk::Widget = builder.object("toplevel").unwrap();
        let name_entry: gtk::Entry = builder.object("name_entry").unwrap();
        let children_display: gtk::ComboBox = builder.object("children_display").unwrap();

        let pe = PropsEditor {
            toplevel,
            name_entry,
            children_display,
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
        let selection = ctx.selection_host.get();
        
        *self.interior.borrow_mut() = Some(PropsInterior {
            document_host: ctx.document_host.clone(),
            selection_host: ctx.selection_host.clone(),
            selection: selection.clone(),
        });

        /*
        ctx.selection_model.connect_selection_changed(clone!(@weak self as pe => move |_hierarchy_model, _pos, _n_items| {
            pe.update_selection();
        }));

        self.update_selection();
        */
    }

    /*
    fn update_selection(&self) {
        let mut interior_guard = self.interior.borrow_mut();
        if let Some(interior) = interior_guard.as_mut() {
            let (selection_mode, document) = interior.selection.selection_mode();

            let path = match selection_mode {
                selection::Mode::Single(path) => Some(path),
                _ => None
            };

            if let Some(path) = &path {
                let (node, _addr) = document.lookup_node(&path);
                
                self.name_entry.set_text(&node.props.name);
                
                self.name_entry.set_sensitive(true);
                self.children_display.set_sensitive(true);
            } else {
                self.name_entry.set_text("");
                
                self.name_entry.set_sensitive(false);
                self.children_display.set_sensitive(false);
            }

            interior.document = document;
            interior.path = path;
        }
}
    */

    pub fn toplevel(&self) -> &gtk::Widget {
        &self.toplevel
    }
}
