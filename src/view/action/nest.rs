use std::rc;
use std::sync;

use gtk::prelude::*;
use gtk::glib;
use gtk::glib::clone;
use gtk::gio;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::view::helpers;
use crate::view::hierarchy;
use crate::view::window;

struct NestAction {
    document_host: sync::Arc<document::DocumentHost>,
    hierarchy_model: hierarchy::StructureSelectionModel
}

pub fn create_action(window_context: &window::WindowContext) -> gio::SimpleAction {
    let action_impl = rc::Rc::new(NestAction {
        document_host: window_context.document_host.clone(),
        hierarchy_model: window_context.hierarchy_model.clone(),
    });
    
    let action = helpers::create_simple_action_strong(action_impl, "nest", |action| action.activate());

    window_context.hierarchy_model.connect_selection_changed(clone!(@weak action => move |hierarchy_model, _pos, _n_items| {
        update_enabled(&action, &hierarchy_model);
    }));

    update_enabled(&action, &window_context.hierarchy_model);
    
    action
}

fn update_enabled(action: &gio::SimpleAction, hierarchy_model: &hierarchy::StructureSelectionModel) {
    action.set_enabled(match hierarchy_model.selection_mode().0 {
        hierarchy::SelectionMode::Empty => false,
        hierarchy::SelectionMode::Single(path) => !path.is_empty(),
        hierarchy::SelectionMode::SiblingRange(_, _, _) => true,
        hierarchy::SelectionMode::All => false,
    });
}

impl NestAction {    
    fn activate(&self) {
        let (selection, document) = self.hierarchy_model.selection_mode();

        let (parent, first_sibling, last_sibling) = match &selection {
            hierarchy::SelectionMode::Empty => return,
            hierarchy::SelectionMode::Single(path) if !path.is_empty() => (&path[0..path.len()-1], *path.last().unwrap(), *path.last().unwrap()),
            hierarchy::SelectionMode::SiblingRange(path, begin, end) => (&path[..], *begin, *end),
            hierarchy::SelectionMode::All | hierarchy::SelectionMode::Single(_) => {
                // TODO: find a way to issue a warning for this
                return;
            }
        };

        // TODO: failure feedback
        let _ = self.document_host.nest(&document, parent.to_vec(), first_sibling, last_sibling, structure::Properties {
            name: "nest".to_string(),
            title_display: structure::TitleDisplay::Minor,
            children_display: structure::ChildrenDisplay::Full,
            content_display: structure::ContentDisplay::Hexdump(addr::Size::from(16)),
            locked: false,
        });
    }
}
