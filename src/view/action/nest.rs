use std::cell;
use std::rc;
use std::sync;

use gtk::glib;
use gtk::glib::clone;
use gtk::gio;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::selection;
use crate::model::versioned::Versioned;
use crate::view::helpers;
use crate::view::window;

struct NestAction {
    document_host: sync::Arc<document::DocumentHost>,
    selection_host: sync::Arc<selection::Host>,
    selection: cell::RefCell<sync::Arc<selection::Selection>>,
    window: rc::Weak<window::CharmWindow>,
    
    subscriber: once_cell::unsync::OnceCell<helpers::AsyncSubscriber>,
}

pub fn create_action(window_context: &window::WindowContext) -> gio::SimpleAction {
    let selection = window_context.selection_host.get();
    
    let action_impl = rc::Rc::new(NestAction {
        document_host: window_context.document_host.clone(),
        selection_host: window_context.selection_host.clone(),
        selection: cell::RefCell::new(selection.clone()),
        window: window_context.window.clone(),
        subscriber: Default::default(),
    });
    
    let action = helpers::create_simple_action_strong(action_impl.clone(), "nest", |action| action.activate());

    update_enabled(&action, &selection);
    
    action_impl.subscriber.set(helpers::subscribe_to_updates(rc::Rc::downgrade(&action_impl), action_impl.selection_host.clone(), selection, clone!(@weak action => move |action_impl, selection| {
        *action_impl.selection.borrow_mut() = selection.clone();
        update_enabled(&action, selection);
    }))).unwrap();
    
    action
}

fn update_enabled(action: &gio::SimpleAction, selection: &selection::Selection) {
    action.set_enabled(match &selection.mode {
        selection::Mode::Empty => false,
        selection::Mode::Single(path) => !path.is_empty(),
        selection::Mode::SiblingRange(_, _, _) => true,
        selection::Mode::All => false,
    });
}

impl NestAction {    
    fn activate(&self) {
        let selection = self.selection.borrow();

        let (parent, first_sibling, last_sibling) = match &selection.mode {
            selection::Mode::Empty => return,
            selection::Mode::Single(path) if !path.is_empty() => (&path[0..path.len()-1], *path.last().unwrap(), *path.last().unwrap()),
            selection::Mode::SiblingRange(path, begin, end) => (&path[..], *begin, *end),
            selection::Mode::All | selection::Mode::Single(_) => {
                // TODO: find a way to issue a warning for this
                return;
            }
        };

        let new_doc = match self.document_host.change(selection.document.nest(parent.to_vec(), first_sibling, last_sibling, structure::Properties {
            name: "".to_string(),
            title_display: structure::TitleDisplay::Minor,
            children_display: structure::ChildrenDisplay::Full,
            content_display: structure::ContentDisplay::Hexdump(addr::Size::from(16)),
            locked: false,
        })) {
            Ok(new_doc) => new_doc,
            Err(e) => {
                // TODO: better failure feedback
                println!("failed to change document: {:?}", e);
                return;
            }
        };

        let record = &new_doc.previous().expect("just-changed document should have a previous document and change").1;
        let nested_node_path = match record {
            document::change::Change { ty: document::change::ChangeType::Nest(parent, first_child, _, _), .. } => {
                let mut path = parent.clone();
                path.push(*first_child);
                path
            },
            _ => panic!("change was transmuted into a different type?")
        };
        
        let new_sel = match self.selection_host.change(selection::Change::SetSingle(new_doc, nested_node_path)) {
            Ok(new_sel) => new_sel,
            Err(e) => {
                // TODO: better failure feedback
                println!("failed to update selection: {:?}", e);
                return;
            }
        };

        if let Some(w) = self.window.upgrade() {
            /* Force the properties editor to update to the new selection synchronously */
            w.props_editor.selection_updated_bulk(&new_sel);
            w.props_editor.focus_name();
        }
    }
}
