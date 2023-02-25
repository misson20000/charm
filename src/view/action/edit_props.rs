use std::cell;
use std::sync;

use gtk::gio;
use gtk::glib;
use gtk::glib::clone;

use crate::model::document;
use crate::view::helpers;
use crate::view::hierarchy;
use crate::view::window;

struct EditPropsAction {
    document_host: sync::Arc<document::DocumentHost>,
    document: cell::RefCell<sync::Arc<document::Document>>,
    
    hierarchy_model: hierarchy::StructureSelectionModel,

    dialog: gtk::ApplicationWindow,
    name_entry: gtk::Entry,
    children_display: gtk::Entry,

    subscriber: once_cell::unsync::OnceCell<helpers::AsyncSubscriber>
}

pub fn create_action(window_context: &window::WindowContext) -> gio::SimpleAction {
    let builder = gtk::Builder::from_string(include_str!("../display-editor.ui"));

    let name_entry: gtk::Entry = builder.object("name_entry").unwrap();
    let children_display: gtk::Entry = builder.object("children_display").unwrap();
    
    let dialog = gtk::ApplicationWindow::builder()
        .application(&window_context.window.upgrade().unwrap().application.application)
        .child(&builder.object::<gtk::Widget>("toplevel").unwrap())
        .resizable(true)
        .title(&format!("Editing properties for '{}'", node_info.props.name))
        .transient_for(&window_context.window.upgrade().unwrap().window)
        .hide_on_close(true)
        .destroy_with_parent(true)
        .build();

    let action_impl = rc::Rc::new(EditPropsAction {
        document_host: window_context.document_host.clone(),
        hierarchy_model: window_context.hierarchy_model.clone(),

        dialog,
        name_entry,
        children_display,

        subscriber: once_cell::unsync::OnceCell::new(),
    });

    action_impl.subscriber.set(helpers::subscribe_to_document_updates(rc::Rc::downgrade(&action_impl), action_impl.document_host.clone(), action_impl.document.borrow().clone(), |action, new_document| {
        action.update(new_document);
    })).unwrap();

    let action = helpers::create_simple_action_strong(action_impl, "edit_properties", |action| action.activate());

    window_context.hierarchy_model.connect_selection_changed(clone!(@weak action, @weak action_impl => move |hierarchy_model, _pos, _n_items| {
        let path = match hierarchy_model.selection_mode().0 {
            hierarchy::SelectionMode::Empty => None,
            hierarchy::SelectionMode::Single(path) => Some(path),
            hierarchy::SelectionMode::SiblingRange(_, _, _) => None,
            hierarchy::SelectionMode::All => None,
        };

        action.set_enabled(path.is_some());
        action_impl.bind_path(path);
    }));
    
    action
}    

impl EditPropsAction {
    fn bind_item(&self, item: Option<hierarchy::NodeItem>) {
        self.name_binding().take();
        self.children_display_binding().take();

        if let Some(item) = item {
            *self.name_binding.borrow_mut() = item.bind_property("name", &self.name_entry.buffer(), "text").flags(glib::BindingFlags::BIDIRECTIONAL | glib::BindingFlags::SYNC_CREATE).build();
            *self.children_display_binding.borrow_mut() = item.bind_property("children-display", &self.children_display.buffer(), "text").flags(glib::BindingFlags::BIDIRECTIONAL | glib::BindingFlags::SYNC_CREATE).build();
            
            self.name_entry.set_sensitive(true);
            self.children_display.set_sensitive(true);
        } else {
            self.name_entry.buffer().set_text("");
            self.children_display.buffer().set_text("");
            
            self.name_entry.set_sensitive(false);
            self.children_display.set_sensitive(false);
        }
    }
    
    fn activate(&self) {
        self.bind_to_selection();
        self.dialog.show();
        
        if let Some(item) = self.hierarchy_editor.model().and_then(|model| {
            let selection = model.selection();
            if selection.size() == 0 {
                None
            } else {
                model.item(selection.nth(0))
            }
        }).map(|object| {
            object.downcast::<gtk::TreeListRow>().unwrap().item().unwrap().downcast::<hierarchy::NodeItem>().unwrap()
        }) {
            let node_info = item.imp().info.get().unwrap().borrow();
            std::mem::drop(node_info);

            self.bind_item(Some(item));
                        
            self.dialog.show();
        }
    }
}
