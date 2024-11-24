use gtk::prelude::*;
use gtk::glib;
use gtk::glib::clone;

use std::cell;
use std::rc;
use std::sync;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::selection;
use crate::view::export;
use crate::view::helpers;
use crate::view::window;

struct ExportBinaryNodeAction {
    window: rc::Weak<window::CharmWindow>,
    document_host: sync::Arc<document::DocumentHost>,
    selection_host: sync::Arc<selection::tree::Host>,
    selection: cell::RefCell<(sync::Arc<document::Document>, Option<structure::Path>)>,
    subscriber: once_cell::unsync::OnceCell<helpers::AsyncSubscriber>,
}

pub fn add_action(window_context: &window::WindowContext) {
    let selection = window_context.tree_selection_host.get();

    /*
    let dialog = gtk::FileChooserNative::builder()
        .accept_label("Export")
        .cancel_label("Cancel")
        .title("Charm: Export Node as Raw Binary")
        .modal(true)
        .transient_for(&window.window)
        .action(gtk::FileChooserAction::Save)
        .select_multiple(false)
        .create_folders(true)
    .build();
    */
    

    let action_impl = rc::Rc::new(ExportBinaryNodeAction {
        window: window_context.window.clone(),
        document_host: window_context.project.document_host.clone(),
        selection_host: window_context.tree_selection_host.clone(),
        selection: cell::RefCell::new((selection.document.clone(), selection.single_selected())),
        subscriber: Default::default(),
    });

    let action = helpers::create_simple_action_strong(action_impl.clone(), "export_binary_node", |action| action.activate());
    action.set_enabled(action_impl.enabled());

    action_impl.subscriber.set(helpers::subscribe_to_updates(rc::Rc::downgrade(&action_impl), action_impl.selection_host.clone(), selection, clone!(#[weak] action, move |action_impl, selection| {
        *action_impl.selection.borrow_mut() = (selection.document.clone(), selection.single_selected());
        action.set_enabled(action_impl.enabled());
    }))).unwrap();

    window_context.action_group.add_action(&action);
}

impl ExportBinaryNodeAction {
    fn enabled(&self) -> bool {
        self.selection.borrow().1.is_some()
    }

    fn activate(&self) {
        let Some(window) = self.window.upgrade() else { return };
        let guard = self.selection.borrow();
        let Some(path) = guard.1.as_ref() else { return };

        let (node, addr) = guard.0.lookup_node(&path);

        let dialog = glib::Object::builder::<export::CharmExportDialog>()
            .property("application", window.application.application.clone())
            .property("transient-for", window.window.clone())
            .build();
        dialog.set(self.document_host.clone(), addr::AbsoluteExtent::sized(addr, node.size));
        dialog.show();
    }
}
