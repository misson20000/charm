use std::cell;
use std::rc;
use std::sync;

use crate::catch_panic;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::token;
use crate::model::listing::token::AsTokenRef;
use crate::model::listing::token::TokenKind;
use crate::model::selection;
use crate::view::helpers;
use crate::view::props_editor;
use crate::view::window;

use gtk::prelude::*;
use gtk::glib;
use gtk::glib::clone;
use gtk::gio;

struct EditPropsAction {
    document_host: sync::Arc<document::DocumentHost>,
    pe: rc::Rc<props_editor::PropsEditor>,
    dialog: gtk::ApplicationWindow,
    
    selection: cell::RefCell<selection::TreeSelection>,
    selection_subscriber: helpers::AsyncSubscriber,

    cursor_token: cell::RefCell<(sync::Arc<document::Document>, token::Token)>,
    cursor_subscriber: helpers::AsyncSubscriber,
    
    action: glib::WeakRef<gio::SimpleAction>,
}

struct EditPropsActivation {
    document: sync::Arc<document::Document>,
    path: structure::Path,
}

pub fn add_action(window_context: &window::WindowContext) {
    let edit_props_gio_action = gio::SimpleAction::new("edit_props", None);
    let edit_props_action = EditPropsAction::new(window_context, edit_props_gio_action.clone());
    edit_props_action.update_is_enabled();

    edit_props_gio_action.connect_activate(move |_, _| catch_panic! {
        edit_props_action.activate();
    });
    
    window_context.action_group.add_action(&edit_props_gio_action);
}

impl EditPropsAction {
    fn new(window_context: &window::WindowContext, action: gio::SimpleAction) -> rc::Rc<Self> {
        let document_host = window_context.project.document_host.clone();
        let selection_host = window_context.listing_selection_host.clone();
        let selection = selection_host.get();

        let mut token_watch = window_context.lw.cursor_token_watch();
        let token = token_watch.borrow().clone();

        let pe = props_editor::PropsEditor::new();
        pe.bind_window(&window_context.window.upgrade().unwrap());
        
        let dialog = gtk::ApplicationWindow::builder()
            .application(&window_context.window.upgrade().unwrap().application.application)
            .child(&pe.toplevel)
            .resizable(true)
            .title("Property Editor")
            .transient_for(&window_context.window.upgrade().unwrap().window)
            .hide_on_close(true)
            .destroy_with_parent(true)
            .default_widget(&pe.name_entry)
            .build();
        
        let action = rc::Rc::new_cyclic(|weak: &rc::Weak<Self>| {
            
            let weak_for_cursor_task = weak.clone();
            Self {
                dialog,
                pe,
                
                selection: cell::RefCell::new(selection.to_tree()),
                selection_subscriber: helpers::subscribe_to_updates(weak.clone(), selection_host.clone(), selection.clone(), |action, new_sel| {
                    action.selection_updated(new_sel);
                }),

                cursor_token: cell::RefCell::new(token),
                cursor_subscriber: helpers::spawn_on_main_context(async move {
                    while let Ok(_) = token_watch.changed().await {
                        let Some(action) = weak_for_cursor_task.upgrade() else { break };

                        action.cursor_token_updated(token_watch.borrow_and_update().clone());
                        action.update_is_enabled();
                    }
                }),

                document_host,
                action: action.downgrade(),
            }
        });

        action.dialog.connect_close_request(clone!(#[weak] action, #[upgrade_or] glib::Propagation::Proceed, move |_| {
            catch_panic! {
                action.pe.unbind();
            }
            glib::Propagation::Proceed
        }));

        helpers::bind_simple_action(&action, &action.dialog, "cancel", |action| {
            action.deactivate();
        });
        
        action.pe.name_entry.connect_activate(clone!(#[weak] action, move |_| catch_panic! {
            action.deactivate();
        }));
        
        action
    }

    fn selection_updated(&self, new_sel: &sync::Arc<selection::ListingSelection>) {
        self.selection.replace(new_sel.to_tree());
        self.update_is_enabled();
    }

    fn cursor_token_updated(&self, new_token: (sync::Arc<document::Document>, token::Token)) {
        self.cursor_token.replace(new_token);
        self.update_is_enabled();
    }

    fn cursor_on_title(&self) -> bool {
        self.cursor_token.borrow().1.as_specific_token_ref(token::Title::MARKER).is_some()
    }
    
    fn update_is_enabled(&self) {
        if let Some(action) = self.action.upgrade() {
            action.set_enabled(self.selection.borrow().any_selected() || self.cursor_on_title());
        }
    }

    fn activate(&self) {
        let cursor_token = self.cursor_token.borrow();
        
        if self.selection.borrow().any_selected() {
            self.pe.bind_independent(self.document_host.clone(), sync::Arc::new(self.selection.borrow().clone()));
            self.dialog.present();
        } else if let Some(title) = cursor_token.1.as_specific_token_ref(token::Title::MARKER) {
            let mut selection = selection::TreeSelection::new(cursor_token.0.clone());
            selection.add_single(title.node_path());
            self.pe.bind_independent(self.document_host.clone(), sync::Arc::new(selection));
            self.dialog.present();
        }
    }

    fn deactivate(&self) {
        self.pe.unbind();
        self.dialog.hide();
    }
}

impl Drop for EditPropsAction {
    fn drop(&mut self) {
        self.dialog.destroy();
    }
}
