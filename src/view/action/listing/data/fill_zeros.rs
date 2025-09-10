use std::cell;
use std::rc;
use std::sync;

use crate::catch_panic;
use crate::model::document;
use crate::model::selection;
use crate::view::error;
use crate::view::helpers;
use crate::view::listing;
use crate::view::window;
use crate::view::window::ErrorReporter; 

use gtk::prelude::*;
use gtk::glib;
use gtk::glib::clone;
use gtk::gio;

struct FillZerosAction {
    lw: listing::ListingWidget,
    document_host: sync::Arc<document::DocumentHost>,
    selection_host: sync::Arc<selection::listing::Host>,
    selection: cell::RefCell<sync::Arc<selection::ListingSelection>>,
    subscriber: helpers::AsyncSubscriber,

    gio_action: glib::WeakRef<gio::SimpleAction>,
}

pub fn add_action(window_context: &window::WindowContext) {
    let gio_action = gio::SimpleAction::new("fill_zeros", None);
    let window = window_context.window.clone();
    
    let action = FillZerosAction::new(window_context, gio_action.clone());
    action.update_is_enabled();

    gio_action.connect_activate(clone!(#[strong] window, #[strong] action, move |_, _| catch_panic! {
        let Some(window) = window.upgrade() else { return };
        
        action.activate(&*window);
    }));

    window_context.action_group.add_action(&gio_action);
}

impl FillZerosAction {
    fn new(window_context: &window::WindowContext, gio_action: gio::SimpleAction) -> rc::Rc<Self> {
        let document_host = window_context.project.document_host.clone();
        let selection_host = window_context.listing_selection_host.clone();
        let selection = selection_host.get();

        rc::Rc::new_cyclic(|weak: &rc::Weak<Self>| Self {
            subscriber: helpers::subscribe_to_updates(weak.clone(), selection_host.clone(), selection.clone(), |action, new_sel| {
                action.selection_updated(new_sel);
            }),

            lw: window_context.lw.clone(),
            document_host,
            selection_host,
            selection: cell::RefCell::new(selection),
            gio_action: gio_action.downgrade(),
        })
    }

    fn do_fill(&self) -> Result<(), error::Trouble> {
        let selection = self.selection.borrow();
        let extent = match selection.mode {
            selection::listing::Mode::Address(extent) => extent,
            _ => return Err(error::Trouble::UnsupportedListingSelectionMode {
                selection: selection.clone()
            })
        };
        
        self.document_host.change(selection.document.fill_with_zeros(
            extent.begin.truncate_or_else(|| error::Trouble::Other("Fill range must be byte-aligned".to_string()))?,
            extent.len().truncate_or_else(|| error::Trouble::Other("Fill range must be byte-aligned".to_string()))?,
        ))?;
        
        Ok(())
    }
    
    fn activate(&self, window: &window::CharmWindow) {
        if let Err(trouble) = self.do_fill() {
            window.report_error(error::Error {
                while_attempting: error::Action::FillZeros,
                trouble,
                level: error::Level::Error,
                is_bug: false,
            });
        }
    }

    fn update_is_enabled(&self) {
        if let Some(gio_action) = self.gio_action.upgrade() {
            gio_action.set_enabled(self.selection.borrow().is_address());
        }
    }

    fn selection_updated(&self, new_sel: &sync::Arc<selection::ListingSelection>) {
        *self.selection.borrow_mut() = new_sel.clone();
        self.update_is_enabled();        
    }
}
