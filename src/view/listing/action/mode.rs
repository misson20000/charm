use std::sync;

use crate::view::listing::ListingWidget;
use crate::view::listing::component;

use gtk::gio;
use gtk::glib;
use gtk::prelude::*;

pub fn create_mode(rc: &sync::Arc<parking_lot::RwLock<ListingWidget>>, _lw: &ListingWidget) -> gio::SimpleAction {
    let action = gio::SimpleAction::new_stateful("mode", Some(glib::VariantTy::new("s").unwrap()), &"command".to_variant());

    /* some examples of modes we might have:
     * - command
     * - entry
     * - text entry
     */
    
    let rc_clone = rc.clone();
    action.set_enabled(true);
    
    action.connect_change_state(move |act, par| {
        let mode = match par.and_then(|v| v.str()) {
            Some("command") => component::cursor::Mode::Command,
            Some("entry") => component::cursor::Mode::Entry,
            Some("utf8") => component::cursor::Mode::TextEntry,
            _ => return
        };

        rc_clone.write().cursor_view.change_mode(mode);

        if let Some(v) = par {
            act.set_state(v);
        }
    });

    action
}

pub fn create_insert_mode(rc: &sync::Arc<parking_lot::RwLock<ListingWidget>>, _lw: &ListingWidget) -> gio::SimpleAction {
    let action = gio::SimpleAction::new_stateful("insert_mode", None, &false.to_variant());

    let rc_clone = rc.clone();
    action.set_enabled(true);
    
    action.connect_change_state(move |act, par| {
        if let Some(insert) = par.and_then(|v| v.get::<bool>()) {
            rc_clone.write().cursor_view.change_insert(insert);
        };

        if let Some(v) = par {
            act.set_state(v);
        }
    });

    action
}
