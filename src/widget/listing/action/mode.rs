use std::sync;

use crate::widget::listing::ListingWidget;
use crate::widget::listing::component;

pub fn create_mode(rc: &sync::Arc<parking_lot::RwLock<ListingWidget>>, _lw: &ListingWidget) -> gio::SimpleAction {
    let action = gio::SimpleAction::new_stateful("mode", Some(glib::VariantTy::new("s").unwrap()), &glib::Variant::from("command"));

    /* some examples of modes we might have:
     * - command
     * - entry
     * - text entry
     */
    
    let rc_clone = rc.clone();
    action.set_enabled(true);
    
    action.connect_change_state(move |act, par| {
        let mode = match par.and_then(|v| v.get_str()) {
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
