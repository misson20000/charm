use std::sync;

use crate::widget::listing::ListingWidget;

pub fn create(rc: &sync::Arc<parking_lot::RwLock<ListingWidget>>, _: &ListingWidget) -> gio::SimpleAction {
    let action = gio::SimpleAction::new("collapse", None);
    
    let rc_clone = rc.clone();
    action.set_enabled(true);
    action.connect_activate(move |_act, _par| {
        let lw = rc_clone.read();
        let addr = lw.cursor_view.cursor.get_addr();
        let listing = lw.listing_watch.get_listing();
        let brk = listing.get_breaks().get(&addr);

        if let Some(brk) = brk {
            let mut brk = (**brk).clone();
            brk.collapsed = !brk.collapsed;

            std::mem::drop(listing);
            lw.listing_watch.insert_break(brk);
        }
    });

    action
}
