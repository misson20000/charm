use std::sync;

use gtk::prelude::*;

use crate::addr;
use crate::widget::listing::ListingWidget;

pub fn create(rc: &sync::Arc<sync::RwLock<ListingWidget>>, da: &gtk::DrawingArea) -> gio::SimpleAction {
    let goto_action = gio::SimpleAction::new("goto", None);
    let goto_dialog = gtk::Dialog::new_with_buttons::<gtk::Window>(
        Some("Go to Location"),
        None,
        gtk::DialogFlags::MODAL | gtk::DialogFlags::DESTROY_WITH_PARENT,
        &[
            ("_Cancel", gtk::ResponseType::Cancel),
            ("_Go", gtk::ResponseType::Accept),
        ]);
    goto_dialog.set_default_response(gtk::ResponseType::Accept);

    let ca = goto_dialog.get_content_area();
    let goto_entry = gtk::Entry::new();
    goto_entry.set_activates_default(true);
    ca.pack_start(&goto_entry, true, true, 0);
    ca.show_all();
    
    let rc_clone = rc.clone();
    let da_clone = da.clone();
    goto_action.set_enabled(true);
    goto_action.connect_activate(move |_act, _par| {
        goto_dialog.set_transient_for(da_clone.get_toplevel().and_then(|tl| tl.dynamic_cast::<gtk::Window>().ok()).as_ref());

        // do not hold a lock on ListingWidget here since run() blocking is a dirty lie
        goto_entry.set_text(&format!("{}", rc_clone.read().unwrap().cursor.get_addr()));
        goto_entry.grab_focus();
        
        while match goto_dialog.run() {
            gtk::ResponseType::Accept => {
                let text_gstring = goto_entry.get_text();
                let text = match text_gstring.as_ref() {
                    Some(gs) => gs.as_ref(),
                    None => ""
                };
                match addr::Address::parse(text) {
                    Ok(addr) => { rc_clone.write().unwrap().goto(addr); false },
                    Err(addr::AddressParseError::MissingBytes) => false, // user entered a blank address, just ignore
                    Err(e) => {
                        let message = match e {
                            addr::AddressParseError::MissingBytes => "Missing bytes field.", // this one shouldn't happen here... but rustc can't figure that out
                            addr::AddressParseError::MalformedBytes(_) => "Failed to parse bytes.",
                            addr::AddressParseError::MalformedBits(_) => "Failed to parse bits.",
                            addr::AddressParseError::TooManyBits => "Invalid amount of bits."
                        }; true
                    }
                }
            }
            _ => false
        } {}
        goto_dialog.hide();
        goto_dialog.set_transient_for::<gtk::Window>(None);
    });

    goto_action
}
