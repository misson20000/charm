use std::rc;
use std::sync;

use gtk::prelude::*;

use crate::addr;
use crate::listing::cursor;
use crate::widget::listing::ListingWidget;

struct GoToAction {
    dialog: gtk::Dialog,
    lw: sync::Arc<parking_lot::RwLock<ListingWidget>>,
    da: gtk::DrawingArea, // TODO weak-ref
    error_label: gtk::Label,
    goto_entry: gtk::Entry
}

pub fn create(rc: &sync::Arc<parking_lot::RwLock<ListingWidget>>, lw: &ListingWidget) -> gio::SimpleAction {
    let action = gio::SimpleAction::new("goto", None);
    let dialog = gtk::Dialog::new_with_buttons::<gtk::Window>(
        Some("Go to Location"),
        None,
        gtk::DialogFlags::MODAL | gtk::DialogFlags::DESTROY_WITH_PARENT,
        &[
            ("_Cancel", gtk::ResponseType::Cancel),
            ("_Go", gtk::ResponseType::Accept),
        ]);
    dialog.set_default_response(gtk::ResponseType::Accept);

    let ca = dialog.get_content_area();
    
    let error_label = gtk::Label::new(None);
    ca.pack_start(&error_label, false, false, 0);
    
    let goto_entry = gtk::Entry::new();
    goto_entry.set_activates_default(true);
    ca.pack_start(&goto_entry, true, true, 0);
    
    ca.show_all();

    let gta = rc::Rc::new(GoToAction {
        dialog,
        lw: rc.clone(),
        da: (*lw.da).clone(),
        error_label,
        goto_entry,
    });
    
    action.set_enabled(true);
    action.connect_activate(move |_act, _par| {
        gta.activate();
    });

    action
}

impl GoToAction {
    fn activate(&self) {
        self.dialog.set_transient_for(self.da.get_toplevel().and_then(|tl| tl.dynamic_cast::<gtk::Window>().ok()).as_ref());

        self.goto_entry.set_text(&format!("{}", self.lw.read().cursor_view.cursor.get_addr()));
        self.goto_entry.grab_focus();
        
        loop {
            let response = self.dialog.run();
            let text_gstring = self.goto_entry.get_text();
            let text = match text_gstring.as_ref() {
                Some(gs) => gs.as_ref(),
                None => ""
            };
            
            let message = match response {
                gtk::ResponseType::Accept => {
                    match addr::Address::parse(text) {
                        Ok(addr) => match self.lw.write().goto(addr) {
                            Ok(_) => break,
                            Err(cursor::PlacementFailure::HitBottomOfAddressSpace) => "Hit end of address space while placing cursor.",
                        },
                        Err(addr::AddressParseError::MissingBytes) => break, /* user entered a blank address, just ignore */
                        Err(addr::AddressParseError::MalformedBytes(_)) => "Failed to parse bytes.",
                        Err(addr::AddressParseError::MalformedBits(_)) => "Failed to parse bits.",
                        Err(addr::AddressParseError::TooManyBits) => "Invalid amount of bits.",
                    }
                }
                _ => break
            };
            self.error_label.set_text(message);
        }
        self.dialog.hide();
        self.dialog.set_transient_for::<gtk::Window>(None);
    }
}
