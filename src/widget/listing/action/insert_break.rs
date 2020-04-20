use std::rc;
use std::sync;

use gtk::prelude::*;

use crate::addr;
use crate::listing;
use crate::listing::BreakMapExt;
use crate::listing::brk;
use crate::widget::listing::ListingWidget;

struct InsertBreakAction {
    dialog: gtk::Dialog,
    lw: sync::Arc<parking_lot::RwLock<ListingWidget>>,
    listing: sync::Arc<listing::Listing>,
    da: gtk::DrawingArea, // TODO: weak-ref
    error_label: gtk::Label,
    label_entry: gtk::Entry,
    addr_entry: gtk::Entry
}

pub fn create(rc: &sync::Arc<parking_lot::RwLock<ListingWidget>>, listing: &sync::Arc<listing::Listing>, da: &gtk::DrawingArea) -> gio::SimpleAction {
    let action = gio::SimpleAction::new("insert_break", None);
    let dialog = gtk::Dialog::new_with_buttons::<gtk::Window>(
        Some("Insert Break"),
        None,
        gtk::DialogFlags::MODAL | gtk::DialogFlags::DESTROY_WITH_PARENT,
        &[
            ("_Cancel", gtk::ResponseType::Cancel),
            ("_Delete", gtk::ResponseType::Other(1)),
            ("_Edit", gtk::ResponseType::Other(2)),
            ("_Insert", gtk::ResponseType::Other(3)),
        ]);

    let ca = dialog.get_content_area();
    let grid = gtk::Grid::new();

    let error_label = gtk::Label::new(None);
    ca.pack_start(&error_label, false, false, 0);

    let label_entry = gtk::Entry::new();
    label_entry.set_activates_default(true);
    grid.attach(&gtk::Label::new(Some("Label")), 0, 0, 1, 1);
    grid.attach(&label_entry, 1, 0, 1, 1);
    
    let addr_entry = gtk::Entry::new();
    addr_entry.set_activates_default(true);
    grid.attach(&gtk::Label::new(Some("Address")), 0, 1, 1, 1);
    grid.attach(&addr_entry, 1, 1, 1, 1);

    let iba = rc::Rc::new(InsertBreakAction {
        dialog,
        lw: rc.clone(),
        listing: listing.clone(),
        da: da.clone(),
        error_label,
        label_entry,
        addr_entry
    });
    
    {
        let iba_clone = iba.clone();
        iba.addr_entry.connect_changed(move |_entry| {
            iba_clone.update_dialog_buttons();
        });
    }
    
    ca.pack_start(&grid, true, true, 0);
    ca.show_all();

    action.connect_activate(move |_act, _par| {
        iba.activate();
    });

    action.set_enabled(true);
        
    action
}

impl InsertBreakAction {
    fn update_dialog_buttons(&self) {
        let addr_text_gstring = self.addr_entry.get_text();
        let addr_text = match addr_text_gstring.as_ref() {
            Some(gs) => gs.as_ref(),
            None => ""
        };

        match addr::Address::parse(addr_text) {
            Ok(addr) => {
                match self.listing.get_break_map().break_at(addr) {
                    brk if brk.addr == addr => {
                        self.dialog.get_widget_for_response(gtk::ResponseType::Other(1)).map(|w| w.set_sensitive(addr != addr::unit::NULL)); // delete
                        self.dialog.get_widget_for_response(gtk::ResponseType::Other(2)).map(|w| w.set_sensitive(true)); // edit
                        self.dialog.get_widget_for_response(gtk::ResponseType::Other(3)).map(|w| w.set_sensitive(false)); // insert
                        self.dialog.set_default_response(gtk::ResponseType::Other(2)); // edit
                    },
                    _ => {
                        self.dialog.get_widget_for_response(gtk::ResponseType::Other(1)).map(|w| w.set_sensitive(false)); // delete
                        self.dialog.get_widget_for_response(gtk::ResponseType::Other(2)).map(|w| w.set_sensitive(false)); // edit
                        self.dialog.get_widget_for_response(gtk::ResponseType::Other(3)).map(|w| w.set_sensitive(true)); // insert
                        self.dialog.set_default_response(gtk::ResponseType::Other(3)); // insert
                    }
                }
            },
            _ => {
                self.dialog.get_widget_for_response(gtk::ResponseType::Other(1)).map(|w| w.set_sensitive(false)); // delete
                self.dialog.get_widget_for_response(gtk::ResponseType::Other(2)).map(|w| w.set_sensitive(false)); // edit
                self.dialog.get_widget_for_response(gtk::ResponseType::Other(3)).map(|w| w.set_sensitive(false)); // insert
                self.dialog.set_default_response(gtk::ResponseType::Cancel);
            }
        }
    }

    fn activate(&self) {
        self.dialog.set_transient_for(self.da.get_toplevel().and_then(|tl| tl.dynamic_cast::<gtk::Window>().ok()).as_ref());

        self.label_entry.grab_focus();
        self.error_label.set_text("");

        self.addr_entry.set_text(&format!("{}", self.lw.read().cursor_view.cursor.get_addr()));
        
        loop {
            let response = self.dialog.run();
            let addr_text_gstring = self.addr_entry.get_text();
            let addr_text = match addr_text_gstring.as_ref() {
                Some(gs) => gs.as_ref(),
                None => ""
            };

            let message = match response {
                gtk::ResponseType::Other(2) | gtk::ResponseType::Other(3) => match addr::Address::parse(addr_text) { // edit or insert
                    Ok(addr) => {
                        self.listing.insert_break(brk::Break {
                            addr,
                            label: self.label_entry.get_text().and_then(|t| if t.len() == 0 { None } else { Some(t.to_string()) }),
                            class: brk::BreakClass::Hex(brk::hex::HexBreak {
                                line_size: addr::Size::from(16),
                            }),
                        }); break
                    },
                    Err(addr::AddressParseError::MissingBytes) => break, // user entered a blank address, just exit out
                    Err(addr::AddressParseError::MalformedBytes(_)) => "Failed to parse bytes.",
                    Err(addr::AddressParseError::MalformedBits(_)) => "Failed to parse bits.",
                    Err(addr::AddressParseError::TooManyBits) => "Invalid amount of bits.",
                }
                gtk::ResponseType::Other(1) => match addr::Address::parse(addr_text) { // delete
                    Ok(addr) => match self.listing.delete_break(&addr) {
                        Ok(_) => break,
                        Err(listing::BreakDeletionError::IsZeroBreak) => "Can't delete the zero break.",
                        Err(listing::BreakDeletionError::NotFound) => "Not found.",
                    }
                    Err(addr::AddressParseError::MissingBytes) => break, // user entered a blank address, just exit out
                    Err(addr::AddressParseError::MalformedBytes(_)) => "Failed to parse bytes.",
                    Err(addr::AddressParseError::MalformedBits(_)) => "Failed to parse bits.",
                    Err(addr::AddressParseError::TooManyBits) => "Invalid amount of bits.",
                }
                _ => break // what?
            };
            self.error_label.set_text(message);
        }
        self.dialog.hide();
        self.dialog.set_transient_for::<gtk::Window>(None);
    }
}
