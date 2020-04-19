use std::sync;

use gtk::prelude::*;

use crate::addr;
use crate::listing;
use crate::listing::BreakMapExt;
use crate::listing::brk;
use crate::widget::listing::ListingWidget;

pub fn create(listing: sync::Arc<listing::Listing>, da: &gtk::DrawingArea) -> gio::SimpleAction {
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

    {
        let listing_clone = listing.clone();
        let dialog_clone = dialog.clone();
        addr_entry.connect_changed(move |entry| {
            let addr_text_gstring = entry.get_text();
            let addr_text = match addr_text_gstring.as_ref() {
                Some(gs) => gs.as_ref(),
                None => ""
            };

            match addr::Address::parse(addr_text) {
                Ok(addr) => {
                    match listing_clone.get_break_map().break_at(addr) {
                        brk if brk.addr == addr => {
                            dialog_clone.get_widget_for_response(gtk::ResponseType::Other(1)).map(|w| w.set_sensitive(addr != addr::unit::NULL)); // delete
                            dialog_clone.get_widget_for_response(gtk::ResponseType::Other(2)).map(|w| w.set_sensitive(true)); // edit
                            dialog_clone.get_widget_for_response(gtk::ResponseType::Other(3)).map(|w| w.set_sensitive(false)); // insert
                            dialog_clone.set_default_response(gtk::ResponseType::Other(2)); // edit
                        },
                        _ => {
                            dialog_clone.get_widget_for_response(gtk::ResponseType::Other(1)).map(|w| w.set_sensitive(false)); // delete
                            dialog_clone.get_widget_for_response(gtk::ResponseType::Other(2)).map(|w| w.set_sensitive(false)); // edit
                            dialog_clone.get_widget_for_response(gtk::ResponseType::Other(3)).map(|w| w.set_sensitive(true)); // insert
                            dialog_clone.set_default_response(gtk::ResponseType::Other(3)); // insert
                        }
                    }
                },
                _ => {
                    dialog_clone.get_widget_for_response(gtk::ResponseType::Other(1)).map(|w| w.set_sensitive(false)); // delete
                    dialog_clone.get_widget_for_response(gtk::ResponseType::Other(2)).map(|w| w.set_sensitive(false)); // edit
                    dialog_clone.get_widget_for_response(gtk::ResponseType::Other(3)).map(|w| w.set_sensitive(false)); // insert
                    dialog_clone.set_default_response(gtk::ResponseType::Cancel);
                }
            }
        });
    }
    
    ca.pack_start(&grid, true, true, 0);
    ca.show_all();

    let da_clone = da.clone();
    action.connect_activate(move |_act, _par| {
        dialog.set_transient_for(da_clone.get_toplevel().and_then(|tl| tl.dynamic_cast::<gtk::Window>().ok()).as_ref());

        label_entry.grab_focus();
        error_label.set_text("");

        // TODO: populate address entry from cursor
        
        let mut message = None;
        while {
            let response = dialog.run();
            let addr_text_gstring = addr_entry.get_text();
            let addr_text = match addr_text_gstring.as_ref() {
                Some(gs) => gs.as_ref(),
                None => ""
            };

            match response {
                gtk::ResponseType::Other(2) | gtk::ResponseType::Other(3) => { // Edit or Insert
                    match addr::Address::parse(addr_text) {
                        Ok(addr) => {
                            listing.insert_break(brk::Break {
                                addr,
                                label: label_entry.get_text().and_then(|t| if t.len() == 0 { None } else { Some(t.to_string()) }),
                                class: brk::BreakClass::Hex(brk::hex::HexBreak {
                                    line_size: addr::Size::from(16),
                                }),
                            }); false
                        },
                        Err(addr::AddressParseError::MissingBytes) => false, // user entered a blank address, just exit out
                        Err(e) => {
                            message = Some(match e {
                                addr::AddressParseError::MissingBytes => "Missing bytes field.", // this one shouldn't happen here... but rustc can't figure that out
                                addr::AddressParseError::MalformedBytes(_) => "Failed to parse bytes.",
                                addr::AddressParseError::MalformedBits(_) => "Failed to parse bits.",
                                addr::AddressParseError::TooManyBits => "Invalid amount of bits."
                            }); true
                        }
                    }
                }
                gtk::ResponseType::Other(1) => { // Delete
                    match addr::Address::parse(addr_text) {
                        Ok(addr) => match listing.delete_break(&addr) {
                            Ok(_) => false,
                            Err(listing::BreakDeletionError::IsZeroBreak) => { message = Some("Can't delete the zero break."); true },
                            Err(listing::BreakDeletionError::NotFound) => { message = Some("Not found."); true },
                        }
                        Err(addr::AddressParseError::MissingBytes) => false, // user entered a blank address, just exit out
                        Err(e) => {
                            message = Some(match e {
                                addr::AddressParseError::MissingBytes => "Missing bytes field.", // this one shouldn't happen here... but rustc can't figure that out
                                addr::AddressParseError::MalformedBytes(_) => "Failed to parse bytes.",
                                addr::AddressParseError::MalformedBits(_) => "Failed to parse bits.",
                                addr::AddressParseError::TooManyBits => "Invalid amount of bits."
                            }); true
                        }
                    }
                }
                _ => false
            }
        } {
            error_label.set_text(message.take().unwrap_or(""))
        }
        dialog.hide();
        dialog.set_transient_for::<gtk::Window>(None);
    });

    action.set_enabled(true);
        
    action
}
