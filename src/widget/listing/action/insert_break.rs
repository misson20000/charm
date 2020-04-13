use std::sync;
use std::rc;
use std::cell;

use gtk::prelude::*;

use crate::addr;
use crate::space::edit;
use crate::widget::listing::ListingWidget;

pub fn create(rc: &sync::Arc<sync::RwLock<ListingWidget>>, da: &gtk::DrawingArea) -> gio::SimpleAction {
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

    let editing_break_rc: rc::Rc<cell::RefCell<sync::Weak<edit::Break>>> = rc::Rc::new(cell::RefCell::new(sync::Weak::new()));
    
    {
        let dialog_clone = dialog.clone();
        let editing_break_clone = editing_break_rc.clone();
        addr_entry.connect_changed(move |entry| {
            let addr_text_gstring = entry.get_text();
            let addr_text = match addr_text_gstring.as_ref() {
                Some(gs) => gs.as_ref(),
                None => ""
            };

            match (editing_break_clone.borrow().upgrade(), addr::Address::parse(addr_text)) {
                (Some(brk), Ok(addr)) if addr == brk.address => {
                    dialog_clone.get_widget_for_response(gtk::ResponseType::Other(1)).map(|w| w.set_sensitive(brk.address != addr::unit::NULL)); // delete
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
        });
    }
    
    ca.pack_start(&grid, true, true, 0);
    ca.show_all();
    
    let rc_clone = rc.clone();
    let da_clone = da.clone();
    action.set_enabled(true);
    action.connect_activate(move |_act, _par| {
        dialog.set_transient_for(da_clone.get_toplevel().and_then(|tl| tl.dynamic_cast::<gtk::Window>().ok()).as_ref());

        // do not hold a lock on ListingWidget here since run() blocking is a dirty lie
        let editing_break = {
            let lw = rc_clone.read().unwrap();
            let cursor_addr = lw.cursor.get_addr();

            match lw.editor.break_at(cursor_addr) {
                Ok(brk) => { // we're on a break
                    addr_entry.set_text(&format!("{}", brk.address));
                    label_entry.set_text(&brk.label);

                    dialog.get_widget_for_response(gtk::ResponseType::Other(1)).map(|w| { // delete
                        w.show();
                        w.set_sensitive(brk.address != addr::unit::NULL);
                    });
                    dialog.get_widget_for_response(gtk::ResponseType::Other(2)).map(|w| { w.show(); w.set_sensitive(true); }); // edit
                    dialog.get_widget_for_response(gtk::ResponseType::Other(3)).map(|w| { w.show(); w.set_sensitive(false); }); // insert
                    dialog.set_default_response(gtk::ResponseType::Other(2)); // edit
                    
                    Some(brk)
                },
                Err(_) => {
                    addr_entry.set_text(&format!("{}", cursor_addr));
                    label_entry.set_text("");

                    dialog.get_widget_for_response(gtk::ResponseType::Other(1)).map(|w| w.hide()); // delete
                    dialog.get_widget_for_response(gtk::ResponseType::Other(2)).map(|w| w.hide()); // edit
                    dialog.get_widget_for_response(gtk::ResponseType::Other(3)).map(|w| { w.show(); w.set_sensitive(true); }); // insert
                    dialog.set_default_response(gtk::ResponseType::Other(3)); // insert
                    
                    None
                },
            }
        };
        
        std::mem::replace(&mut *editing_break_rc.borrow_mut(), editing_break.as_ref().map(|b| sync::Arc::downgrade(b)).unwrap_or(sync::Weak::new()));

        label_entry.grab_focus();
        error_label.set_text("");
        
        let mut message = None;
        while match dialog.run() {
            gtk::ResponseType::Other(2) | gtk::ResponseType::Other(3) => { // Edit or Insert
                let rc = rc_clone.read().unwrap();
                
                let addr_text_gstring = addr_entry.get_text();
                let addr_text = match addr_text_gstring.as_ref() {
                    Some(gs) => gs.as_ref(),
                    None => ""
                };

                match (addr::Address::parse(addr_text), label_entry.get_text()) {
                    (Ok(addr), Some(label)) => match &editing_break {
                        Some(brk) if brk.address == addr => {
                            let _ = rc.engine.editor.edit_break(&brk, &label);
                            false
                        },
                        _ => match rc.engine.editor.insert_break(addr, &label) {
                            Ok(()) => false,
                            Err(edit::BreakInsertionError::ExistingBreak(_)) => { message = Some("A break already exists at that address."); true },
                        },
                    },
                    (Ok(_), None) => { message = Some("Missing label."); true },
                    (Err(addr::AddressParseError::MissingBytes), _) => false, // user entered a blank address, just ignore
                    (Err(e), _) => {
                        message = Some(match e {
                            addr::AddressParseError::MissingBytes => "Missing bytes field.", // this one shouldn't happen here... but rustc can't figure that out
                            addr::AddressParseError::MalformedBytes(_) => "Failed to parse bytes.",
                            addr::AddressParseError::MalformedBits(_) => "Failed to parse bits.",
                            addr::AddressParseError::TooManyBits => "Invalid amount of bits."
                        }); true
                    }
                }
            },
            gtk::ResponseType::Other(1) => { // Delete
                let rc = rc_clone.read().unwrap();
                
                match &editing_break {
                    Some(ed) => match rc.engine.editor.delete_break(&ed) {
                        Ok(_) => false,
                        Err(edit::BreakDeletionError::IsZeroBreak) => { message = Some("Can't delete the zero break."); true },
                        Err(edit::BreakDeletionError::NotFound) => { message = Some("Not found."); true },
                    },
                    None => { message = Some("Not editing an existing break."); true }
                }
            }
            _ => false
        } {
            error_label.set_text(message.take().unwrap_or(""))
        }
        dialog.hide();
        dialog.set_transient_for::<gtk::Window>(None);
    });

    action
}
