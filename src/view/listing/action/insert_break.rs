use std::rc;
use std::sync;

use crate::model::addr;
use crate::model::document;
use crate::model::document::BreakMapExt;
use crate::model::document::brk;
use crate::view::listing::ListingWidget;

use gtk::gio;
use gtk::prelude::*;

struct InsertBreakAction {
    dialog: gtk::Dialog,
    lw: sync::Arc<parking_lot::RwLock<ListingWidget>>, // TODO: change this out for a cursor reference or something?
    document_host: sync::Arc<document::DocumentHost>,
    da: gtk::DrawingArea, // TODO: weak-ref
    error_label: gtk::Label,
    label_entry: gtk::Entry,
    addr_entry: gtk::Entry,
    type_box: gtk::ComboBoxText,

    hex_break_grid: gtk::Grid,
    hex_break_line_width_entry: gtk::SpinButton,
}

pub fn create(rc: &sync::Arc<parking_lot::RwLock<ListingWidget>>, lw: &ListingWidget) -> gio::SimpleAction {
    let action = gio::SimpleAction::new("insert_break", None);
    let dialog = gtk::Dialog::with_buttons::<gtk::Window>(
        Some("Insert Break"),
        None,
        gtk::DialogFlags::MODAL | gtk::DialogFlags::DESTROY_WITH_PARENT,
        &[
            ("_Cancel", gtk::ResponseType::Cancel),
            ("_Delete", gtk::ResponseType::Other(1)),
            ("_Edit", gtk::ResponseType::Other(2)),
            ("_Insert", gtk::ResponseType::Other(3)),
        ]);

    let ca = dialog.content_area();
    ca.set_spacing(12);
    
    let grid = gtk::Grid::new();
    grid.set_row_spacing(12);
    
    let error_label = gtk::Label::new(None);
    ca.pack_start(&error_label, false, false, 0);

    let label_entry = gtk::Entry::new(); {
        let label = gtk::Label::new(Some("Label"));
        label.set_hexpand(true);
        label.set_halign(gtk::Align::Start);
        label_entry.set_activates_default(true);
        grid.attach(&label, 0, 0, 1, 1);
        grid.attach(&label_entry, 1, 0, 1, 1);
    };
    
    let addr_entry = gtk::Entry::new(); {
        let label = gtk::Label::new(Some("Address"));
        label.set_hexpand(true);
        label.set_halign(gtk::Align::Start);
        addr_entry.set_activates_default(true);
        grid.attach(&label, 0, 1, 1, 1);
        grid.attach(&addr_entry, 1, 1, 1, 1);
        ca.pack_start(&grid, false, false, 0);
    };
    
    let type_box = gtk::ComboBoxText::new();
    type_box.append(Some("hex"), "Hex");
    ca.pack_start(&type_box, false, false, 0);

    let hex_break_grid = gtk::Grid::new();
    hex_break_grid.set_row_spacing(12);
    let hex_break_line_width_entry = gtk::SpinButton::with_range(1.0, 32.0, 1.0); {
        let label = gtk::Label::new(Some("Line Width"));
        label.set_hexpand(true);
        label.set_halign(gtk::Align::Start);
        hex_break_grid.attach(&label, 0, 0, 1, 1);
        hex_break_grid.attach(&hex_break_line_width_entry, 1, 0, 1, 1);
    };
    ca.pack_start(&hex_break_grid, false, false, 0);
    
    let iba = rc::Rc::new(InsertBreakAction {
        dialog,
        lw: rc.clone(),
        document_host: lw.document_host.clone(),
        da: (*lw.da).clone(),
        error_label,
        label_entry,
        addr_entry,
        type_box,

        hex_break_grid,
        hex_break_line_width_entry,
    });
    
    {
        let iba_clone = iba.clone();
        iba.addr_entry.connect_changed(move |_entry| {
            iba_clone.update_dialog_buttons();
        });
    }

    {
        let iba_clone = iba.clone();
        iba.type_box.connect_changed(move |_entry| {
            iba_clone.update_available_details();
        });
    }
    
    ca.show_all();

    action.connect_activate(move |_act, _par| {
        iba.activate();
    });

    action.set_enabled(true);
        
    action
}

impl InsertBreakAction {
    fn update_dialog_buttons(&self) {
        let addr_text_gstring = self.addr_entry.text();
        let addr_text = addr_text_gstring.as_ref();

        match addr::Address::parse(addr_text) {
            Ok(addr) => {
                match self.document_host.get_document().breaks.break_at(addr) {
                    brk if brk.addr == addr => {
                        self.dialog.widget_for_response(gtk::ResponseType::Other(1)).map(|w| w.set_sensitive(addr != addr::unit::NULL)); /* delete */
                        self.dialog.widget_for_response(gtk::ResponseType::Other(2)).map(|w| w.set_sensitive(true)); /* edit */
                        self.dialog.widget_for_response(gtk::ResponseType::Other(3)).map(|w| w.set_sensitive(false)); /* insert */
                        self.dialog.set_default_response(gtk::ResponseType::Other(2)); /* edit */
                    },
                    _ => {
                        self.dialog.widget_for_response(gtk::ResponseType::Other(1)).map(|w| w.set_sensitive(false)); /* delete */
                        self.dialog.widget_for_response(gtk::ResponseType::Other(2)).map(|w| w.set_sensitive(false)); /* edit */
                        self.dialog.widget_for_response(gtk::ResponseType::Other(3)).map(|w| w.set_sensitive(true)); /* insert */
                        self.dialog.set_default_response(gtk::ResponseType::Other(3)); /* insert */
                    }
                }
            },
            _ => {
                self.dialog.widget_for_response(gtk::ResponseType::Other(1)).map(|w| w.set_sensitive(false)); /* delete */
                self.dialog.widget_for_response(gtk::ResponseType::Other(2)).map(|w| w.set_sensitive(false)); /* edit */
                self.dialog.widget_for_response(gtk::ResponseType::Other(3)).map(|w| w.set_sensitive(false)); /* insert */
                self.dialog.set_default_response(gtk::ResponseType::Cancel);
            }
        }
    }

    fn update_details(&self, brk: &sync::Arc<brk::Break>) {
        self.type_box.set_active_id(match &brk.class {
            brk::BreakClass::Hex(hex) => {
                self.hex_break_line_width_entry.set_value(hex.line_size.bytes as f64);
                
                Some("hex")
            },
        });

        self.update_available_details();
    }

    fn update_available_details(&self) {
        self.hex_break_grid.hide();

        match self.type_box.active_id() {
            Some(gs) => match gs.as_ref() {
                "hex" => self.hex_break_grid.show(),
                _ => {},
            },
            None => {},
        }
        
    }

    fn activate(&self) {
        self.dialog.set_transient_for(self.da.toplevel().and_then(|tl| tl.dynamic_cast::<gtk::Window>().ok()).as_ref());

        let lw = self.lw.read();
        let addr = lw.cursor_view.cursor.get_addr();

        let upstream = self.document_host.get_document();
        let breaks = &upstream.breaks;
        
        self.label_entry.set_text(breaks.get(&addr).and_then(|brk| brk.label.as_ref()).map(|l| l.as_str()).unwrap_or(""));
        self.label_entry.grab_focus();
        
        self.error_label.set_text("");
        self.addr_entry.set_text(&format!("{}", lw.cursor_view.cursor.get_addr()));

        self.update_details(breaks.break_at(addr));

        std::mem::drop(upstream);
        std::mem::drop(lw);
        
        loop {
            let response = self.dialog.run();
            let addr_text_gstring = self.addr_entry.text();
            let addr_text = addr_text_gstring.as_ref();
            let label_text = self.label_entry.text();
            
            let message = match response {
                gtk::ResponseType::Other(2) | gtk::ResponseType::Other(3) => match addr::Address::parse(addr_text) { /* edit or insert */
                    Ok(addr) => {
                        self.document_host.insert_break(brk::Break {
                            addr,
                            label: if label_text.len() == 0 { None } else { Some(label_text.to_string()) },
                            class: brk::BreakClass::Hex(brk::hex::HexBreak {
                                line_size: addr::Size::from(self.hex_break_line_width_entry.value_as_int() as u64),
                            }),
                        }); break
                    },
                    Err(addr::AddressParseError::MissingBytes) => break, /* user entered a blank address, just exit out */
                    Err(addr::AddressParseError::MalformedBytes(_)) => "Failed to parse bytes.",
                    Err(addr::AddressParseError::MalformedBits(_)) => "Failed to parse bits.",
                    Err(addr::AddressParseError::TooManyBits) => "Invalid amount of bits.",
                }
                gtk::ResponseType::Other(1) => match addr::Address::parse(addr_text) { /* delete */
                    Ok(addr) => match self.document_host.delete_break(&addr) {
                        Ok(_) => break,
                        Err(document::BreakDeletionError::IsZeroBreak) => "Can't delete the zero break.",
                        Err(document::BreakDeletionError::NotFound) => "Not found.",
                    }
                    Err(addr::AddressParseError::MissingBytes) => break, /* user entered a blank address, just exit out */
                    Err(addr::AddressParseError::MalformedBytes(_)) => "Failed to parse bytes.",
                    Err(addr::AddressParseError::MalformedBits(_)) => "Failed to parse bits.",
                    Err(addr::AddressParseError::TooManyBits) => "Invalid amount of bits.",
                }
                _ => break // what?
            };
            self.error_label.set_text(message);
        }
        self.dialog.hide();
        self.dialog.set_transient_for(Option::<&gtk::Window>::None);
    }
}
