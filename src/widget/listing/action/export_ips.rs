use std::rc;
use std::sync;

use gio::prelude::*;
use gtk::prelude::*;

use crate::listing::ListingWatch;
use crate::widget::listing::ListingWidget;

extern crate byteorder;
use byteorder::ByteOrder;

struct ExportIPSAction {
    dialog: gtk::FileChooserDialog,
    lw: sync::Arc<parking_lot::RwLock<ListingWidget>>,
    da: gtk::DrawingArea, // TODO weak-ref
}

pub fn create(rc: &sync::Arc<parking_lot::RwLock<ListingWidget>>, lw: &ListingWidget) -> gio::SimpleAction {
    let action = gio::SimpleAction::new("export_ips", None);

    let dialog = gtk::FileChooserDialog::with_buttons::<gtk::ApplicationWindow>(
        Some("Charm: Export Patches (IPS)"),
        None,
        gtk::FileChooserAction::Save,
        &[
            ("_Cancel", gtk::ResponseType::Cancel),
            ("_Export", gtk::ResponseType::Accept)
        ]);
    dialog.set_do_overwrite_confirmation(true);
        
    let eipsa = rc::Rc::new(ExportIPSAction {
        dialog,
        lw: rc.clone(),
        da: (*lw.da).clone(),
    });
    
    action.set_enabled(true);
    action.connect_activate(move |_act, _par| {
        eipsa.activate();
    });

    action
}

enum IPSExportError {
    NoFileSelected,
    PatchLocationTooLarge(u64),
    PatchSizeTooLarge(usize),
    InvalidHunkOffset, // TODO: add an extra byte to beginning of patch
    IncompleteWrite,
    IOError(glib::Error)
}

impl std::fmt::Display for IPSExportError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IPSExportError::NoFileSelected => write!(f, "No file was selected"),
            IPSExportError::PatchLocationTooLarge(loc) => write!(f, "Patch location ({:x}) too large for IPS24 format", loc),
            IPSExportError::PatchSizeTooLarge(size) => write!(f, "Patch size ({:x}) too large for IPS24 format", size),
            IPSExportError::InvalidHunkOffset => write!(f, "Patch occurs at forbidden location for IPS24 format"),
            IPSExportError::IncompleteWrite => write!(f, "Failed to write all data to file"),
            IPSExportError::IOError(e) => e.fmt(f),
        }
    }
}

trait FullWrite {
    fn full_write(self: &Self, buffer: &[u8]) -> Result<(), IPSExportError>;
}

impl FullWrite for gio::FileOutputStream {
    fn full_write(self: &Self, buffer: &[u8]) -> Result<(), IPSExportError> {
        match self.write_all::<gio::Cancellable>(buffer, None) {
            Ok((size, None)) if size == buffer.len() => Ok(()),
            Ok((_, None)) => Err(IPSExportError::IncompleteWrite),
            Ok((_, Some(e))) => Err(IPSExportError::IOError(e)),
            Err(e) => Err(IPSExportError::IOError(e))
        }
    }
}

fn export_patches(fos: &gio::FileOutputStream, watch: &ListingWatch) -> Result<(), IPSExportError> {
    fos.full_write(b"PATCH")?;
    for (loc, patch) in watch.get_listing().get_patches().iter() {
        if *loc == byteorder::BE::read_u24(b"EOF") as u64 {
            return Err(IPSExportError::InvalidHunkOffset);
        }
        
        if *loc > 0xffffff {
            return Err(IPSExportError::PatchLocationTooLarge(*loc));
        }

        if patch.bytes.len() > 0xffff {
            return Err(IPSExportError::PatchSizeTooLarge(patch.bytes.len()));
        }
        
        let mut header = [0; 5];
        byteorder::BE::write_u24(&mut header[0..3], *loc as u32);
        byteorder::BE::write_u16(&mut header[3..5], patch.bytes.len() as u16);
        fos.full_write(&header)?;
        fos.full_write(&patch.bytes)?;
    }
    fos.full_write(b"EOF")?;

    Ok(())
}

impl ExportIPSAction {
    fn activate(&self) {
        self.dialog.set_transient_for(self.da.get_toplevel().and_then(|tl| tl.dynamic_cast::<gtk::Window>().ok()).as_ref());
        // TODO: self.dialog.set_current_name( document.ips ) ?

        match self.dialog.run() {
            gtk::ResponseType::Accept => {
                if let Err(e) = self.dialog.get_file()
                    .ok_or(IPSExportError::NoFileSelected) /* no file selected? */
                    .and_then(|f| f.replace::<gio::Cancellable>(
                        None,
                        false,
                        gio::FileCreateFlags::NONE,
                        None).map_err(|e| IPSExportError::IOError(e)))
                    .and_then(|fos| {
                        export_patches(&fos, &self.lw.read().listing_watch)
                    }) {
                        let message = format!("{}", e);
                        let dialog = gtk::MessageDialogBuilder::new()
                            .message_type(gtk::MessageType::Error)
                            .buttons(gtk::ButtonsType::Close)
                            .text(&message)
                            .focus_on_map(true)
                            .transient_for(&self.dialog)
                            .build();
                        dialog.run();
                        dialog.close();
                    };
            },
            _ => {} /* we were cancelled, ignore */
        }
        
        self.dialog.hide();
        self.dialog.set_transient_for::<gtk::Window>(None);
    }
}
