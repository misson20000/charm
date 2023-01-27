//pub mod export_ips;
//pub mod goto;
//pub mod insert_break;
//pub mod mode;
//pub mod movement;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::view::helpers;

use gtk::prelude::*;
use gtk::subclass::prelude::*;
use tracing::{event, Level};

use std::rc;
use std::sync;
use std::vec;

struct InsertDialogState {
    path: structure::Path,
    name_entry: gtk::Entry,
    size_entry: gtk::Entry,
    offset_entry: gtk::Entry,
    dialog: gtk::ApplicationWindow,
    document: sync::Arc<document::Document>,
    document_host: sync::Arc<document::DocumentHost>,
}

pub fn action_insert_with_prompt(listing: &super::ListingWidget) {
    let builder = gtk::Builder::from_string(include_str!("../../insert-node.ui"));

    let name_entry: gtk::Entry = builder.object("name_entry").unwrap();
    let size_entry: gtk::Entry = builder.object("size_entry").unwrap();
    let offset_entry: gtk::Entry = builder.object("offset_entry").unwrap();
    let insert_button: gtk::Button = builder.object("insert_button").unwrap();

    let parent = listing.ancestor(gtk::ApplicationWindow::static_type()).expect("listing widget should be a descendent of ApplicationWindow").downcast::<gtk::ApplicationWindow>().unwrap();
    
    let dialog = gtk::ApplicationWindow::builder()
        .application(&parent.application().unwrap())
        .child(&builder.object::<gtk::Widget>("toplevel").unwrap())
        .resizable(true)
        .title(&"Insert node")
        .transient_for(&parent)
        .default_widget(&insert_button)
        .build();

    let listing_interior = listing.imp().interior.get().unwrap().read();
    let cursor = &listing_interior.cursor.cursor;

    offset_entry.set_text(&format!("{}", cursor.structure_offset()));
    
    let state = rc::Rc::new(InsertDialogState {
        path: cursor.structure_path(),
        name_entry,
        size_entry,
        offset_entry,
        dialog: dialog.clone(),
        document: cursor.document(),
        document_host: listing_interior.document_host.clone(),
    });

    helpers::bind_simple_action(&state, &state.dialog, "cancel", |state| {
        state.dialog.close();
        state.dialog.destroy();
    });

    helpers::bind_simple_action(&state, &state.dialog, "insert", |state| {
        let name = state.name_entry.text().as_str().to_string();
        
        let size = match addr::Address::parse(state.size_entry.text().as_str()) {
            Ok(a) => a,
            Err(e) => { println!("TODO: failed to parse address: {:?}", e); return }
        }.to_size();
        
        let offset = match addr::Address::parse(state.offset_entry.text().as_str()) {
            Ok(a) => a,
            Err(e) => { println!("TODO: failed to parse address: {:?}", e); return }
        };

        let parent_node = state.document.lookup_node(&state.path).0;
        let index = parent_node.children.iter().enumerate().find_map(|(i, child)| if child.offset > offset { Some(i) } else { None }).unwrap_or(0);
        
        state.document_host.insert_node(
            &state.document,
            state.path.clone(),
            index,
            offset,
            sync::Arc::new(structure::Node {
                props: structure::Properties {
                    name,
                    title_display: structure::TitleDisplay::Minor,
                    children_display: structure::ChildrenDisplay::Full,
                    content_display: structure::ContentDisplay::Hexdump(addr::Size::from(16)),
                    locked: false
                },
                children: vec::Vec::new(),
                size,
            })).expect("TODO: handle this");

        state.dialog.close();
        state.dialog.destroy();
    });
    
    /* extend lifetime */
    dialog.connect_destroy(move |_| {
        let _ = state;
    });
    
    dialog.present();
}

fn action_insert_node<S: Into<addr::Size>>(listing: &super::ListingWidget, name: String, size: S) {
    let mut interior = listing.imp().interior.get().unwrap().write();
    let dh = interior.document_host.clone();
    
    match interior.cursor.cursor.insert_node(&dh, sync::Arc::new(structure::Node {
        props: structure::Properties {
            name,
            title_display: structure::TitleDisplay::Minor,
            children_display: structure::ChildrenDisplay::Full,
            content_display: structure::ContentDisplay::Hexdump(addr::Size::from(16)),
            locked: false,
        },
        children: vec::Vec::new(),
        size: size.into(),
    })) {
        Ok(()) => {},
        Err(e) => {
            event!(Level::ERROR, "failed to insert empty node at cursor: {:?}", e);
            interior.cursor.bonk()
        },
    }
}

pub fn action_insert_empty_node(listing: &super::ListingWidget) {
    action_insert_node(listing, "empty".to_string(), 0);
}

pub fn action_insert_byte(listing: &super::ListingWidget) {
    action_insert_node(listing, "byte".to_string(), 1);
}

pub fn action_insert_word(listing: &super::ListingWidget) {
    action_insert_node(listing, "word".to_string(), 2);
}

pub fn action_insert_dword(listing: &super::ListingWidget) {
    action_insert_node(listing, "dword".to_string(), 4);
}

pub fn action_insert_qword(listing: &super::ListingWidget) {
    action_insert_node(listing, "qword".to_string(), 8);
}
