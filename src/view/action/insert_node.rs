use std::cell;
use std::rc;
use std::sync;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::view::helpers;
use crate::view::listing;
use crate::view::window;

use gtk::prelude::*;
use gtk::gio;

struct InsertNodeAction {
    document_host: sync::Arc<document::DocumentHost>,
    document: cell::RefCell<sync::Arc<document::Document>>,

    lw: listing::ListingWidget,
    
    dialog: gtk::ApplicationWindow,
    
    path: cell::RefCell<structure::Path>,
    name_entry: gtk::Entry,
    size_entry: gtk::Entry,
    offset_entry: gtk::Entry,
    path_display: gtk::Entry,
}

pub fn create_action(window_context: &window::WindowContext) -> gio::SimpleAction {
    let builder = gtk::Builder::from_string(include_str!("../insert-node.ui"));

    let name_entry: gtk::Entry = builder.object("name_entry").unwrap();
    let size_entry: gtk::Entry = builder.object("size_entry").unwrap();
    let offset_entry: gtk::Entry = builder.object("offset_entry").unwrap();
    let path_display: gtk::Entry = builder.object("path_display").unwrap();
    let insert_button: gtk::Button = builder.object("insert_button").unwrap();

    let dialog = gtk::ApplicationWindow::builder()
        .application(&window_context.window.upgrade().unwrap().application.application)
        .child(&builder.object::<gtk::Widget>("toplevel").unwrap())
        .resizable(true)
        .title("Insert node")
        .transient_for(&window_context.window.upgrade().unwrap().window)
        .hide_on_close(true)
        .destroy_with_parent(true)
        .default_widget(&insert_button)
        .build();
    
    let action = rc::Rc::new(InsertNodeAction {
        document_host: window_context.document_host.clone(),
        document: cell::RefCell::new(window_context.document_host.get()),
        lw: window_context.lw.clone(),
        dialog: dialog.clone(),
        
        path: cell::RefCell::new(vec![]),
        name_entry,
        size_entry,
        offset_entry,
        path_display,
    });
    
    helpers::bind_simple_action(&action, &action.dialog, "cancel", |action| {
        action.dialog.hide();
    });

    helpers::bind_simple_action(&action, &action.dialog, "insert", |action| {
        action.do_insert();
        action.dialog.hide();
    });

    helpers::create_simple_action_strong(action, "insert_node", |ina| ina.activate())
}

impl InsertNodeAction {
    fn do_insert(&self) {
        let name = self.name_entry.text().as_str().to_string();
        
        let size = match addr::Address::parse(self.size_entry.text().as_str()) {
            Ok(a) => a,
            Err(e) => { println!("TODO: failed to parse address: {:?}", e); return }
        }.to_size();
        
        let offset = match addr::Address::parse(self.offset_entry.text().as_str()) {
            Ok(a) => a,
            Err(e) => { println!("TODO: failed to parse address: {:?}", e); return }
        };

        let document = self.document.borrow();
        let parent_node = document.lookup_node(&*self.path.borrow()).0;
        let index = parent_node.children.partition_point(|child| offset >= child.offset);

        self.document_host.change(document.insert_node(
            self.path.borrow().clone(),
            index,
            structure::Childhood::new(
                sync::Arc::new(structure::Node {
                    props: parent_node.props.clone_rename(name),
                    children: Vec::new(),
                    size,
                }),
                offset
            )
        )).expect("TODO: handle this");
    }
    
    pub fn activate(&self) {
        let cursor = self.lw.cursor();

        *self.path.borrow_mut() = cursor.structure_path();
        *self.document.borrow_mut() = cursor.document();
        self.offset_entry.set_text(&format!("{}", cursor.structure_offset()));
        self.path_display.set_text(&self.document.borrow().describe_path(&self.path.borrow()));

        self.name_entry.grab_focus();
        self.dialog.present();
    }
}

impl Drop for InsertNodeAction {
    fn drop(&mut self) {
        self.dialog.destroy();
    }
}

struct InsertFixedSizeNodeAtCursorAction {
    document_host: sync::Arc<document::DocumentHost>,
    lw: listing::ListingWidget
}

pub fn create_insert_fixed_size_node_at_cursor_action<S: Into<addr::Size>>(window_context: &window::WindowContext, name: &str, size: S) -> gio::SimpleAction {
    let document_host = window_context.document_host.clone();
    let lw = window_context.lw.clone();
    let action = gio::SimpleAction::new(&format!("insert_{}", name), None);
    let size = size.into();
    let name = name.to_string();

    action.connect_activate(move |_, _| {
        let mut cursor = lw.cursor_mut();
        
        match cursor.insert_node(&document_host, sync::Arc::new(structure::Node {
            props: structure::Properties {
                name: name.clone(),
                title_display: structure::TitleDisplay::Minor,
                children_display: structure::ChildrenDisplay::Full,
                content_display: structure::ContentDisplay::default_hexdump(),
                locked: false,
            },
            children: Vec::new(),
            size,
        })) {
            Ok(()) => {},
            Err(e) => {
                println!("failed to insert node at cursor: {:?}", e);
                std::mem::drop(cursor);
                lw.bonk();
            },
        }
    });

    action.set_enabled(true);

    action
}
