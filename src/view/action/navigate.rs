use crate::view::window;

use std::cell;
use std::rc;

struct NavigateAction {
    window_context: rc::Weak<window::WindowContext>,
    
    dialog: gtk::ApplicationWindow,
    document: cell::RefCell<sync::Arc<document::Document>>,
    document_host: sync::Arc<document::DocumentHost>
}

impl NavigateAction {
    fn new(window_context: &window::WindowContext) -> NavigateAction {
        let builder = gtk::Builder::from_string(include_str!("../../navigate.ui"));

        let entry: gtk::SearchEntry = builder.object("entry").unwrap();
        let list: gtk::ListView = builder.object("list").unwrap();
        let cancel_button: gtk::Button = builder.object("cancel_button").unwrap();

        
    }

    fn activate(&self) {
        self.dialog.present();
    }
}

impl Drop for NavigateAction {
    fn drop(&mut self) {
        self.dialog.destroy();
    }
}
