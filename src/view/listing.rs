use std::cell;
use std::sync;
//use std::task;
use std::vec;
//use std::time;
//use std::collections::HashMap;

//use crate::view::config;
//use crate::util;
//use crate::model::addr;
//use crate::model::datapath;
//use crate::model::datapath::DataPathExt;
use crate::model::document;
//use crate::model::document::structure;
//use crate::model::listing;
//use crate::model::listing::cursor;
use crate::model::listing::layout;
use crate::model::listing::token;
use crate::view;
//use crate::view::ext::CairoExt;

use gtk::glib;
use gtk::subclass::prelude::*;
use gtk::prelude::*;

//mod facet;
//mod token;

const MICROSECONDS_PER_SECOND: f64 = 1000000.0;
const MICROSECONDS_PER_SECOND_INT: i64 = 1000000;

struct Line {
    tokens: vec::Vec<token::Token>
}

impl layout::Line for Line {
    type TokenIterator = vec::IntoIter<token::Token>;
    
    fn from_tokens(tokens: vec::Vec<token::Token>) -> Self {
        Line {
            tokens: tokens.into(),
        }
    }

    fn to_tokens(self) -> Self::TokenIterator {
        self.tokens.into_iter()
    }

}

struct Interior {
    document_host: sync::Arc<document::DocumentHost>,
    window: layout::Window<Line>,
}

#[derive(Default)]
pub struct ListingWidgetImp {
    interior: cell::Cell<Option<sync::Arc<parking_lot::RwLock<Interior>>>>,
}

#[glib::object_subclass]
impl ObjectSubclass for ListingWidgetImp {
    const NAME: &'static str = "CharmListingWidget";
    type Type = ListingWidget;
    type ParentType = gtk::Widget;
}

impl ObjectImpl for ListingWidgetImp {
    fn constructed(&self, obj: &Self::Type) {
        self.parent_constructed(obj);

        obj.set_vexpand(true);
        obj.set_hexpand(true);
    }
}

impl WidgetImpl for ListingWidgetImp {
    fn measure(&self, widget: &Self::Type, orientation: gtk::Orientation, for_size: i32) -> (i32, i32, i32, i32) {
        match orientation {
            gtk::Orientation::Horizontal => {
                (100, 200, -1, -1)
            },
            gtk::Orientation::Vertical => {
                (600, 1200, -1, -1)
            },
            _ => (-1, -1, -1, -1)
        }
    }
}

impl ListingWidgetImp {
    fn init(&self, interior: sync::Arc<parking_lot::RwLock<Interior>>) {
        self.interior.set(Some(interior));
    }
}

glib::wrapper! {
    pub struct ListingWidget(ObjectSubclass<ListingWidgetImp>)
        @extends gtk::Widget,
        @implements gtk::Accessible, gtk::Buildable, gtk::ConstraintTarget;
}

impl ListingWidget {
    pub fn new() -> ListingWidget {
        glib::Object::new(&[]).expect("failed to create ListingWidget")
    }

    pub fn init(&self, _window: &view::window::CharmWindow, document_host: &sync::Arc<document::DocumentHost>) {
        let interior = Interior {
            document_host: document_host.clone(),
            window: layout::Window::new(&document_host.get_document().root),
        };

        let interior = sync::Arc::new(parking_lot::RwLock::new(interior));

        self.imp().init(interior);
    }
}
