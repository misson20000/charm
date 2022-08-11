use std::sync;
//use std::task;
//use std::vec;
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
//use crate::model::listing::layout;
use crate::view;
//use crate::view::ext::CairoExt;

//use gtk::prelude::*;

//mod facet;
//mod token;

const MICROSECONDS_PER_SECOND: f64 = 1000000.0;
const MICROSECONDS_PER_SECOND_INT: i64 = 1000000;

pub struct ListingWidget {
    
}

impl ListingWidget {
    pub fn new(window: &view::window::CharmWindow, document_host: &sync::Arc<document::DocumentHost>) -> sync::Arc<parking_lot::RwLock<ListingWidget>> {
        sync::Arc::new(parking_lot::RwLock::new(ListingWidget {
        }))
    }
}
