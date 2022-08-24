use std::sync;
//use std::task;
use std::vec;
//use std::time;
//use std::collections::HashMap;

use crate::view::config;
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
use gtk::graphene;
use gtk::gsk;
use gtk::pango;
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

struct RenderDetail {
    pango: pango::Context,
    font_mono: pango::Font,
}

struct Interior {
    document_host: sync::Arc<document::DocumentHost>,
    window: layout::Window<Line>,

    render: RenderDetail,
}

#[derive(Default)]
pub struct ListingWidgetImp {
    interior: once_cell::unsync::OnceCell<sync::Arc<parking_lot::RwLock<Interior>>>,
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
    fn measure(&self, _widget: &Self::Type, orientation: gtk::Orientation, _for_size: i32) -> (i32, i32, i32, i32) {
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

    fn snapshot(&self, widget: &Self::Type, snapshot: &gtk::Snapshot) {
        let interior = match self.interior.get() {
            Some(interior) => interior.write(),
            None => return,
        };

        let render = &interior.render;
        
        snapshot.append_color(&config::get().background_color, &graphene::Rect::new(0.0, 0.0, widget.width() as f32, widget.height() as f32));

        render.pango.set_font_description(&render.font_mono.describe().unwrap());
        let items = pango::itemize(&render.pango, "foo", 0, 3, &pango::AttrList::new(), None);
        if items.len() != 1 {
            panic!("expected exactly one pango::Item");
        }

        let mut gs = pango::GlyphString::new();
        pango::shape("foo", &items[0].analysis(), &mut gs);

        snapshot.append_node(gsk::TextNode::new(&render.font_mono, &mut gs, &config::get().text_color, &graphene::Point::new(50.0, 50.0)).unwrap());
    }
}

impl ListingWidgetImp {
    fn init(&self, interior: sync::Arc<parking_lot::RwLock<Interior>>) {
        if self.interior.set(interior).is_err() {
            panic!("ListingWidget should only be initialized once");
        }
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
        let pg = self.create_pango_context();
        let fm = pg.font_map().unwrap();

        let font_mono = fm.load_font(&pg, &config::get().monospace_font).expect("expected to be able to load selected font");
        
        let render = RenderDetail {
            pango: pg,
            font_mono,
        };
        
        let interior = Interior {
            document_host: document_host.clone(),
            window: layout::Window::new(&document_host.get_document().root),

            render,
        };

        let interior = sync::Arc::new(parking_lot::RwLock::new(interior));

        self.imp().init(interior);
    }
}
