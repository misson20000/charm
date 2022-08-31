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
    config: sync::Arc<config::Config>,
    
    pango: pango::Context,
    font_mono: pango::Font,
    metrics: pango::FontMetrics,
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

    fn size_allocate(&self, widget: &Self::Type, width: i32, height: i32, baseline: i32) {
        let mut interior = match self.interior.get() {
            Some(interior) => interior.write(),
            None => return,
        };

        interior.size_allocate(widget, width, height, baseline);
    }
    
    fn snapshot(&self, widget: &Self::Type, snapshot: &gtk::Snapshot) {
        let mut interior = match self.interior.get() {
            Some(interior) => interior.write(),
            None => return,
        };

        let render = interior.refresh_render_detail(widget);
        
        snapshot.append_color(&render.config.background_color, &graphene::Rect::new(0.0, 0.0, widget.width() as f32, widget.height() as f32));

        render.pango.set_font_description(&render.font_mono.describe().unwrap());
        let items = pango::itemize(&render.pango, "foo", 0, 3, &pango::AttrList::new(), None);
        if items.len() != 1 {
            panic!("expected exactly one pango::Item");
        }

        let mut gs = pango::GlyphString::new();
        pango::shape("foo", &items[0].analysis(), &mut gs);

        snapshot.append_node(gsk::TextNode::new(&render.font_mono, &mut gs, &render.config.text_color, &graphene::Point::new(50.0, 50.0)).unwrap());
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
        let render = RenderDetail::new(config::copy(), self.pango_context());
        
        let mut interior = Interior {
            document_host: document_host.clone(),
            window: layout::Window::new(&document_host.get_document().root),

            render,
        };

        /* Set the initial size. */
        interior.size_allocate(
            self,
            self.allocated_width(),
            self.allocated_height(),
            self.allocated_baseline());

        let interior = sync::Arc::new(parking_lot::RwLock::new(interior));

        self.imp().init(interior);
    }
}

impl RenderDetail {
    fn new(config: config::Copy, pg: pango::Context) -> RenderDetail {
        let fm = pg.font_map().unwrap();

        let font_mono = fm.load_font(&pg, &config.monospace_font).expect("expected to be able to load selected font");
        let metrics = font_mono.metrics(Option::<&pango::Language>::None).unwrap();
        
        RenderDetail {
            config,
            pango: pg,
            font_mono,
            metrics,
        }
    }
}

impl Interior {
    fn refresh_render_detail(&mut self, widget: &ListingWidget) -> &RenderDetail {
        let config = config::borrow();
        if !sync::Arc::ptr_eq(&self.render.config, &*config) {
            self.render = RenderDetail::new(config.clone(), widget.pango_context());
        }
        &self.render
    }
    
    /// Resize the underlying window.
    fn size_allocate(&mut self, widget: &ListingWidget, _width: i32, height: i32, _baseline: i32) {
        let render = self.refresh_render_detail(widget);
        let line_height = render.metrics.height();

        // TODO: replace with div_ceil when it gets stabilized
        // https://github.com/rust-lang/rust/issues/88581
        //let line_count = height.div_ceil(line_height) as usize;
        let line_count = ((height + line_height - 1) / line_height) as usize;

        self.window.resize(line_count);
    }
}
