use std::iter;
use std::sync;
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
use crate::view::gsc;
use crate::view::helpers;
//use crate::view::ext::CairoExt;

use gtk::gdk;
use gtk::glib;
use gtk::graphene;
use gtk::gsk;
use gtk::pango;
use gtk::subclass::prelude::*;
use gtk::prelude::*;

mod facet;
mod token_view;

use facet::Facet;

const MICROSECONDS_PER_SECOND: f64 = 1000000.0;
const MICROSECONDS_PER_SECOND_INT: i64 = 1000000;
const NATURAL_ADDRESS_STRING_LENGTH: i32 = 2 + (2*8);

struct Line {
    tokens: vec::Vec<token_view::TokenView>,
    render_serial: u64,
    render_node: Option<gsk::RenderNode>,
}

impl layout::Line for Line {
    type TokenIterator = iter::Map<vec::IntoIter<token_view::TokenView>, fn(token_view::TokenView) -> token::Token>;
    
    fn from_tokens(tokens: vec::Vec<token::Token>) -> Self {
        Line {
            tokens: tokens.into_iter().map(|t| token_view::TokenView::from(t)).collect(),
            render_serial: 0,
            render_node: None,
        }
    }

    fn to_tokens(self) -> Self::TokenIterator {
        self.tokens.into_iter().map(|t| t.to_token())
    }
}

// TODO: see if we can reduce visibility on this.
// https://github.com/rust-lang/rust/issues/34537
// I can't read that right now because I'm on an airplane.
pub struct RenderDetail {
    config: sync::Arc<config::Config>,
    serial: u64,
    
    pango: pango::Context,
    font_mono: pango::Font,
    font_bold: pango::Font,
    metrics: pango::FontMetrics,

    gsc_mono: gsc::Cache,
    gsc_bold: gsc::Cache,

    addr_pane_width: f32,
}

struct Interior {
    document_host: sync::Arc<document::DocumentHost>,
    document: sync::Arc<document::Document>,
    
    window: layout::Window<Line>,
    cursor: facet::cursor::CursorView,

    last_frame: i64,
    render: sync::Arc<RenderDetail>,
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
        let mut interior_guard = match self.interior.get() {
            Some(interior) => interior.write(),
            None => return,
        };
        let interior = &mut *interior_guard;

        let render = interior.refresh_render_detail(widget).clone();

        /* fill in background */
        snapshot.append_color(&render.config.background_color, &graphene::Rect::new(0.0, 0.0, widget.width() as f32, widget.height() as f32));

        /* fill in address pane */
        snapshot.append_color(&render.config.addr_pane_color, &graphene::Rect::new(0.0, 0.0, render.addr_pane_width, widget.height() as f32));
        
        /* render lines */
        snapshot.save();
        for line in interior.window.lines.iter_mut() {
            if let Some(node) = line.render(&interior.cursor, &*render) {
                snapshot.append_node(node);
            }
            
            snapshot.translate(&graphene::Point::new(0.0, render.metrics.height() as f32 / pango::SCALE as f32));
        }
        snapshot.restore();

        /* render debug text */
        {
            let mut pos = graphene::Point::new(1000.0, 100.0);
            gsc::render_text(
                &snapshot,
                &render.pango,
                &render.font_mono,
                &render.config.text_color,
                &format!("last_frame: {:?}", interior.last_frame),
                &mut pos);
        }
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
    
    pub fn init(&self, _window: &view::window::CharmWindow, document_host: sync::Arc<document::DocumentHost>) {
        let render = RenderDetail::new(config::copy(), self.pango_context(), 0);
        let document = document_host.get();
        
        let mut interior = Interior {
            document_host: document_host.clone(),
            document: document.clone(),
            
            window: layout::Window::new(document.clone()),
            cursor: facet::cursor::CursorView::new(document.clone()),

            last_frame: match self.frame_clock() {
                Some(fc) => fc.frame_time(),
                None => 0
            },
            render: sync::Arc::new(render),
        };

        /* Set the initial size. */
        interior.size_allocate(
            self,
            self.allocated_width(),
            self.allocated_height(),
            self.allocated_baseline());

        let interior = sync::Arc::new(parking_lot::RwLock::new(interior));

        self.imp().init(interior);

        self.add_tick_callback(|lw, frame_clock| {
            lw.imp().interior.get().unwrap().write().animate(lw, frame_clock)
        });
        
        helpers::subscribe_to_document_updates(self.downgrade(), document_host, document, |lw, new_doc| {
            lw.document_updated(new_doc);
        });
    }

    fn document_updated(&self, new_document: &sync::Arc<document::Document>) {
        let mut interior = self.imp().interior.get().unwrap().write();

        interior.window.update(new_document);
        interior.document = new_document.clone();
        
        self.queue_draw();
    }
}

impl RenderDetail {
    fn new(config: config::Copy, pg: pango::Context, serial: u64) -> RenderDetail {
        let fm = pg.font_map().unwrap();

        let font_mono = fm.load_font(&pg, &config.monospace_font).expect("expected to be able to load selected font");

        let mut desc_bold = config.monospace_font.clone();
        desc_bold.set_weight(pango::Weight::Bold);
        let font_bold = fm.load_font(&pg, &desc_bold).expect("expected to be able to load bold variant of selected font");
        
        let metrics = font_mono.metrics(Option::<&pango::Language>::None).unwrap();

        let gsc_mono = gsc::Cache::new(&pg, &font_mono);
        let gsc_bold = gsc::Cache::new(&pg, &font_bold);

        let addr_pane_width = (gsc_bold.space_width() * NATURAL_ADDRESS_STRING_LENGTH) as f32 / pango::SCALE as f32
            + (2.0 * config.padding as f32);
        
        RenderDetail {
            config,
            serial,

            gsc_mono,
            gsc_bold,

            pango: pg,
            font_mono,
            font_bold,
            metrics,

            addr_pane_width,
        }
    }
}

impl Interior {
    fn refresh_render_detail(&mut self, widget: &ListingWidget) -> &sync::Arc<RenderDetail> {
        let config = config::borrow();
        if !sync::Arc::ptr_eq(&self.render.config, &*config) {
            self.render = sync::Arc::new(
                RenderDetail::new(
                    config.clone(),
                    widget.pango_context(),
                    self.render.serial + 1));
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
        let line_count = (((height * pango::SCALE) + line_height - 1) / line_height) as usize;
        println!("height: {}, line height: {}, line count: {}", height, line_height, line_count);

        self.window.resize(line_count);
    }

    fn animate(&mut self, widget: &ListingWidget, frame_clock: &gdk::FrameClock) -> glib::Continue {
        let frame_time = frame_clock.frame_time(); // in microseconds

        if frame_time - self.last_frame > (MICROSECONDS_PER_SECOND as i64) {
            /* if we fall too far behind, just drop frames */
            self.last_frame = frame_time;
        }

        let delta = (frame_time - self.last_frame) as f64 / MICROSECONDS_PER_SECOND as f64;

        self.cursor.animate(delta);

        if self.cursor.wants_draw().collect() {
            widget.queue_draw();
        }
        
        self.last_frame = frame_time;
        
        glib::Continue(true)
    }
}

impl Line {
    fn invalidate(&mut self) {
        self.render_node = None;
    }

    fn render(&mut self, cursor: &facet::cursor::CursorView, render: &RenderDetail) -> Option<gsk::RenderNode> {
        /* if we rendered this line earlier and the render hasn't been invalidated, just reuse the previous snapshot. */
        if let Some(rn) = self.render_node.as_ref() {
            if self.render_serial == render.serial {
                /* if the cursor is on any of our tokens, we need to render fresh every time. */
                if !self.tokens.iter().any(|t| t.contains_cursor(&cursor.cursor)) {
                    return Some(rn.clone());
                }
            }
        }

        let snapshot = gtk::Snapshot::new();
        let mut position = graphene::Point::zero();

        /* begin rendering to the right of the address pane */
        position.set_x(render.addr_pane_width + render.config.padding as f32);

        /* indent by first token */
        if let Some(first) = self.tokens.get(0) {
            position.set_x(
                position.x() +
                render.config.indentation_width *
                    render.gsc_mono.space_width() as f32 /
                    pango::SCALE as f32 *
                    first.get_indentation() as f32);
        }

        let mut visible_address = None;

        /* render tokens */
        for token in &mut self.tokens {
            snapshot.save();
            let advance = token.render(&snapshot, cursor, render, &position);
            snapshot.restore();
            position.set_x(position.x() + advance.x());

            /* pick the address from the first token that should display an address */
            if visible_address.is_none() {
                visible_address = token.visible_address();
            }
        }

        /* if any of our tokens wanted to show an address, render the first one into the address pane */
        if let Some(addr) = visible_address {
            let mut pos = graphene::Point::new(render.addr_pane_width - render.config.padding as f32, render.metrics.height() as f32 / pango::SCALE as f32);
            gsc::render_text_align_right(&snapshot, &render.pango, &render.font_mono, &render.config.addr_color, &format!("{}", addr), &mut pos);
        }

        /* update our stored snapshot so we can reuse it later */
        self.render_serial = render.serial;
        self.render_node = snapshot.to_node();

        self.render_node.clone()
    }
}
