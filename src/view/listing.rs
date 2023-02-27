use std::future;
use std::pin;
use std::sync;
use std::task;

use crate::view::config;
use crate::util;
use crate::model::addr;
use crate::model::datapath::DataPathExt;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::layout;
use crate::model::versioned::Versioned;
use crate::view;
use crate::view::gsc;
use crate::view::helpers;

use gtk::gdk;
use gtk::glib;
use gtk::glib::clone;
use gtk::graphene;
use gtk::pango;
use gtk::subclass::prelude::*;
use gtk::prelude::*;

pub mod facet;
mod token_view;
mod line;

use facet::Facet;

const MICROSECONDS_PER_SECOND: f64 = 1000000.0;
const MICROSECONDS_PER_SECOND_INT: i64 = 1000000;
const NATURAL_ADDRESS_STRING_LENGTH: i32 = 2 + (2*8);

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
    ascii_pane_position: f32,
}

struct Interior {
    document_host: sync::Arc<document::DocumentHost>,
    document: sync::Arc<document::Document>,
    
    window: layout::Window<line::Line>,
    cursor: facet::cursor::CursorView,
    scroll: facet::scroll::Scroller,

    last_frame: i64,
    render: sync::Arc<RenderDetail>,

    document_update_event_source: once_cell::sync::OnceCell<helpers::AsyncSubscriber>,
    work_event_source: once_cell::sync::OnceCell<helpers::AsyncSubscriber>,
    work_notifier: util::Notifier,
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
    fn constructed(&self) {
        self.parent_constructed();

        self.obj().set_vexpand(true);
        self.obj().set_hexpand(true);
    }
}

impl WidgetImpl for ListingWidgetImp {
    fn measure(&self, orientation: gtk::Orientation, _for_size: i32) -> (i32, i32, i32, i32) {
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

    fn size_allocate(&self, width: i32, height: i32, baseline: i32) {
        let mut interior = match self.interior.get() {
            Some(interior) => interior.write(),
            None => return,
        };

        interior.size_allocate(&self.obj(), width, height, baseline);
    }
    
    fn snapshot(&self, snapshot: &gtk::Snapshot) {
        let widget = self.obj();
        let mut interior_guard = match self.interior.get() {
            Some(interior) => interior.write(),
            None => return,
        };
        let interior = &mut *interior_guard;
        
        let render = interior.refresh_render_detail(&self.obj()).clone();

        /* fill in background */
        snapshot.append_color(&render.config.background_color, &graphene::Rect::new(0.0, 0.0, widget.width() as f32, widget.height() as f32));

        /* fill in address pane */
        snapshot.append_color(&render.config.addr_pane_color, &graphene::Rect::new(0.0, 0.0, render.addr_pane_width, widget.height() as f32));
        
        /* render lines */
        snapshot.save();
        snapshot.translate(&graphene::Point::new(0.0, interior.scroll.get_position() as f32 * -helpers::pango_unscale(render.metrics.height())));
        for line in interior.window.lines.iter_mut() {
            if let Some(node) = line.render(&interior.cursor, &*render) {
                snapshot.append_node(node);
            }
            
            snapshot.translate(&graphene::Point::new(0.0, helpers::pango_unscale(render.metrics.height())));
        }
        snapshot.restore();

        /* render debug text */
        if false {
            let mut pos = graphene::Point::new(1000.0, 100.0);
            gsc::render_text(
                snapshot,
                &render.pango,
                &render.font_mono,
                &render.config.text_color,
                &format!("cursor: {:?}", interior.cursor.cursor),
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

struct ListingWidgetWorkFuture(<ListingWidget as glib::clone::Downgrade>::Weak);

impl ListingWidget {
    pub fn new() -> ListingWidget {
        let lw: ListingWidget = glib::Object::builder().build();
        lw.set_can_focus(true);
        lw.set_focusable(true);
        lw.set_focus_on_click(true);
        lw
    }
    
    pub fn init(&self, _window: &view::window::CharmWindow, document_host: sync::Arc<document::DocumentHost>) {
        let render = RenderDetail::new(config::copy(), self.pango_context(), 0);
        let document = document_host.get();
        
        let mut interior = Interior {
            document_host: document_host.clone(),
            document: document.clone(),
            
            window: layout::Window::new(document.clone()),
            cursor: facet::cursor::CursorView::new(document.clone()),
            scroll: facet::scroll::Scroller::new(),

            last_frame: match self.frame_clock() {
                Some(fc) => fc.frame_time(),
                None => 0
            },
            render: sync::Arc::new(render),

            document_update_event_source: once_cell::sync::OnceCell::new(),
            work_event_source: once_cell::sync::OnceCell::new(),
            work_notifier: util::Notifier::new(),
        };

        /* Set the initial size. */
        interior.size_allocate(
            self,
            self.allocated_width(),
            self.allocated_height(),
            self.allocated_baseline());

        self.add_tick_callback(|lw, frame_clock| {
            lw.imp().interior.get().unwrap().write().animate(lw, frame_clock)
        });

        let update_subscriber = helpers::subscribe_to_updates(self.downgrade(), document_host, document, |lw, new_doc| {
            lw.document_updated(new_doc);
        });
        
        if interior.document_update_event_source.set(update_subscriber).is_err() {
            panic!("double-initialized document_update_event_source");
        }

        if interior.work_event_source.set(helpers::spawn_on_main_context(ListingWidgetWorkFuture(self.downgrade()))).is_err() {
            panic!("double-initialized work_event_source");
        }

        let ec_key = gtk::EventControllerKey::new();
        ec_key.connect_key_pressed(clone!(@weak self as lw => @default-return gtk::Inhibit(false), move |_eck, keyval, keycode, modifier| {
            let inhibit = lw.imp().interior.get().unwrap().write().key_pressed(&lw, keyval, keycode, modifier);
            inhibit
        }));
        self.add_controller(&ec_key);

        let ec_scroll = gtk::EventControllerScroll::new(gtk::EventControllerScrollFlags::VERTICAL);
        ec_scroll.connect_scroll(clone!(@weak self as lw => @default-return gtk::Inhibit(false), move |_ecs, dx, dy| {
            let inhibit = lw.imp().interior.get().unwrap().write().scroll(&lw, dx, dy);
            inhibit
        }));
        self.add_controller(&ec_scroll);
        
        let interior = sync::Arc::new(parking_lot::RwLock::new(interior));
        self.imp().init(interior);
    }

    pub fn cursor(&self) -> parking_lot::MappedRwLockReadGuard<'_, crate::model::listing::cursor::Cursor> {
        parking_lot::RwLockReadGuard::map(self.imp().interior.get().unwrap().read(), |int| &int.cursor.cursor)
    }

    pub fn cursor_mut(&self) -> parking_lot::MappedRwLockWriteGuard<'_, crate::model::listing::cursor::Cursor> {
        self.queue_draw();
        parking_lot::RwLockWriteGuard::map(self.imp().interior.get().unwrap().write(), |int| &mut int.cursor.cursor)
    }

    pub fn bonk(&self) {
        self.imp().interior.get().unwrap().write().cursor.bonk();
    }

    pub fn goto(&self, document: &sync::Arc<document::Document>, path: &structure::Path, offset: addr::Address) {
        let mut interior_guard = self.imp().interior.get().unwrap().write();
        let interior = &mut *interior_guard;
        
        if interior.document.generation() != document.generation() {
            self.bonk();
            return;
        }

        interior.cursor.goto(path, offset).expect("lost cursor");
        interior.scroll.ensure_cursor_is_in_view(&mut interior.window, &mut interior.cursor, facet::scroll::EnsureCursorInViewDirection::Any)
    }

    fn document_updated(&self, new_document: &sync::Arc<document::Document>) {
        let mut interior = self.imp().interior.get().unwrap().write();

        interior.window.update(new_document);
        interior.cursor.cursor.update(new_document);
        interior.document = new_document.clone();

        interior.work_notifier.notify();
        
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
        
        let metrics = font_mono.metrics(Option::<&pango::Language>::None);

        let gsc_mono = gsc::Cache::new(&pg, &font_mono);
        let gsc_bold = gsc::Cache::new(&pg, &font_bold);

        let addr_pane_width = helpers::pango_unscale(gsc_bold.space_width() * NATURAL_ADDRESS_STRING_LENGTH)
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
            ascii_pane_position: 800.0,
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

        self.window.resize(line_count + 2 + (2 * self.scroll.get_lookahead()));
        self.work_notifier.notify();
    }

    fn work(&mut self, widget: &ListingWidget, cx: &mut task::Context) {
        self.work_notifier.enroll(cx);

        self.document.datapath.poll(cx);
        
        for line in self.window.lines.iter_mut() {
            line.work(&self.document, cx);

            if line.wants_draw().collect() {
                widget.queue_draw();
            }
        }

        if self.cursor.wants_work().collect() {
            self.cursor.work(&self.document, cx);
        }
        
        if self.cursor.wants_draw().collect() {
            widget.queue_draw();
        }
    }

    fn collect_events(&mut self, widget: &ListingWidget) {
        self.cursor.collect_events(widget, &self.work_notifier);
        self.scroll.collect_events(widget, &self.work_notifier);
    }
    
    fn animate(&mut self, widget: &ListingWidget, frame_clock: &gdk::FrameClock) -> glib::Continue {
        let frame_time = frame_clock.frame_time(); // in microseconds

        if frame_time - self.last_frame > (MICROSECONDS_PER_SECOND as i64) {
            /* if we fall too far behind, just drop frames */
            self.last_frame = frame_time;
        }

        let delta = (frame_time - self.last_frame) as f64 / MICROSECONDS_PER_SECOND as f64;

        self.cursor.animate(delta);
        self.scroll.animate(&mut self.window, &self.cursor, delta);

        self.collect_events(widget);
        
        self.last_frame = frame_time;
        
        glib::Continue(true)
    }

    fn cursor_transaction<F>(&mut self, cb: F, dir: facet::scroll::EnsureCursorInViewDirection) -> gtk::Inhibit
    where F: FnOnce(&mut facet::cursor::CursorView) {
        cb(&mut self.cursor);
        self.scroll.ensure_cursor_is_in_view(&mut self.window, &self.cursor, dir);
        gtk::Inhibit(true)
    }

    // TODO: bring back ensure cursor in view
    fn cursor_transaction_fallible<F>(&mut self, cb: F, dir: facet::scroll::EnsureCursorInViewDirection) -> gtk::Inhibit
    where F: FnOnce(&mut facet::cursor::CursorView) -> bool {
        if cb(&mut self.cursor) {
            self.scroll.ensure_cursor_is_in_view(&mut self.window, &self.cursor, dir);
            gtk::Inhibit(true)
        } else {
            gtk::Inhibit(false)
        }
    }

    fn key_pressed(&mut self, widget: &ListingWidget, keyval: gdk::Key, _keycode: u32, modifier: gdk::ModifierType) -> gtk::Inhibit {
        let r = match (keyval, modifier.intersects(gdk::ModifierType::SHIFT_MASK), modifier.intersects(gdk::ModifierType::CONTROL_MASK)) {
            /* basic cursor   key    shift  ctrl  */
            (gdk::Key::Left,  false, false) => self.cursor_transaction(|c| c.move_left(),  facet::scroll::EnsureCursorInViewDirection::Up),
            (gdk::Key::Right, false, false) => self.cursor_transaction(|c| c.move_right(), facet::scroll::EnsureCursorInViewDirection::Down),
            //(gdk::keys::constants::Up,    false, false) => self.cursor_transaction(|c| c.move_up(),    facet::scroll::EnsureCursorInViewDirection::Up),
            //(gdk::keys::constants::Down,  false, false) => self.cursor_transaction(|c| c.move_down(),  facet::scroll::EnsureCursorInViewDirection::Down),

            /* fast cursor    key    shift  ctrl  */
            //(gdk::keys::constants::Left,  false, true ) => self.cursor_transaction(|c| c.move_left_large(),    facet::scroll::EnsureCursorInViewDirection::Up),
            //(gdk::keys::constants::Right, false, true ) => self.cursor_transaction(|c| c.move_right_large(),   facet::scroll::EnsureCursorInViewDirection::Down),
            //(gdk::keys::constants::Up,    false, true ) => self.cursor_transaction(|c| c.move_up_to_break(),   facet::scroll::EnsureCursorInViewDirection::Up),
            //(gdk::keys::constants::Down,  false, true ) => self.cursor_transaction(|c| c.move_down_to_break(), facet::scroll::EnsureCursorInViewDirection::Down),

            /* basic scroll   key         shift  ctrl  */
            (gdk::Key::Page_Up,   false, false) => { self.scroll.page_up(&self.window); gtk::Inhibit(true) },
            (gdk::Key::Page_Down, false, false) => { self.scroll.page_down(&self.window); gtk::Inhibit(true) },
            
            //_ => self.cursor_transaction_fallible(|c| c.entry(&document_host, ek), facet::scroll::EnsureCursorInViewDirection::Any),
            _ => gtk::Inhibit(false),
        };

        self.collect_events(widget);

        r
    }

    fn scroll(&mut self, widget: &ListingWidget, _dx: f64, dy: f64) -> gtk::Inhibit {
        self.scroll.scroll_wheel_impulse(dy);
        
        self.collect_events(widget);
        
        gtk::Inhibit(true)
    }
}

impl future::Future for ListingWidgetWorkFuture {
    type Output = ();

    fn poll(self: pin::Pin<&mut Self>, cx: &mut task::Context<'_>) -> task::Poll<()> {
        if let Some(lw) = self.0.upgrade() {
            lw.imp().interior.get().unwrap().write().work(&lw, cx);
            task::Poll::Pending
        } else {
            task::Poll::Ready(())
        }
    }
}
