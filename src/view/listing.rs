use std::future;
use std::pin;
use std::rc;
use std::sync;
use std::task;

use crate::view::config;
use crate::util;
use crate::model::addr;
use crate::model::datapath::DataPathExt;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::cursor;
use crate::model::listing::layout as layout_model;
use crate::model::selection;
use crate::model::versioned::Versioned;
use crate::view;
use crate::view::error;
use crate::view::gsc;
use crate::view::helpers;

use gtk::gdk;
use gtk::glib;
use gtk::glib::clone;
use gtk::graphene;
use gtk::pango;
use gtk::subclass::prelude::*;
use gtk::prelude::*;

mod bucket;
pub mod facet;
mod token_view;
mod line;
mod layout;
mod pick;

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
    selection_host: sync::Arc<selection::listing::Host>,
    
    document: sync::Arc<document::Document>,
    selection: sync::Arc<selection::ListingSelection>,

    charm_window: rc::Weak<view::window::CharmWindow>,
    window: layout_model::Window<line::Line>,
    cursor: facet::cursor::CursorView,
    scroll: facet::scroll::Scroller,
    hover: Option<(f64, f64)>,
    rubber_band_begin: Option<pick::Triplet>,
    
    last_frame: i64,
    render: sync::Arc<RenderDetail>,

    document_update_event_source: once_cell::sync::OnceCell<helpers::AsyncSubscriber>,
    selection_update_event_source: once_cell::sync::OnceCell<helpers::AsyncSubscriber>,
    config_update_event_source: once_cell::sync::OnceCell<helpers::AsyncSubscriber>,
    work_event_source: once_cell::sync::OnceCell<helpers::AsyncSubscriber>,
    work_notifier: util::Notifier,
    runtime: tokio::runtime::Handle,
}

#[derive(Default)]
pub struct ListingWidgetImp {
    interior: once_cell::unsync::OnceCell<sync::Arc<parking_lot::RwLock<Interior>>>,
    should_repick: sync::atomic::AtomicBool,
}

#[glib::object_subclass]
impl ObjectSubclass for ListingWidgetImp {
    const NAME: &'static str = "CharmListingWidget";
    type Type = ListingWidget;
    type ParentType = gtk::Widget;

    fn class_init(klass: &mut Self::Class) {
        klass.add_shortcut(&gtk::Shortcut::new(gtk::ShortcutTrigger::parse_string("B"), Some(gtk::NamedAction::new("ctx.insert_byte"))));
        klass.add_shortcut(&gtk::Shortcut::new(gtk::ShortcutTrigger::parse_string("W"), Some(gtk::NamedAction::new("ctx.insert_word"))));
        klass.add_shortcut(&gtk::Shortcut::new(gtk::ShortcutTrigger::parse_string("D"), Some(gtk::NamedAction::new("ctx.insert_dword"))));
        klass.add_shortcut(&gtk::Shortcut::new(gtk::ShortcutTrigger::parse_string("Q"), Some(gtk::NamedAction::new("ctx.insert_qword"))));
        klass.add_shortcut(&gtk::Shortcut::new(gtk::ShortcutTrigger::parse_string("Insert"), Some(gtk::NamedAction::new("ctx.insert_node"))));
        klass.add_shortcut(&gtk::Shortcut::new(gtk::ShortcutTrigger::parse_string("N"), Some(gtk::NamedAction::new("ctx.navigate"))));
    }
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
        let render = &interior.render;
        
        let selection = match &interior.selection {
            sel if sync::Arc::ptr_eq(&sel.document, &interior.document) => &sel.mode,
            _ => {
                println!("WARNING: rendering listing widget with selection that does not agree with document. this should not happen!");
                const INVALID_MODE: selection::listing::Mode = selection::listing::Mode::Structure(selection::listing::StructureMode::Empty);

                &INVALID_MODE
            }
        };

        /* fill in background */
        snapshot.append_color(&render.config.background_color, &graphene::Rect::new(0.0, 0.0, widget.width() as f32, widget.height() as f32));

        /* fill in address pane */
        snapshot.append_color(&render.config.addr_pane_color, &graphene::Rect::new(0.0, 0.0, render.addr_pane_width, widget.height() as f32));
        
        /* render lines */
        snapshot.save();
        snapshot.translate(&graphene::Point::new(0.0, interior.scroll.get_position() as f32 * -helpers::pango_unscale(render.metrics.height())));

        for line in interior.window.line_views.iter_mut() {
            if let Some(node) = line.render(&interior.cursor, selection, &*render) {
                snapshot.append_node(node);
            }
            
            snapshot.translate(&graphene::Point::new(0.0, helpers::pango_unscale(render.metrics.height())));
        }
        snapshot.restore();
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
    
    pub fn init(
        &self,
        window: &rc::Rc<view::window::CharmWindow>,
        document_host: sync::Arc<document::DocumentHost>,
        selection_host: sync::Arc<selection::listing::Host>
    ) {
        let config_host = config::INSTANCE.clone();
        
        let config = config_host.get();
        let document = document_host.get();
        let selection = selection_host.get();

        let render = RenderDetail::new(config.clone(), self.pango_context(), 0);
        
        let mut interior = Interior {
            document_host: document_host.clone(),
            document: document.clone(),

            selection_host: selection_host.clone(),
            selection: selection.clone(),

            charm_window: rc::Rc::downgrade(window),
            window: layout_model::Window::new(document.clone()),
            cursor: facet::cursor::CursorView::new(document.clone(), config.clone()),
            scroll: facet::scroll::Scroller::new(config.clone()),
            hover: None,
            rubber_band_begin: None,

            last_frame: match self.frame_clock() {
                Some(fc) => fc.frame_time(),
                None => 0
            },
            render: sync::Arc::new(render),

            document_update_event_source: once_cell::sync::OnceCell::new(),
            selection_update_event_source: once_cell::sync::OnceCell::new(),
            config_update_event_source: once_cell::sync::OnceCell::new(),
            work_event_source: once_cell::sync::OnceCell::new(),
            work_notifier: util::Notifier::new(),
            runtime: window.application.rt.handle().clone(),
        };

        /* Set the initial size. */
        interior.size_allocate(
            self,
            self.allocated_width(),
            self.allocated_height(),
            self.allocated_baseline());

        /* Register animation callback */
        self.add_tick_callback(|lw, frame_clock| {
            lw.imp().interior.get().unwrap().write().animate(lw, frame_clock)
        });

        /* Subscribe to document updates */
        {
            let update_subscriber = helpers::subscribe_to_updates(self.downgrade(), document_host, document, |lw, new_doc| {
                lw.document_updated(new_doc);
            });
            if interior.document_update_event_source.set(update_subscriber).is_err() {
                panic!("double-initialized document_update_event_source");
            }
        }

        /* Subscribe to selection updates */
        {
            let update_subscriber = helpers::subscribe_to_updates(self.downgrade(), selection_host, selection, |lw, new_sel| {
                lw.selection_updated(new_sel);
            });
            if interior.selection_update_event_source.set(update_subscriber).is_err() {
                panic!("double-initialized selection_update_event_source");
            }
        }

        /* Subscribe to config updates */
        {
            let update_subscriber = helpers::subscribe_to_updates(self.downgrade(), config_host, config, |lw, new_conf| {
                lw.config_updated(new_conf);
            });
            if interior.config_update_event_source.set(update_subscriber).is_err() {
                panic!("double-initialized config_update_event_source");
            }
        }

        /* Spawn work task */
        if interior.work_event_source.set(helpers::spawn_on_main_context(ListingWidgetWorkFuture(self.downgrade()))).is_err() {
            panic!("double-initialized work_event_source");
        }

        /* Register keybaord event controller */
        let ec_key = gtk::EventControllerKey::new();
        ec_key.connect_key_pressed(clone!(@weak self as lw => @default-return glib::Propagation::Proceed, move |_eck, keyval, keycode, modifier| {
            let propagation = lw.imp().interior.get().unwrap().write().key_pressed(&lw, keyval, keycode, modifier);
            propagation
        }));
        self.add_controller(ec_key);

        /* Register scroll event controller */
        let ec_scroll = gtk::EventControllerScroll::new(gtk::EventControllerScrollFlags::VERTICAL);
        ec_scroll.connect_scroll(clone!(@weak self as lw => @default-return glib::Propagation::Proceed, move |_ecs, dx, dy| {
            let propagation = lw.imp().interior.get().unwrap().write().scroll(&lw, dx, dy);
            propagation
        }));
        self.add_controller(ec_scroll);
        
        /* Register motion event controller for hovering */
        let ec_motion = gtk::EventControllerMotion::new();
        ec_motion.connect_motion(clone!(@weak self as lw => move |_ecm, x, y| {
            let propagation = lw.imp().interior.get().unwrap().write().hover(&lw, Some((x, y)));
            propagation
        }));
        ec_motion.connect_leave(clone!(@weak self as lw => move |_ecm| {
            let propagation = lw.imp().interior.get().unwrap().write().hover(&lw, None);
            propagation
        }));
        self.add_controller(ec_motion);
        
        /* Single click (grab focus & move cursor) */
        let ec_click = gtk::GestureClick::new();
        ec_click.connect_pressed(clone!(@weak self as lw => move |_gesture, _n_press, _x, _y| {
            lw.grab_focus();
        }));
        ec_click.connect_released(clone!(@weak self as lw => move |gesture, _n_press, x, y| {
            lw.imp().interior.get().unwrap().write().move_cursor_to_coordinates(x, y);
            lw.queue_draw();
            gesture.set_state(gtk::EventSequenceState::Claimed);
        }));
        ec_click.set_button(1);
        ec_click.set_exclusive(true);
        self.add_controller(ec_click.clone());

        /* Rubber-band selection */
        let ec_select = gtk::GestureDrag::new();
        ec_select.connect_drag_begin(clone!(@weak self as lw => move |_ecs, x, y| {
            lw.imp().interior.get().unwrap().write().drag_begin(&lw, x, y);
        }));
        ec_select.connect_drag_update(clone!(@weak self as lw => move |ecs, dx, dy| {
            lw.imp().interior.get().unwrap().write().drag_update(&lw, &ecs, dx, dy);
            ecs.set_state(gtk::EventSequenceState::Claimed);
        }));
        ec_select.connect_drag_end(clone!(@weak self as lw => move |ecs, dx, dy| {
            lw.imp().interior.get().unwrap().write().drag_end(&lw, &ecs, dx, dy);
        }));
        ec_select.set_button(1);
        ec_select.set_exclusive(true);
        self.add_controller(ec_select.clone());

        /* Notify cursor when focus state changes */
        self.connect_has_focus_notify(move |lw| {
            lw.imp().interior.get().unwrap().write().cursor.change_focused(lw.has_focus());
        });

        /* Finished initializing; set interior. */
        let interior = sync::Arc::new(parking_lot::RwLock::new(interior));
        self.imp().init(interior);
    }

    pub fn selection(&self) -> parking_lot::MappedRwLockReadGuard<'_, sync::Arc<selection::ListingSelection>> {
        parking_lot::RwLockReadGuard::map(self.imp().interior.get().unwrap().read(), |int| &int.selection)
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

    pub fn goto(&self, document: &sync::Arc<document::Document>, path: &structure::Path, offset: addr::Address, hint: cursor::PlacementHint) {
        let mut interior_guard = self.imp().interior.get().unwrap().write();
        let interior = &mut *interior_guard;
        
        if interior.document.generation() != document.generation() {
            interior.cursor.bonk();
            return;
        }

        interior.cursor.goto(document.clone(), path, offset, hint);
        interior.scroll.ensure_cursor_is_in_view(&mut interior.window, &mut interior.cursor, facet::scroll::EnsureCursorInViewDirection::Any);

        self.queue_draw();
    }

    fn document_updated(&self, new_document: &sync::Arc<document::Document>) {
        let mut interior = self.imp().interior.get().unwrap().write();
        interior.document_updated(new_document);

        self.queue_draw();
    }

    fn selection_updated(&self, new_selection: &sync::Arc<selection::ListingSelection>) {
        let mut interior = self.imp().interior.get().unwrap().write();
        interior.selection_updated(new_selection);

        self.queue_draw();
    }

    fn config_updated(&self, new_config: &sync::Arc<config::Config>) {
        let mut interior = self.imp().interior.get().unwrap().write();
        interior.config_updated(new_config, self);

        self.queue_draw();
    }
}

impl RenderDetail {
    fn new(config: sync::Arc<config::Config>, pg: pango::Context, serial: u64) -> RenderDetail {
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
    fn document_updated(&mut self, new_document: &sync::Arc<document::Document>) {
        self.window.update(new_document);
        self.cursor.cursor.update(new_document);
        self.document = new_document.clone();
        self.rubber_band_begin = None;

        self.work_notifier.notify();
    }

    fn selection_updated(&mut self, new_selection: &sync::Arc<selection::ListingSelection>) {
        if self.document.is_outdated(&new_selection.document) {
            self.document_updated(&new_selection.document);
        }

        self.selection = new_selection.clone();
    }

    fn config_updated(&mut self, new_config: &sync::Arc<config::Config>, widget: &ListingWidget) {
        self.scroll.reconf(new_config.clone());
        self.cursor.reconf(new_config.clone());
        self.render = sync::Arc::new(
            RenderDetail::new(
                new_config.clone(),
                widget.pango_context(),
                self.render.serial + 1));
    }
    
    /// Resize the underlying window.
    fn size_allocate(&mut self, _widget: &ListingWidget, _width: i32, height: i32, _baseline: i32) {
        let render = &self.render;
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
        
        for line in self.window.line_views.iter_mut() {
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
    
    fn animate(&mut self, widget: &ListingWidget, frame_clock: &gdk::FrameClock) -> glib::ControlFlow {
        let frame_time = frame_clock.frame_time(); // in microseconds

        if frame_time - self.last_frame > (MICROSECONDS_PER_SECOND as i64) {
            /* if we fall too far behind, just drop frames */
            self.last_frame = frame_time;
        }

        let delta = (frame_time - self.last_frame) as f64 / MICROSECONDS_PER_SECOND as f64;

        self.cursor.animate(delta);
        self.scroll.animate(&mut self.window, &self.cursor, delta);

        self.collect_events(widget);

        if widget.imp().should_repick.swap(false, sync::atomic::Ordering::Relaxed) {
            self.update_rubber_band_from_hover();
        }
        
        self.last_frame = frame_time;
        
        glib::ControlFlow::Continue
    }

    fn cursor_transaction<F>(&mut self, cb: F, dir: facet::scroll::EnsureCursorInViewDirection) -> glib::Propagation
    where F: FnOnce(&mut facet::cursor::CursorView) {
        cb(&mut self.cursor);
        self.scroll.ensure_cursor_is_in_view(&mut self.window, &self.cursor, dir);
        glib::Propagation::Stop
    }

    // TODO: bring back ensure cursor in view
    fn cursor_transaction_fallible<F>(&mut self, cb: F, dir: facet::scroll::EnsureCursorInViewDirection) -> glib::Propagation
    where F: FnOnce(&mut facet::cursor::CursorView) -> bool {
        if cb(&mut self.cursor) {
            self.scroll.ensure_cursor_is_in_view(&mut self.window, &self.cursor, dir);
            glib::Propagation::Stop
        } else {
            glib::Propagation::Proceed
        }
    }

    fn key_pressed(&mut self, widget: &ListingWidget, keyval: gdk::Key, _keycode: u32, modifier: gdk::ModifierType) -> glib::Propagation {
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
            (gdk::Key::Page_Up,   false, false) => { self.scroll.page_up(&self.window); glib::Propagation::Stop },
            (gdk::Key::Page_Down, false, false) => { self.scroll.page_down(&self.window); glib::Propagation::Stop },
            
            //_ => self.cursor_transaction_fallible(|c| c.entry(&document_host, ek), facet::scroll::EnsureCursorInViewDirection::Any),
            _ => glib::Propagation::Proceed,
        };

        self.collect_events(widget);

        r
    }

    fn scroll(&mut self, widget: &ListingWidget, _dx: f64, dy: f64) -> glib::Propagation {
        self.scroll.scroll_wheel_impulse(dy);

        self.collect_events(widget);
        
        glib::Propagation::Stop
    }

    fn drag_begin(&mut self, widget: &ListingWidget, x: f64, y: f64) {
        self.rubber_band_begin = self.pick(x, y);

        match self.selection_host.change(selection::listing::Change::Clear) {
            Ok(new_selection) => { self.selection_updated(&new_selection); },
            Err((error, attempted_version)) => { self.charm_window.upgrade().map(|window| window.report_error(error::Error {
                while_attempting: error::Action::RubberBandSelection,
                trouble: error::Trouble::ListingSelectionUpdateFailure {
                    error,
                    attempted_version,
                },
                level: error::Level::Warning,
                is_bug: true,
            })); }
        }

        widget.queue_draw();
    }

    fn update_rubber_band_from_hover(&mut self) {
        if let Some((x, y)) = self.hover {
            self.update_rubber_band(x, y);
        }
    }
    
    fn update_rubber_band(&mut self, x: f64, y: f64) {
        if let (Some(rbb), Some(rbe)) = (&self.rubber_band_begin, self.pick(x, y)) {
            match self.selection_host.change(selection::listing::Change::AssignStructure(pick::to_structure_selection(&self.document, rbb, &rbe))) {
                Ok(new_selection) => {
                    self.selection_updated(&new_selection);
                },
                Err((error, attempted_version)) => { self.charm_window.upgrade().map(|window| window.report_error(error::Error {
                    while_attempting: error::Action::RubberBandSelection,
                    trouble: error::Trouble::ListingSelectionUpdateFailure {
                        error,
                        attempted_version,
                    },
                    level: error::Level::Warning,
                    is_bug: true,
                })); }
            }
        }
    }
    
    fn drag_update(&mut self, widget: &ListingWidget, gesture: &gtk::GestureDrag, dx: f64, dy: f64) {
        if let Some((x, y)) = gesture.start_point() {
            self.update_rubber_band(x+dx, y+dy);
            widget.queue_draw();
        }
    }

    fn drag_end(&mut self, _widget: &ListingWidget, _gesture: &gtk::GestureDrag, _dx: f64, _dy: f64) {
        self.rubber_band_begin = None;
    }

    fn hover(&mut self, widget: &ListingWidget, hover: Option<(f64, f64)>) {
        self.hover = hover;
        widget.queue_draw();
    }

    fn move_cursor_to_coordinates(&mut self, x: f64, y: f64) {
        self.cursor.blink();
        
        let (path, part) = match self.pick(x, y) {
            Some(pick::Triplet {
                middle: (path, part),
                ..
            }) => (path, part),
            
            None => return,
        };

        let new_cursor = cursor::Cursor::place(self.document.clone(), &path, part.offset(), part.cursor_placement_hint());

        self.cursor.cursor = new_cursor;
        self.scroll.ensure_cursor_is_in_view(&mut self.window, &mut self.cursor, facet::scroll::EnsureCursorInViewDirection::Any)
    }
    
    fn pick_line(&self, y: f64) -> Option<(usize, f64)> {
        let descent = helpers::pango_unscale(self.render.metrics.descent()) as f64;
        let line_height = helpers::pango_unscale(self.render.metrics.height()) as f64;
        let lineno = ((y - descent) / line_height) + self.scroll.get_position();

        // TODO: if rust ever comes out with checked float -> int conversions, use that here.
        if lineno >= 0.0 {
            Some((lineno as usize, y - descent - (lineno.trunc() + self.scroll.get_position()) * line_height))
        } else {
            None
        }
    }

    fn pick(&self, x: f64, y: f64) -> Option<pick::Triplet> {
        let (lineno, y) = self.pick_line(y)?;
        let line = self.window.line_views.get(lineno)?;
        line.pick(x, y)
    }
}

impl future::Future for ListingWidgetWorkFuture {
    type Output = ();

    fn poll(self: pin::Pin<&mut Self>, cx: &mut task::Context<'_>) -> task::Poll<()> {
        if let Some(lw) = self.0.upgrade() {
            let mut interior = lw.imp().interior.get().unwrap().write();

            /* This gets polled from glib event loop, but we need to be in a tokio runtime. */
            let handle = interior.runtime.clone();
            let guard = handle.enter();
            interior.work(&lw, cx);
            std::mem::drop(guard);
            
            task::Poll::Pending
        } else {
            task::Poll::Ready(())
        }
    }
}
