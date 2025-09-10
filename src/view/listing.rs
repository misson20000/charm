use std::fmt::Write;
use std::future;
use std::pin;
use std::rc;
use std::sync;
use std::task;

use crate::catch_panic;
use crate::util;
use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;
use crate::model::listing::cursor;
use crate::model::listing::token::TokenKind;
use crate::model::listing::window as window_model;
use crate::model::listing::window::LineView;
use crate::model::selection;
use crate::model::versioned::Versioned;
use crate::view;
use crate::view::breadcrumbs;
use crate::view::crashreport;
use crate::view::config;
use crate::view::error;
use crate::view::gsc;
use crate::view::helpers;
use crate::view::window::ErrorReporter;

use gtk::gdk;
use gtk::gio;
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
pub mod mode;

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
}

enum RubberBandState {
    Inactive,
    Mouse(pick::Triplet),
    Keyboard((structure::Path, usize, addr::Offset)),
}

enum RubberBandMode {
    Structure,
    Address,
}

struct Interior {
    document_host: sync::Arc<document::DocumentHost>,
    selection_host: sync::Arc<selection::listing::Host>,
    
    document: sync::Arc<document::Document>,
    selection: sync::Arc<selection::ListingSelection>,

    charm_window: rc::Weak<view::window::CharmWindow>,
    charm_window_id: u64,
    window: window_model::Window<line::Line>,
    cursor: facet::cursor::CursorView,
    scroll: facet::scroll::Scroller,
    mode: mode::Mode,
    hover: Option<(f64, f64)>,
    rubber_band_begin: RubberBandState,
    rubber_band_mode: RubberBandMode,
    popover_menu: gtk::PopoverMenu,
    breadcrumbs: gio::ListStore,
    
    last_frame: i64,
    render: sync::Arc<RenderDetail>,

    document_update_event_source: once_cell::sync::OnceCell<helpers::AsyncSubscriber>,
    selection_update_event_source: once_cell::sync::OnceCell<helpers::AsyncSubscriber>,
    config_update_event_source: once_cell::sync::OnceCell<helpers::AsyncSubscriber>,
    work_event_source: once_cell::sync::OnceCell<helpers::AsyncSubscriber>,
    work_notifier: util::Notifier,
    work_complete_notifier: util::Notifier,
    work_incomplete: bool,
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
        catch_panic! {
            klass.add_shortcut(&gtk::Shortcut::new(
                gtk::ShortcutTrigger::parse_string("B"),
                Some(gtk::NamedAction::new("ctx.insert_byte"))));
            klass.add_shortcut(&gtk::Shortcut::new(
                gtk::ShortcutTrigger::parse_string("W"),
                Some(gtk::NamedAction::new("ctx.insert_word"))));
            klass.add_shortcut(&gtk::Shortcut::new(
                gtk::ShortcutTrigger::parse_string("D"),
                Some(gtk::NamedAction::new("ctx.insert_dword"))));
            klass.add_shortcut(&gtk::Shortcut::new(
                gtk::ShortcutTrigger::parse_string("Q"),
                Some(gtk::NamedAction::new("ctx.insert_qword"))));
            klass.add_shortcut(&gtk::Shortcut::new(
                gtk::ShortcutTrigger::parse_string("Insert"),
                Some(gtk::NamedAction::new("ctx.insert_node"))));
            klass.add_shortcut(&gtk::Shortcut::new(
                gtk::ShortcutTrigger::parse_string("G"),
                Some(gtk::NamedAction::new("ctx.goto"))));
            klass.add_shortcut(&gtk::Shortcut::new(
                gtk::ShortcutTrigger::parse_string("E"),
                /* NamedAction doesn't support detailed action strings */
                Some(gtk::CallbackAction::new(|w, _| match w.activate_action("ctx.mode", Some(&"entry".to_variant())) {
                    Ok(()) => glib::Propagation::Stop,
                    Err(_) => glib::Propagation::Proceed
                }))));
            klass.add_shortcut(&gtk::Shortcut::new(
                gtk::ShortcutTrigger::parse_string("Return"),
                /* NamedAction doesn't support detailed action strings */
                Some(gtk::CallbackAction::new(|w, _| match w.activate_action("ctx.mode", Some(&"command".to_variant())) {
                    Ok(()) => glib::Propagation::Stop,
                    Err(_) => glib::Propagation::Proceed
                }))));
            klass.add_shortcut(&gtk::Shortcut::new(
                gtk::ShortcutTrigger::parse_string("<Ctrl>C"),
                Some(gtk::NamedAction::new("ctx.copy"))));
            klass.add_shortcut(&gtk::Shortcut::new(
                gtk::ShortcutTrigger::parse_string("<Ctrl>X"),
                Some(gtk::NamedAction::new("ctx.cut"))));
            klass.add_shortcut(&gtk::Shortcut::new(
                gtk::ShortcutTrigger::parse_string("<Ctrl>V"),
                Some(gtk::NamedAction::new("ctx.paste"))));
            klass.add_shortcut(&gtk::Shortcut::new(
                gtk::ShortcutTrigger::parse_string("Delete"),
                Some(gtk::NamedAction::new("ctx.delete_selected_nodes"))));
            klass.add_shortcut(&gtk::Shortcut::new(
                gtk::ShortcutTrigger::parse_string("bracketleft"),
                Some(gtk::NamedAction::new("ctx.create_array"))));
            
            klass.set_css_name("listing");
        }
    }
}

impl ObjectImpl for ListingWidgetImp {
    fn constructed(&self) {
        catch_panic! {
            self.parent_constructed();

            self.obj().set_vexpand(true);
            self.obj().set_hexpand(true);
        }
    }

    fn properties() -> &'static [glib::ParamSpec] {
        /* FFI CALLBACK: panic-safe */
        
        static PROPERTIES: once_cell::sync::Lazy<Vec<glib::ParamSpec>> =
            once_cell::sync::Lazy::new(|| vec![
                glib::ParamSpecObject::builder::<breadcrumbs::CharmBreadcrumb>("breadcrumbs").build(),
            ]);
        PROPERTIES.as_ref()
    }

    fn property(&self, _id: usize, pspec: &glib::ParamSpec) -> glib::Value {
        catch_panic! {
            @default(glib::Value::from_type(glib::Type::INVALID));

            let Some(interior) = self.interior.get().map(|i| i.read()) else { return glib::Value::from_type(glib::Type::INVALID) };
            
            match pspec.name() {
                "breadcrumbs" => glib::Value::from(&interior.breadcrumbs),
                x => panic!("access to invalid property: {}", x)
            }
        }
    }
}

impl WidgetImpl for ListingWidgetImp {
    fn measure(&self, orientation: gtk::Orientation, _for_size: i32) -> (i32, i32, i32, i32) {
        catch_panic! {
            @default((-1, -1, -1, -1));
            
            match orientation {
                gtk::Orientation::Horizontal => {
                    let mut interior = match self.interior.get() {
                        Some(interior) => interior.write(),
                        None => return (100, 200, -1, -1),
                    };

                    interior.measure_horizontal(&self.obj())
                },
                gtk::Orientation::Vertical => {
                    (200, 1200, -1, -1)
                },
                x => panic!("invalid orientation {:?}", x)
            }
        }
    }

    fn size_allocate(&self, width: i32, height: i32, baseline: i32) {
        catch_panic! {
            let mut interior = match self.interior.get() {
                Some(interior) => interior.write(),
                None => return,
            };

            interior.size_allocate(&self.obj(), width, height, baseline);
        }
    }
    
    fn snapshot(&self, snapshot: &gtk::Snapshot) {
        catch_panic! {
            let widget = self.obj();
            let mut interior_guard = match self.interior.get() {
                Some(interior) => interior.write(),
                None => return,
            };
            let interior = &mut *interior_guard;

            let _circumstances = crashreport::circumstances([crashreport::Circumstance::InWindow(interior.charm_window_id)]);
            
            let render = &interior.render;
            
            let selection = match &interior.selection {
                sel if sync::Arc::ptr_eq(&sel.document, &interior.document) => &sel.mode,
                _ => {
                    println!("WARNING: rendering listing widget with selection that does not agree with document. this should not happen!");
                    const INVALID_MODE: selection::listing::Mode = selection::listing::Mode::Structure(selection::listing::StructureMode::Empty);

                    &INVALID_MODE
                }
            };

            snapshot.push_clip(&graphene::Rect::new(0.0, 0.0, widget.width() as f32, widget.height() as f32));
            
            /* fill in background */
            snapshot.append_color(render.config.background_color.rgba(), &graphene::Rect::new(0.0, 0.0, widget.width() as f32, widget.height() as f32));

            /* fill in address pane */
            snapshot.append_color(render.config.addr_pane_color.rgba(), &graphene::Rect::new(0.0, 0.0, render.addr_pane_width, widget.height() as f32));
            
            /* render lines */
            let lh = helpers::pango_unscale(render.metrics.height());
            snapshot.save();
            snapshot.translate(&graphene::Point::new(0.0, interior.scroll.get_position() as f32 * -lh));

            for line in interior.window.line_views.iter_mut() {
                if let Some(node) = line.render(&interior.cursor, selection, &*render) {
                    snapshot.append_node(node);
                }
                
                snapshot.translate(&graphene::Point::new(0.0, lh));
            }
            snapshot.restore();

            /* render mode line */
            let ml_pad = 5.0;
            let ml_height = lh + ml_pad * 2.0;
            snapshot.append_color(render.config.background_color.rgba(), &graphene::Rect::new(0.0, widget.height() as f32 - ml_height, widget.width() as f32, ml_height));
            gsc::begin_text(
                &render.pango,
                &render.font_mono,
                render.config.text_color.rgba(),
                &render.config,
                match &interior.mode_line_text() {
                    Ok(t) => &t,
                    Err(_) => "(failed to format mode line text)",
                },
                &mut graphene::Point::new(render.addr_pane_width + render.config.padding as f32, widget.height() as f32 - ml_pad - helpers::pango_unscale(render.metrics.descent()))
            ).render(&snapshot);

            let mode_color = if interior.cursor.has_focus {
                interior.mode.color(&render.config)
            } else {
                render.config.mode_defocused_color.rgba()
            };
            
            snapshot.append_color(mode_color, &graphene::Rect::new(0.0, widget.height() as f32 - ml_height, render.addr_pane_width, ml_height));
            gsc::begin_text(
                &render.pango,
                &render.font_bold,
                render.config.mode_text_color.rgba(),
                &render.config,
                interior.mode.name(),
                &mut graphene::Point::new(render.config.padding as f32, widget.height() as f32 - ml_pad - helpers::pango_unscale(render.metrics.descent()))
            ).render(&snapshot);

            /* draw frame outline */
            snapshot.append_color(mode_color, &graphene::Rect::new(0.0, 0.0, widget.width() as f32, 2.0));
            snapshot.append_color(mode_color, &graphene::Rect::new(0.0, 0.0, 2.0, widget.height() as f32 - ml_height));
            snapshot.append_color(mode_color, &graphene::Rect::new(0.0, widget.height() as f32 - ml_height - 2.0, widget.width() as f32, 2.0));
            snapshot.append_color(mode_color, &graphene::Rect::new(widget.width() as f32 - 2.0, 0.0, 2.0, widget.height() as f32 - ml_height));
            
            snapshot.pop();
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

pub struct ListingWidgetWorkFuture {
    widget: <ListingWidget as glib::clone::Downgrade>::Weak,
}

pub struct ListingWidgetWorkCompletionFuture {
    widget: <ListingWidget as glib::clone::Downgrade>::Weak,
}

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

        let context_menu = gio::Menu::new();
        context_menu.append(Some("Cut"), Some("ctx.cut"));
        context_menu.append(Some("Copy"), Some("ctx.copy"));
        context_menu.append(Some("Paste"), Some("ctx.paste"));
        context_menu.append(Some("Create node..."), Some("ctx.insert_node"));
        context_menu.append(Some("Create array..."), Some("ctx.create_array"));
        context_menu.append(Some("Delete nodes"), Some("ctx.delete_selected_nodes"));
        context_menu.freeze();
        
        let mut interior = Interior {
            document_host: document_host.clone(),
            document: document.clone(),

            selection_host: selection_host.clone(),
            selection: selection.clone(),

            charm_window: rc::Rc::downgrade(window),
            charm_window_id: window.id,
            window: window_model::Window::new(document.clone()),
            cursor: facet::cursor::CursorView::new(document.clone(), config.clone()),
            scroll: facet::scroll::Scroller::new(config.clone()),
            mode: mode::Mode::default(),
            hover: None,
            rubber_band_begin: RubberBandState::Inactive,
            rubber_band_mode: RubberBandMode::Address,
            popover_menu: gtk::PopoverMenu::from_model(Some(&context_menu)),
            breadcrumbs: gio::ListStore::new::<breadcrumbs::CharmBreadcrumb>(),

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
            work_complete_notifier: util::Notifier::new(),
            work_incomplete: true,
            runtime: window.application.rt.handle().clone(),
        };

        interior.popover_menu.set_parent(self);

        /* Set the initial size. */
        interior.size_allocate(
            self,
            self.allocated_width(),
            self.allocated_height(),
            self.allocated_baseline());

        /* Register animation callback */
        self.add_tick_callback(|lw, frame_clock| catch_panic! {
            @default(glib::ControlFlow::Break);
            
            lw.animate(frame_clock);

            glib::ControlFlow::Continue
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
        if interior.work_event_source.set(helpers::spawn_on_main_context(ListingWidgetWorkFuture {
            widget: self.downgrade()
        })).is_err() {
            panic!("double-initialized work_event_source");
        }

        /* Register keyboard event controller */
        let ec_key = gtk::EventControllerKey::new();
        ec_key.connect_key_pressed(clone!(#[weak(rename_to=lw)] self, #[upgrade_or] glib::Propagation::Proceed, move |_eck, keyval, keycode, modifier| catch_panic! {
            @default(glib::Propagation::Proceed);
            
            lw.imp().interior.get().unwrap().write().key_pressed(&lw, keyval, keycode, modifier)
        }));
        self.add_controller(ec_key);

        /* Register scroll event controller */
        let ec_scroll = gtk::EventControllerScroll::new(gtk::EventControllerScrollFlags::VERTICAL | gtk::EventControllerScrollFlags::KINETIC);
        ec_scroll.connect_scroll(clone!(#[weak(rename_to=lw)] self, #[upgrade_or] glib::Propagation::Proceed, move |ecs, dx, dy| catch_panic! {
            @default(glib::Propagation::Proceed);

            let source = ecs.current_event_device().map(|device| device.source());

            let kinetic_allowed = match source {
                /* scroll wheels are allowed to use kinetic scrolling */
                Some(gdk::InputSource::Mouse) => true,

                /* trackpads, trackpoints, touch screens, etc. should never use kinetic acceleration */
                _ => false,
            };
            
            lw.imp().interior.get().unwrap().write().scroll(&lw, &ecs, dx, dy, kinetic_allowed)
        }));
        ec_scroll.connect_decelerate(clone!(#[weak(rename_to=lw)] self, move |ecs, vx, vy| catch_panic! {
            lw.imp().interior.get().unwrap().write().scroll_decelerate(&lw, &ecs, vx, vy);
        }));
        self.add_controller(ec_scroll);
        
        /* Register motion event controller for hovering */
        let ec_motion = gtk::EventControllerMotion::new();
        ec_motion.connect_motion(clone!(#[weak(rename_to=lw)] self, move |_ecm, x, y| catch_panic! {
            lw.imp().interior.get().unwrap().write().hover(&lw, Some((x, y)))
        }));
        ec_motion.connect_leave(clone!(#[weak(rename_to=lw)] self, move |_ecm| catch_panic! {
            lw.imp().interior.get().unwrap().write().hover(&lw, None)
        }));
        self.add_controller(ec_motion);

        /* Context menu */
        let ec_context_menu = gtk::GestureClick::new();
        ec_context_menu.connect_pressed(clone!(#[weak(rename_to=lw)] self, move |gesture, _n_press, x, y| catch_panic! {
            let event = gesture.last_event(gesture.current_sequence().as_ref());

            if event.map(|ev| ev.triggers_context_menu()).unwrap_or(false) {
                lw.open_context_menu(x, y);
            }
        }));
        ec_context_menu.set_button(0);
        self.add_controller(ec_context_menu);
        
        /* Single click (grab focus & move cursor) */
        let ec_click = gtk::GestureClick::new();
        ec_click.connect_pressed(clone!(#[weak(rename_to=lw)] self, move |_gesture, _n_press, _x, _y| catch_panic! {
            lw.grab_focus();
        }));
        ec_click.connect_released(clone!(#[weak(rename_to=lw)] self, move |gesture, n_press, x, y| catch_panic! {
            let mut interior = lw.imp().interior.get().unwrap().write();
            interior.click_at(x, y, n_press);
            interior.collect_events(&lw);
            gesture.set_state(gtk::EventSequenceState::Claimed);
        }));
        ec_click.set_button(1);
        ec_click.set_exclusive(true);
        self.add_controller(ec_click.clone());

        /* Rubber-band selection */
        let ec_select = gtk::GestureDrag::new();
        ec_select.connect_drag_begin(clone!(#[weak(rename_to=lw)] self, move |_ecs, x, y| catch_panic! {
            lw.imp().interior.get().unwrap().write().drag_begin(&lw, x, y);
        }));
        ec_select.connect_drag_update(clone!(#[weak(rename_to=lw)] self, move |ecs, dx, dy| catch_panic! {
            lw.imp().interior.get().unwrap().write().drag_update(&lw, &ecs, dx, dy);
            ecs.set_state(gtk::EventSequenceState::Claimed);
        }));
        ec_select.connect_drag_end(clone!(#[weak(rename_to=lw)] self, move |ecs, dx, dy| catch_panic! {
            lw.imp().interior.get().unwrap().write().drag_end(&lw, &ecs, dx, dy);
        }));
        ec_select.set_button(1);
        ec_select.set_exclusive(true);
        self.add_controller(ec_select.clone());

        /* Notify cursor when focus state changes */
        self.connect_has_focus_notify(move |lw| catch_panic! {
            lw.imp().interior.get().unwrap().write().cursor.change_focused(lw.has_focus());
        });

        /* Finished initializing; set interior. */
        let interior = sync::Arc::new(parking_lot::RwLock::new(interior));
        self.imp().init(interior);
    }

    pub fn set_cursor_hidden(&self, cursor_hidden: bool) {
        self.imp().interior.get().unwrap().write().cursor.hidden = cursor_hidden;
    }
    
    pub fn used_height(&self) -> f32 {
        self.imp().interior.get().unwrap().read().used_height()
    }
    
    pub fn complete_work(&self) -> ListingWidgetWorkCompletionFuture {
        ListingWidgetWorkCompletionFuture {
            widget: self.downgrade()
        }
    }

    pub fn selection(&self) -> parking_lot::MappedRwLockReadGuard<'_, sync::Arc<selection::ListingSelection>> {
        parking_lot::RwLockReadGuard::map(self.imp().interior.get().unwrap().read(), |int| &int.selection)
    }
    
    pub fn cursor(&self) -> parking_lot::MappedRwLockReadGuard<'_, crate::model::listing::cursor::Cursor> {
        parking_lot::RwLockReadGuard::map(self.imp().interior.get().unwrap().read(), |int| &int.cursor.cursor)
    }
    
    pub fn cursor_mut(&self) -> parking_lot::MappedRwLockWriteGuard<'_, crate::model::listing::cursor::Cursor> {
        parking_lot::RwLockWriteGuard::map(self.imp().interior.get().unwrap().write(), |int| &mut int.cursor.cursor)
    }

    pub fn animate(&self, frame_clock: &gdk::FrameClock) {
        self.imp().interior.get().unwrap().write().animate(self, frame_clock)
    }
    
    pub fn bonk(&self) {
        self.imp().interior.get().unwrap().write().cursor.bonk();
    }

    pub fn goto(&self, document: &sync::Arc<document::Document>, path: &structure::Path, offset: addr::Offset, hint: cursor::PlacementHint) {
        let mut interior_guard = self.imp().interior.get().unwrap().write();
        let interior = &mut *interior_guard;

        let _circumstances = crashreport::circumstances([
            crashreport::Circumstance::InWindow(interior.charm_window_id),
            crashreport::Circumstance::Goto {
                document: document.clone(),
                path: path.clone(),
                offset,
                hint: format!("{:?}", hint),
            },
        ]);
        
        if interior.document.generation() != document.generation() {
            interior.cursor.bonk();
            return;
        }

        interior.cursor.goto(document.clone(), path, offset, hint);
        interior.scroll.ensure_cursor_is_in_view(&mut interior.window, &mut interior.cursor, facet::scroll::EnsureCursorInViewDirection::Any);
        interior.collect_events(self);
    }

    pub fn set_mode(&self, mode: mode::Mode) {
        let mut interior = self.imp().interior.get().unwrap().write();
        interior.mode = mode;
        self.queue_draw();
    }
    
    fn open_context_menu(&self, x: f64, y: f64) {
        let interior = self.imp().interior.get().unwrap().read();
        
        /*
        let (path, part) = match interior.pick(x, y) {
            Some(pick::Triplet {
                middle: (path, part),
                ..
            }) => (path, part),
            
            None => return,
    };
        */

        let popover_menu = interior.popover_menu.clone();
        
        /* Ugh. Messing with the popover menu can cause it to change the mouse focus, which immediately invokes one of our mouse focus handlers which tries to lock interior again, causing a deadlock. */
        std::mem::drop(interior);

        popover_menu.set_pointing_to(Some(&gdk::Rectangle::new(x as i32, y as i32, 1, 1)));
        popover_menu.popup();
    }

    fn document_updated(&self, new_document: &sync::Arc<document::Document>) {
        let mut interior = self.imp().interior.get().unwrap().write();
        interior.document_updated(new_document);
        interior.collect_events(self);
        self.queue_draw();
    }

    fn selection_updated(&self, new_selection: &sync::Arc<selection::ListingSelection>) {
        let mut interior = self.imp().interior.get().unwrap().write();
        interior.selection_updated(new_selection);
        interior.collect_events(self);
        self.queue_draw();
    }

    fn config_updated(&self, new_config: &sync::Arc<config::Config>) {
        let mut interior = self.imp().interior.get().unwrap().write();
        interior.config_updated(new_config, self);
        interior.collect_events(self);
        self.queue_draw();
    }

    pub fn breadcrumbs(&self) -> gio::ListStore {
        self.imp().interior.get().unwrap().read().breadcrumbs.clone()
    }
}

impl RenderDetail {
    fn new(config: sync::Arc<config::Config>, pg: pango::Context, serial: u64) -> RenderDetail {
        let fm = pg.font_map().unwrap();

        let font_mono = fm.load_font(&pg, &config.monospace_font.0).expect("expected to be able to load selected font");

        let mut desc_bold = config.monospace_font.0.clone();
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
        }
    }
}

impl Interior {
    fn document_updated(&mut self, new_document: &sync::Arc<document::Document>) {
        let _circumstances = crashreport::circumstances([
            crashreport::Circumstance::InWindow(self.charm_window_id),
            crashreport::Circumstance::LWDocumentUpdate(self.document.clone(), new_document.clone())]
        );

        self.window.update(new_document);
        self.update_breadcrumbs();
        self.cursor.cursor.update(new_document);
        self.document = new_document.clone();
        self.rubber_band_begin = RubberBandState::Inactive;

        self.request_work();
    }

    fn selection_updated(&mut self, new_selection: &sync::Arc<selection::ListingSelection>) {
        let _circumstances = crashreport::circumstances([
            crashreport::Circumstance::InWindow(self.charm_window_id),
            crashreport::Circumstance::LWSelectionUpdate(self.selection.clone(), new_selection.clone())
        ]);
        
        if self.document.is_outdated(&new_selection.document) {
            self.document_updated(&new_selection.document);
        }

        self.selection = new_selection.clone();
    }

    fn config_updated(&mut self, new_config: &sync::Arc<config::Config>, widget: &ListingWidget) {
        let _circumstances = crashreport::circumstances([
            crashreport::Circumstance::InWindow(self.charm_window_id),
        ]);

        self.scroll.reconf(new_config.clone());
        self.cursor.reconf(new_config.clone());
        self.render = sync::Arc::new(
            RenderDetail::new(
                new_config.clone(),
                widget.pango_context(),
                self.render.serial + 1));
        self.update_breadcrumbs();
    }

    fn used_height(&self) -> f32 {
        self.window.line_views.len() as f32 * helpers::pango_unscale(self.render.metrics.height())
    }
    
    fn measure_horizontal(&mut self, _widget: &ListingWidget) -> (i32, i32, i32, i32) {
        let layout = layout::LayoutController::new(0, &self.render);

        (layout.edge() as i32, layout.edge() as i32, -1, -1)
    }
    
    /// Resize the underlying window.
    fn size_allocate(&mut self, _widget: &ListingWidget, _width: i32, height: i32, _baseline: i32) {
        let _circumstances = crashreport::circumstances([
            crashreport::Circumstance::InWindow(self.charm_window_id),
        ]);

        let render = &self.render;
        let line_height = render.metrics.height();

        // TODO: replace with div_ceil when it gets stabilized
        // https://github.com/rust-lang/rust/issues/88581
        //let line_count = height.div_ceil(line_height) as usize;
        let line_count = (((height * pango::SCALE) + line_height - 1) / line_height) as usize;

        self.window.resize(line_count + (2 * self.scroll.get_lookahead()));
        self.update_breadcrumbs();
        self.request_work();
    }

    fn request_work(&mut self) {
        self.work_incomplete = true;
        self.work_notifier.notify();
    }
    
    fn work(&mut self, widget: &ListingWidget, cx: &mut task::Context) {
        let _circumstances = crashreport::circumstances([
            crashreport::Circumstance::InWindow(self.charm_window_id),
        ]);

        let mut work_needed = false;
        
        self.work_notifier.enroll(cx);

        for line in self.window.line_views.iter_mut() {
            work_needed = line.work(&self.document, cx) || work_needed;

            if line.wants_draw().collect() {
                widget.queue_draw();
            }
        }

        self.cursor.wants_work().collect();
        
        work_needed = self.cursor.work(&self.document, cx) || work_needed;
        
        if self.cursor.wants_draw().collect() {
            widget.queue_draw();
        }

        self.work_complete_notifier.notify();

        self.work_incomplete = work_needed;
    }

    fn collect_events(&mut self, widget: &ListingWidget) {
        let _circumstances = crashreport::circumstances([
            crashreport::Circumstance::InWindow(self.charm_window_id),
        ]);

        self.cursor.collect_events(widget, &self.work_notifier, &mut self.work_incomplete);
        self.scroll.collect_events(widget, &self.work_notifier, &mut self.work_incomplete);
    }
    
    fn animate(&mut self, widget: &ListingWidget, frame_clock: &gdk::FrameClock) {
        let _circumstances = crashreport::circumstances([
            crashreport::Circumstance::InWindow(self.charm_window_id),
        ]);

        let frame_time = frame_clock.frame_time(); // in microseconds

        if frame_time - self.last_frame > (MICROSECONDS_PER_SECOND as i64) {
            /* if we fall too far behind, just drop frames */
            self.last_frame = frame_time;
        }

        let delta = (frame_time - self.last_frame) as f64 / MICROSECONDS_PER_SECOND as f64;

        self.cursor.animate(delta);
        self.scroll.animate(&mut self.window, &self.cursor, delta);

        self.collect_events(widget);

        self.update_breadcrumbs();
        
        if widget.imp().should_repick.swap(false, sync::atomic::Ordering::Relaxed) {
            self.update_rubber_band_from_hover();
        }
        
        self.last_frame = frame_time;
    }

    fn cursor_entry<F, T>(&mut self, cb: F, arg: T) -> glib::Propagation
    where F: FnOnce(&mut facet::cursor::CursorView, &document::DocumentHost, T) -> Result<(), cursor::EntryError> {
        match cb(&mut self.cursor, &*self.document_host, arg) {
            Err(cursor::EntryError::ChangeError { err, document }) => { self.charm_window.upgrade().map(|window| window.report_error(error::Error {
                while_attempting: error::Action::DataEntry,
                trouble: error::Trouble::DocumentUpdateFailure {
                    error: err,
                    attempted_version: document,
                },
                level: error::Level::Warning,
                is_bug: false,
            })); },
            Err(e) => println!("entry error: {:?}", e),
            _ => (),
        }
        
        self.scroll.ensure_cursor_is_in_view(&mut self.window, &self.cursor, facet::scroll::EnsureCursorInViewDirection::Down);
        glib::Propagation::Stop
    }
    
    fn cursor_transaction<F>(&mut self, shift: bool, cb: F, dir: facet::scroll::EnsureCursorInViewDirection) -> glib::Propagation
    where F: FnOnce(&mut facet::cursor::CursorView) {
        self.cursor_transaction_fallible(shift, |c| { cb(c); true }, dir)
    }

    // TODO: bring back ensure cursor in view
    fn cursor_transaction_fallible<F>(&mut self, shift: bool, cb: F, dir: facet::scroll::EnsureCursorInViewDirection) -> glib::Propagation
    where F: FnOnce(&mut facet::cursor::CursorView) -> bool {
        if shift {
            match self.rubber_band_begin {
                RubberBandState::Inactive => self.rubber_band_begin = RubberBandState::Keyboard(self.cursor.endpoint_for_rubber_band()),
                _ => {},
            }
        } else {
            self.rubber_band_begin = RubberBandState::Inactive;
        }
        
        if cb(&mut self.cursor) {
            self.scroll.ensure_cursor_is_in_view(&mut self.window, &self.cursor, dir);

            if let RubberBandState::Keyboard(rbb) = &self.rubber_band_begin {
                let pos = self.cursor.endpoint_for_rubber_band();

                let sel = selection::listing::StructureMode::Range(
                    selection::listing::StructureRange::between(
                        &*self.cursor.cursor.document(),
                        selection::listing::StructureEndpoint::borrow(rbb),
                        selection::listing::StructureEndpoint::borrow(&pos)));
                
                match self.selection_host.change(selection::listing::Change::AssignStructure(sel)) {
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
            
            glib::Propagation::Stop
        } else {
            glib::Propagation::Proceed
        }
    }

    fn key_pressed(&mut self, widget: &ListingWidget, keyval: gdk::Key, _keycode: u32, modifier: gdk::ModifierType) -> glib::Propagation {
        let _circumstances = crashreport::circumstances([
            crashreport::Circumstance::InWindow(self.charm_window_id),
        ]);

        let r = match (&self.mode, keyval, modifier.intersects(gdk::ModifierType::SHIFT_MASK), modifier.intersects(gdk::ModifierType::CONTROL_MASK)) {
            /* basic cursor movement keys */
            /*  key              shift  ctrl  */
            (_, gdk::Key::Left,  shift, false) => self.cursor_transaction(shift, |c| c.move_left(),        facet::scroll::EnsureCursorInViewDirection::Up),
            (_, gdk::Key::Right, shift, false) => self.cursor_transaction(shift, |c| c.move_right(),       facet::scroll::EnsureCursorInViewDirection::Down),
            (_, gdk::Key::Left,  shift, true)  => self.cursor_transaction(shift, |c| c.move_left_large(),  facet::scroll::EnsureCursorInViewDirection::Up),
            (_, gdk::Key::Right, shift, true)  => self.cursor_transaction(shift, |c| c.move_right_large(), facet::scroll::EnsureCursorInViewDirection::Down),
            (_, gdk::Key::Up,    shift, false) => self.cursor_transaction(shift, |c| c.move_up(),          facet::scroll::EnsureCursorInViewDirection::Up),
            (_, gdk::Key::Down,  shift, false) => self.cursor_transaction(shift, |c| c.move_down(),        facet::scroll::EnsureCursorInViewDirection::Down),

            /* basic scroll keys */
            /* key                   shift  ctrl  */
            (_, gdk::Key::Page_Up,   false, false) => { self.scroll.page_up(&self.window); glib::Propagation::Stop },
            (_, gdk::Key::Page_Down, false, false) => { self.scroll.page_down(&self.window); glib::Propagation::Stop },
            
            /* hex entry */
            (mode::Mode::Entry, gdk::Key::_0, false, false) => self.cursor_entry(facet::cursor::CursorView::enter_hex, 0x0),
            (mode::Mode::Entry, gdk::Key::_1, false, false) => self.cursor_entry(facet::cursor::CursorView::enter_hex, 0x1),
            (mode::Mode::Entry, gdk::Key::_2, false, false) => self.cursor_entry(facet::cursor::CursorView::enter_hex, 0x2),
            (mode::Mode::Entry, gdk::Key::_3, false, false) => self.cursor_entry(facet::cursor::CursorView::enter_hex, 0x3),
            (mode::Mode::Entry, gdk::Key::_4, false, false) => self.cursor_entry(facet::cursor::CursorView::enter_hex, 0x4),
            (mode::Mode::Entry, gdk::Key::_5, false, false) => self.cursor_entry(facet::cursor::CursorView::enter_hex, 0x5),
            (mode::Mode::Entry, gdk::Key::_6, false, false) => self.cursor_entry(facet::cursor::CursorView::enter_hex, 0x6),
            (mode::Mode::Entry, gdk::Key::_7, false, false) => self.cursor_entry(facet::cursor::CursorView::enter_hex, 0x7),
            (mode::Mode::Entry, gdk::Key::_8, false, false) => self.cursor_entry(facet::cursor::CursorView::enter_hex, 0x8),
            (mode::Mode::Entry, gdk::Key::_9, false, false) => self.cursor_entry(facet::cursor::CursorView::enter_hex, 0x9),
            (mode::Mode::Entry, gdk::Key::a , false, false) => self.cursor_entry(facet::cursor::CursorView::enter_hex, 0xa),
            (mode::Mode::Entry, gdk::Key::b , false, false) => self.cursor_entry(facet::cursor::CursorView::enter_hex, 0xb),
            (mode::Mode::Entry, gdk::Key::c , false, false) => self.cursor_entry(facet::cursor::CursorView::enter_hex, 0xc),
            (mode::Mode::Entry, gdk::Key::d , false, false) => self.cursor_entry(facet::cursor::CursorView::enter_hex, 0xd),
            (mode::Mode::Entry, gdk::Key::e , false, false) => self.cursor_entry(facet::cursor::CursorView::enter_hex, 0xe),
            (mode::Mode::Entry, gdk::Key::f , false, false) => self.cursor_entry(facet::cursor::CursorView::enter_hex, 0xf),

            /* clear selection */
            (_, gdk::Key::Return, false, false) => {
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
                };
                
                glib::Propagation::Proceed
            }
            
            _ => glib::Propagation::Proceed,
        };

        self.collect_events(widget);

        r
    }

    fn scroll(&mut self, widget: &ListingWidget, ecs: &gtk::EventControllerScroll, _dx: f64, dy: f64, kinetic_allowed: bool) -> glib::Propagation {
        let _circumstances = crashreport::circumstances([
            crashreport::Circumstance::InWindow(self.charm_window_id),
        ]);
        
        self.scroll.scroll_wheel_impulse(dy / facet::scroll::ScrollUnit::from(ecs).divisor(&self.render), kinetic_allowed);

        self.collect_events(widget);

        self.update_breadcrumbs();
        
        glib::Propagation::Stop
    }

    fn scroll_decelerate(&mut self, widget: &ListingWidget, ecs: &gtk::EventControllerScroll, _vx: f64, vy: f64) {
        let _circumstances = crashreport::circumstances([
            crashreport::Circumstance::InWindow(self.charm_window_id),
        ]);

        /* gtk reports velocity in pixels/ms, we expect "units"/second */
        self.scroll.scroll_decelerate(vy / facet::scroll::ScrollUnit::from(ecs).divisor(&self.render));

        self.collect_events(widget);

        self.update_breadcrumbs();
    }
    
    fn update_breadcrumbs(&self) {
        let mut tok = None;
        
        for i in (0..(self.scroll.get_position() as usize) + 1).rev() {
            tok = self.window.line_views.get(i).and_then(|l| l.iter_tokens().next());
            
            if tok.is_some() {
                break;
            }
        }
        
        let Some(tok) = tok else {
            self.breadcrumbs.remove_all();
            return;
        };

        let doc = &self.window.current_document;
        let path = tok.node_path();

        for i in 0..=path.len() {
            match self.breadcrumbs.item(i as u32) {
                Some(existing) => {
                    existing.downcast::<breadcrumbs::CharmBreadcrumb>().unwrap().update(&doc, &path[0..i]);
                },
                None => {
                    self.breadcrumbs.append(&breadcrumbs::CharmBreadcrumb::new(doc.clone(), path[0..i].iter().cloned().collect()));
                }
            }
        }

        if self.breadcrumbs.n_items() as usize > path.len() + 1 {
            let additions: &[glib::Object] = &[];
            self.breadcrumbs.splice((path.len() + 1) as u32, self.breadcrumbs.n_items() - (path.len() + 1) as u32, additions);
        }
    }
    
    fn drag_begin(&mut self, widget: &ListingWidget, x: f64, y: f64) {
        let _circumstances = crashreport::circumstances([
            crashreport::Circumstance::InWindow(self.charm_window_id),
        ]);

        self.rubber_band_begin = match self.pick(x, y) {
            None => RubberBandState::Inactive,
            Some(triplet) => RubberBandState::Mouse(triplet)
        };

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
        if let (RubberBandState::Mouse(rbb), Some(rbe)) = (&self.rubber_band_begin, self.pick(x, y)) {
            match self.selection_host.change(match self.rubber_band_mode {
                RubberBandMode::Structure => selection::listing::Change::AssignStructure(
                    pick::to_structure_selection(&self.document, rbb, &rbe)
                ),
                RubberBandMode::Address => selection::listing::Change::AssignAddress(
                    pick::to_address_selection(&self.document, rbb, &rbe)
                ),
            }) {
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
        let _circumstances = crashreport::circumstances([
            crashreport::Circumstance::InWindow(self.charm_window_id),
        ]);

        if let Some((x, y)) = gesture.start_point() {
            self.update_rubber_band(x+dx, y+dy);
            widget.queue_draw();
        }
    }

    fn drag_end(&mut self, _widget: &ListingWidget, _gesture: &gtk::GestureDrag, _dx: f64, _dy: f64) {
        self.rubber_band_begin = RubberBandState::Inactive;
    }

    fn hover(&mut self, widget: &ListingWidget, hover: Option<(f64, f64)>) {
        self.hover = hover;
        widget.queue_draw();
    }

    fn click_at(&mut self, x: f64, y: f64, n_press: i32) {
        let _circumstances = crashreport::circumstances([
            crashreport::Circumstance::InWindow(self.charm_window_id),
        ]);

        self.cursor.blink();
        
        let (path, part) = match self.pick(x, y) {
            Some(pick::Triplet {
                middle: (path, part),
                ..
            }) => (path, part),
            
            None => return,
        };

        let new_cursor = cursor::Cursor::place(self.document.clone(), &path, part.offset(), part.cursor_placement_hint());
        
        self.cursor.set(new_cursor);
        self.scroll.ensure_cursor_is_in_view(&mut self.window, &mut self.cursor, facet::scroll::EnsureCursorInViewDirection::Any);

        if n_press >= 2 {
            let n_press = n_press as usize;
            let sel_node_path = &path[0..(path.len()-std::cmp::min(path.len(), n_press-2))];

            match self.selection_host.change(match self.rubber_band_mode {
                RubberBandMode::Structure => selection::listing::Change::AssignStructure(
                    selection::listing::StructureMode::entire_node(&*self.document, sel_node_path)
                ),
                RubberBandMode::Address => {
                    let (node, addr) = self.document.lookup_node(sel_node_path);
                    selection::listing::Change::AssignAddress(addr::AbsoluteExtent::sized(addr, node.size))
                },
            }) {
                Ok(new_selection) => {
                    self.selection_updated(&new_selection);
                },
                Err((error, attempted_version)) => { self.charm_window.upgrade().map(|window| window.report_error(error::Error {
                    while_attempting: error::Action::DoubleClickSelection,
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

    fn mode_line_text(&self) -> Result<String, std::fmt::Error> {
        let mut s = format!("{}", self.cursor.cursor.addr());

        match &self.selection.mode {
            selection::listing::Mode::Address(extent) if !extent.is_empty() => {
                write!(s, " ({}:{})", extent.begin, extent.len())?;
            },
            selection::listing::Mode::Address(_) => {},
            selection::listing::Mode::Structure(selection::listing::StructureMode::Empty) => {},
            selection::listing::Mode::Structure(selection::listing::StructureMode::Range(range)) => {
                let (_, addr) = self.selection.document.lookup_node(&range.path);
                let extent = range.extent().absolute_from(addr);
                write!(s, " ({}:{})", extent.begin, extent.len())?;
            },
            selection::listing::Mode::Structure(selection::listing::StructureMode::All) => {
                write!(s, " (all)")?;
            },
        };

        Ok(s)
    }
}

impl future::Future for ListingWidgetWorkFuture {
    type Output = ();

    fn poll(self: pin::Pin<&mut Self>, cx: &mut task::Context<'_>) -> task::Poll<()> {
        catch_panic! {
            @default(task::Poll::Ready(()));
        
            if let Some(lw) = self.widget.upgrade() {
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
}

impl future::Future for ListingWidgetWorkCompletionFuture {
    type Output = ();

    fn poll(self: pin::Pin<&mut Self>, cx: &mut task::Context<'_>) -> task::Poll<()> {
        catch_panic! {
            @default(task::Poll::Ready(()));
        
            if let Some(lw) = self.widget.upgrade() {
                let interior = lw.imp().interior.get().unwrap().read();

                if interior.work_incomplete {
                    let handle = interior.runtime.clone();
                    let guard = handle.enter();
                    interior.work_complete_notifier.enroll(cx);
                    std::mem::drop(guard);
                    
                    task::Poll::Pending
                } else {
                    task::Poll::Ready(())
                }
            } else {
                task::Poll::Ready(())
            }
        }
    }
}
