use std::sync;
use std::task;
use std::vec;
use std::time;
use std::collections::HashMap;

use crate::config;
use crate::util;
use crate::model::addr;
use crate::model::datapath;
use crate::model::datapath::DataPathExt;
use crate::model::document;
use crate::model::document::brk;
use crate::model::listing;
use crate::model::listing::cursor;
use crate::model::listing::line_group;
use crate::view;

use crate::ext::CairoExt;

use gtk::prelude::*;

mod action;
mod component;

const MICROSECONDS_PER_SECOND: f64 = 1000000.0;
const MICROSECONDS_PER_SECOND_INT: i64 = 1000000;

struct InternalRenderingContext<'a> {
    cr: &'a cairo::Context,
    cfg: &'a config::Config,
    fonts: Fonts,
    layout: Layout,
    selection: Option<addr::Extent>,
    
    perf: time::Instant,

    byte_cache: vec::Vec<datapath::ByteRecord>,
    
    pad: f64,
}

impl<'a> InternalRenderingContext<'a> {
    fn font_extents(&self) -> &cairo::FontExtents {
        &self.fonts.extents
    }
}

#[derive(Clone)]
struct Fonts {
    mono_face: cairo::FontFace,
    bold_face: cairo::FontFace,
    mono_scaled: cairo::ScaledFont,
    bold_scaled: cairo::ScaledFont,

    extents: cairo::FontExtents,
}

impl Fonts {
    fn new(cfg: &config::Config) -> Fonts {
        let mono_face = cairo::FontFace::toy_create("monospace", cairo::FontSlant::Normal, cairo::FontWeight::Normal).unwrap();
        let bold_face = cairo::FontFace::toy_create("monospace", cairo::FontSlant::Normal, cairo::FontWeight::Bold).unwrap();
        
        let mut font_matrix = cairo::Matrix::identity();
        font_matrix.scale(cfg.font_size, cfg.font_size);
        let ctm = cairo::Matrix::identity();
        let options = cairo::FontOptions::new().unwrap();

        /* TODO: font rewrite */
        let mono_scaled = cairo::ScaledFont::new(&mono_face, &font_matrix, &ctm, &options).unwrap();
        let bold_scaled = cairo::ScaledFont::new(&bold_face, &font_matrix, &ctm, &options).unwrap();
        
        Fonts {
            extents: mono_scaled.extents(),
            
            mono_face,
            bold_face,
            mono_scaled,
            bold_scaled,
        }
    }
}

#[derive(Clone)]
struct Layout {
    width: f64,
    height: f64,
    addr_pane_width: f64,
}

struct LineCacheEntry {
    surface: cairo::Surface,
    pattern: cairo::SurfacePattern,
    touched: bool,
}

#[derive(PartialEq)]
enum AddressEstimationBias {
    Strict,
    Left,
    Right,
    AwayFrom(addr::Address),
}

struct ModeTheme {
    accent: gdk::RGBA,
    counter_accent: gdk::RGBA,
    mode_string: &'static str,
}

pub struct ListingWidget {
    document_host: sync::Arc<document::DocumentHost>,
    
    window: listing::window::FixedWindow,

    update_task: Option<tokio::task::JoinHandle<()>>,
    update_notifier: util::Notifier,
    has_updated: bool, // signal from update thread back to gui thread

    da: send_wrapper::SendWrapper<gtk::DrawingArea>,
    action_group: send_wrapper::SendWrapper<Option<gio::SimpleActionGroup>>,
    
    last_config_version: usize,
    last_animation_time: i64,

    fonts: send_wrapper::SendWrapper<Fonts>,
    layout: Layout,
    cursor_view: component::cursor::CursorView,
    scroll: component::scroll::Scroller,

    line_cache: HashMap<listing::line_group::CacheId, send_wrapper::SendWrapper<LineCacheEntry>>,
    last_frame_duration: Option<time::Duration>,

    hover: (f64, f64),
    drag_start_address: Option<addr::Address>,
    selection: Option<addr::Extent>,
}

impl ListingWidget {
    pub fn new(cw: &view::window::CharmWindow, document_host: &sync::Arc<document::DocumentHost>) -> sync::Arc<parking_lot::RwLock<ListingWidget>> {
        let cfg = config::get();
        let document_host_clone = document_host.clone();
        let document = document_host.get_document();
        
        let rc = sync::Arc::new(parking_lot::RwLock::new(ListingWidget {
            document_host: document_host_clone,
            window: listing::window::FixedWindow::new(&document),

            update_task: None,
            update_notifier: util::Notifier::new(),
            has_updated: false,

            da: send_wrapper::SendWrapper::new(gtk::DrawingArea::new()), // TODO: split out model (things that update) from view (this)
            action_group: send_wrapper::SendWrapper::new(None),
            
            last_config_version: 0,
            last_animation_time: 0,

            fonts: send_wrapper::SendWrapper::new(Fonts::new(&cfg)),
            layout: Layout {
                width: 0.0,
                height: 0.0,
                addr_pane_width: 0.0,
            },
            cursor_view: component::cursor::CursorView::new(&document_host.get_document()),
            scroll: component::scroll::Scroller::new(),

            line_cache: HashMap::new(),
            last_frame_duration: None,

            hover: (0.0, 0.0),
            drag_start_address: None,
            selection: None,
        }));

        let mut lw = rc.write();

        lw.update_task = Some(cw.application.rt.spawn(ListingWidgetFuture::new(&rc)));
        
        /* Events I might be interested in
        O connect_button_press_event
        - connect_button_release_event
        - connect_configure_event
        O connect_draw
        O connect_focus_in_event (animate cursor)
        O connect_focus_out_event (animate cursor)
        O connect_key_press_event
        - connect_key_release_event
        - connect_popup_menu
        - connect_screen_changed (for hidpi)
        O connect_scroll_event
        - connect_selection_clear_event
        - connect_selection_get
        - connect_selection_notify_event
        - connect_selection_received
        - connect_selection_request_event
        O connect_size_allocate
         */

        /* signals */
        { let rc_clone = rc.clone(); lw.da.connect_button_press_event(move |_, eb| rc_clone.write().button_event(eb)); }
        { let rc_clone = rc.clone(); lw.da.connect_button_release_event(move |_, eb| rc_clone.write().button_event(eb)); }
        { let rc_clone = rc.clone(); lw.da.connect_draw(move |_, cr| rc_clone.write().draw(cr)); }
        { let rc_clone = rc.clone(); lw.da.connect_focus_in_event(move |_, ef| rc_clone.write().focus_change_event(ef)); }
        { let rc_clone = rc.clone(); lw.da.connect_focus_out_event(move |_, ef| rc_clone.write().focus_change_event(ef)); }
        { let rc_clone = rc.clone(); lw.da.connect_key_press_event(move |_, ek| rc_clone.write().key_press_event(ek)); }
        { let rc_clone = rc.clone(); lw.da.connect_motion_notify_event(move |_, em| rc_clone.write().motion_notify_event(em)); }
        { let rc_clone = rc.clone(); lw.da.connect_scroll_event(move |_, es| rc_clone.write().scroll_event(es)); }
        { let rc_clone = rc.clone(); lw.da.connect_size_allocate(move |_, al| rc_clone.write().size_allocate(al)); }
        { let rc_clone = rc.clone(); lw.da.add_tick_callback(move |_, fc| rc_clone.write().tick_callback(fc)); }

        lw.da.add_events(
            gdk::EventMask::FOCUS_CHANGE_MASK |
            gdk::EventMask::SCROLL_MASK |
            gdk::EventMask::SMOOTH_SCROLL_MASK |
            gdk::EventMask::KEY_PRESS_MASK |
            gdk::EventMask::KEY_RELEASE_MASK |
            gdk::EventMask::BUTTON_PRESS_MASK |
            gdk::EventMask::BUTTON_RELEASE_MASK |
            gdk::EventMask::POINTER_MOTION_MASK
        );

        /* misc flags */
        lw.da.set_can_focus(true);
        lw.da.set_focus_on_click(true);

        /* actions */
        let ag = gio::SimpleActionGroup::new();
        ag.add_action(&action::export_ips::create(&rc, &lw));
        ag.add_action(&action::goto::create(&rc, &lw));
        ag.add_action(&action::insert_break::create(&rc, &lw));

        ag.add_action(&action::movement::create_goto_start_of_line(&rc, &lw));
        ag.add_action(&action::movement::create_goto_end_of_line(&rc, &lw));
        ag.add_action(&action::mode::create_mode(&rc, &lw));
        ag.add_action(&action::mode::create_insert_mode(&rc, &lw));

        *lw.action_group = Some(ag);
        
        lw.reconfigure(&config::get());

        std::mem::drop(lw);

        rc
    }

    pub fn get_drawing_area(&self) -> &gtk::DrawingArea {
        &self.da
    }

    pub fn get_action_group(&self) -> Option<&gio::SimpleActionGroup> {
        self.action_group.as_ref()
    }
    
    pub fn draw<'a>(&'a mut self, cr: &cairo::Context) -> gtk::Inhibit {
        let frame_begin = time::Instant::now();
        
        let cfg = config::get();
        
        let mut irc = InternalRenderingContext {
            cr,
            cfg: &cfg,
            fonts: (*self.fonts).clone(),
            layout: self.layout.clone(),
            selection: self.selection.clone(),

            perf: frame_begin,
            byte_cache: vec::Vec::new(),

            pad: cfg.padding, /* it is helpful to have a shorthand for this one */
        };

        /* fill background */
        cr.set_source_gdk_rgba(irc.cfg.background_color);
        cr.paint().unwrap();

        /* fill address pane */
        cr.set_source_gdk_rgba(cfg.addr_pane_color);
        cr.rectangle(0.0, 0.0, self.layout.addr_pane_width, self.layout.height);
        cr.fill().unwrap();

        /* render lines */
        cr.save().unwrap();
        cr.translate(0.0, irc.pad + irc.font_extents().height * -self.scroll.get_position());
        self.draw_line_groups(&mut irc);
        cr.restore().unwrap();

        /* render mode line */
        cr.save().unwrap();
        cr.translate(0.0, self.layout.height - irc.font_extents().height - cfg.mode_line_padding * 2.0);
        cr.rectangle(0.0, 0.0, self.layout.width, irc.font_extents().height + cfg.mode_line_padding * 2.0);
        cr.clip();
        self.draw_mode_line(&mut irc);
        cr.restore().unwrap();

        /* stroke frame */
        cr.set_source_gdk_rgba(self.get_mode_theme(&irc).accent);
        cr.set_line_width(2.0);
        cr.rectangle(1.0, 1.0, self.layout.width - 2.0, self.layout.height - irc.font_extents().height - cfg.mode_line_padding * 2.0 - 2.0);
        cr.stroke().unwrap();
        
        /* DEBUG */
        /*
        let debug = vec![
            format!("last frame duration: {}", self.last_frame_duration.map(|d| d.as_micros() as f64).unwrap_or(0.0) / 1000.0),
            format!("document datapath generation: {}", self.document_host.get_document().get_datapath_generation_for_debug()),
        ];
        
        let mut lineno = 0;
        cr.set_scaled_font(&irc.fonts.mono_scaled);
        for group in debug {
            for line in group.split("\n") {
                cr.set_source_rgba(1.0, 1.0, 0.0, 1.0);
                cr.move_to(850.0, 40.0 + (irc.font_extents().height * lineno as f64));
                cr.show_text(&line);
                lineno+= 1;
            }
        }
         */

        self.last_frame_duration = Some(time::Instant::now() - frame_begin);
        
        gtk::Inhibit(false)
    }

    fn draw_line_groups(&mut self, irc: &mut InternalRenderingContext) {
        let hover = self.estimate_address_at(self.hover.0, self.hover.1, AddressEstimationBias::Strict);
        
        for (lineno, lg) in self.window.iter() {
            irc.cr.save().unwrap();
            irc.cr.translate(0.0, irc.font_extents().height * (lineno as f64));

            match self.selection {
                Some(sel) => lg.draw_selection(irc, irc.cr, &sel),
                None => ()
            };

            match hover {
                Some(hover) => lg.draw_hover(irc, irc.cr, hover),
                None => ()
            };
            
            let da = (*self.da).clone();
            let lc = &mut self.line_cache;
            match (lg.get_cache_id().and_then(|cache_id| {
                match lc.entry(cache_id) {
                    std::collections::hash_map::Entry::Occupied(occu) => Some(occu.into_mut()),
                    std::collections::hash_map::Entry::Vacant(vac) => {
                        
                        /* This line is not in our cache, so try to render
                         * it. If we fail, no big deal- just draw the line
                         * straight to the screen. */
                        
                        da.parent_window().and_then(|window| {
                            /* super important for performance! avoid uploading to GPU every frame! */
                            window.create_similar_surface(cairo::Content::ColorAlpha, irc.layout.width as i32, (irc.fonts.extents.height + irc.fonts.extents.ascent + irc.fonts.extents.descent) as i32)
                        }).and_then(|surface| {
                            /* TODO: report this error */
                            let cache_cr = cairo::Context::new(&surface).unwrap();
                            
                            lg.draw(irc, &cache_cr, None);

                            surface.flush();
                            
                            Some(vac.insert(send_wrapper::SendWrapper::new(LineCacheEntry {
                                pattern: cairo::SurfacePattern::create(&surface),
                                surface,
                                touched: true
                            })))
                        })
                    }
                }
            }), if lg == self.cursor_view.cursor.get_line_group() { Some(&self.cursor_view) } else { None }) {
                /* We either found it in the cache, or just rendered it to the cache and are ready to use the cached image. */
                (Some(entry), None) => { /* don't use cached lines if the cursor is on them */
                    entry.touched = true;
                    irc.cr.set_source(&entry.pattern).unwrap();
                    irc.cr.paint().unwrap();
                    
                },
                /* Line was not cacheable, errored trying to allocate surface to put it in cache, or the cursor is on this line. Render it directly. */
                (_, has_cursor) => {
                    lg.draw(irc, irc.cr, has_cursor);
                }
            }

            irc.cr.restore().unwrap();
        }

        /* evict stale entries */
        self.line_cache.retain(|_, lce| std::mem::replace(&mut lce.touched, false));
    }

    fn get_mode_theme(&self, irc: &InternalRenderingContext) -> ModeTheme {
        let mut mt = match self.cursor_view.mode {
            component::cursor::Mode::Command => ModeTheme {
                accent: irc.cfg.mode_command_color,
                counter_accent: irc.cfg.background_color,
                mode_string: "COMMAND",
            },
            component::cursor::Mode::Entry => ModeTheme {
                accent: irc.cfg.mode_entry_color,
                counter_accent: irc.cfg.background_color,
                mode_string: if self.cursor_view.insert { "BYTE INSERT" } else { "BYTE ENTRY" },
            },
            component::cursor::Mode::TextEntry => ModeTheme {
                accent: irc.cfg.mode_text_entry_color,
                counter_accent: irc.cfg.background_color,
                mode_string: if self.cursor_view.insert { "TEXT INSERT" } else { "TEXT ENTRY" },
            }
        };

        if !self.cursor_view.has_focus {
            mt.accent = irc.cfg.mode_defocused_color;
        }

        mt
    }
    
    fn draw_mode_line(&self, irc: &mut InternalRenderingContext) {
        let pad = irc.cfg.mode_line_padding;
        
        irc.cr.set_source_gdk_rgba(irc.cfg.mode_line_color);
        irc.cr.paint().unwrap();

        irc.cr.set_source_gdk_rgba(irc.cfg.text_color);
        irc.cr.move_to(self.layout.addr_pane_width + irc.pad, pad + irc.font_extents().ascent);
        irc.cr.set_scaled_font(&irc.fonts.bold_scaled);

        irc.cr.show_text(&format!("{}", self.cursor_view.cursor.get_addr())).unwrap();

        /* left part */
        {
            let mt = self.get_mode_theme(irc);
            
            irc.cr.set_source_gdk_rgba(mt.accent);
            irc.cr.rectangle(0.0, 0.0, self.layout.addr_pane_width, irc.font_extents().height + pad * 2.0);
            irc.cr.fill().unwrap();
        
            irc.cr.set_source_gdk_rgba(mt.counter_accent);
            irc.cr.move_to(irc.pad, pad + irc.font_extents().ascent);
            irc.cr.set_scaled_font(&irc.fonts.bold_scaled);

            irc.cr.show_text(mt.mode_string).unwrap();
        }
    }
    
    fn collect_events(&mut self) {
        self.cursor_view.events.collect_draw(&self.da);
        self.scroll.events.collect_draw(&self.da);

        let mut wants_update = false;
        wants_update = self.cursor_view.events.collect_update() || wants_update;
        wants_update = self.scroll.events.collect_update() || wants_update;
        wants_update = std::mem::replace(&mut self.window.wants_update, false) || wants_update;

        if wants_update {
            self.update_notifier.notify();
        }
    }
    
    fn scroll_event(&mut self, es: &gdk::EventScroll) -> gtk::Inhibit {
        self.scroll.scroll_wheel_impulse(es.delta().1);
        self.collect_events();
        
        gtk::Inhibit(true)
    }
    
    fn tick_callback(&mut self, fc: &gdk::FrameClock) -> Continue {
        let cfg = config::get();

        if cfg.version > self.last_config_version {
            self.reconfigure(&cfg);
        }

        // TODO: use idle_add to trigger this from update task
        if self.has_updated {
            self.da.queue_draw();
            self.has_updated = false;
        }

        if fc.frame_time() - self.last_animation_time > (MICROSECONDS_PER_SECOND as i64) {
            /* if we fall too far behind, just drop frames */
            self.last_animation_time = fc.frame_time();
        }

        while self.last_animation_time < fc.frame_time() {
            let ais_micros = std::cmp::min(fc.frame_time() - self.last_animation_time, MICROSECONDS_PER_SECOND_INT / 20); /* don't go below 20 TPS or the integration error gets bad */
            let ais:f64 = ais_micros as f64 / MICROSECONDS_PER_SECOND;

            self.scroll.animate(&mut self.window, &self.cursor_view, ais);
            self.cursor_view.animate(&cfg, ais);

            /* do selection */
            if let Some(a) = self.drag_start_address {
                if let Some(na) = self.estimate_address_at(self.hover.0, self.hover.1, AddressEstimationBias::AwayFrom(a)) {
                    self.selection = if a == na {
                        None
                    } else {
                        Some(addr::Extent::between_bidirectional(a, na))
                    };
                    self.da.queue_draw();
                }
            }
            
            self.last_animation_time+= ais_micros;
        }

        self.collect_events();
        
        Continue(true)
    }

    fn reconfigure(&mut self, cfg: &config::Config) {
        self.fonts = send_wrapper::SendWrapper::new(Fonts::new(cfg));

        self.layout.width = self.da.allocated_width() as f64;
        self.layout.height = self.da.allocated_height() as f64;
        self.layout.addr_pane_width = {
            let addr_pane_extents = self.fonts.bold_scaled.text_extents("0x0000000000000000.0");
            cfg.padding * 2.0 + addr_pane_extents.width
        };
        
        self.resize_window(&cfg);
        self.last_config_version = cfg.version;
        self.line_cache = HashMap::new();

        self.da.queue_draw();
        self.collect_events();
    }
    
    fn resize_window(&mut self, cfg: &config::Config) {
        let lines = f64::max((self.layout.height - 2.0 * cfg.padding) / self.fonts.extents.height, 0.0) as usize;
        let window_size =
            lines
            + 1 // round-up (TODO: what??)
            + 1 /* to accomodate scrolling */
            + (2 * cfg.lookahead); /* lookahead works in both directions */
        
        self.window.resize_window(window_size);
    }

    fn goto(&mut self, addr: addr::Address) -> Result<(), cursor::PlacementFailure> {
        self.cursor_view.goto(addr)?;
        self.scroll.ensure_cursor_is_in_view(&mut self.window, &self.cursor_view, component::scroll::EnsureCursorInViewDirection::Any);
        Ok(())
    }
    
    fn button_event(mut self: parking_lot::RwLockWriteGuard<Self>, eb: &gdk::EventButton) -> gtk::Inhibit {
        match eb.event_type() {
            gdk::EventType::ButtonPress => {
                self.drag_start_address = eb.coords().and_then(|(x, y)| self.estimate_address_at(x, y, AddressEstimationBias::Left));
                match self.drag_start_address {
                    Some(a) => { let _ = self.goto(a); },
                    _ => ()
                };
                
                let da = (*self.da).clone();
                std::mem::drop(self); /* grab_focus triggers focus_change_event before we return */
                da.grab_focus();
                return gtk::Inhibit(false);
            },
            gdk::EventType::ButtonRelease => {
                self.drag_start_address = None;
            }
            _ => ()
        }
        
        self.collect_events();

        gtk::Inhibit(false)
    }

    fn focus_change_event(&mut self, ef: &gdk::EventFocus) -> gtk::Inhibit {
        self.cursor_view.blink();
        self.cursor_view.has_focus = ef.is_in();
        
        self.collect_events();

        gtk::Inhibit(false)
    }
    
    fn size_allocate(&mut self, al: &gtk::Rectangle) {
        self.layout.width = al.width() as f64;
        self.layout.height = al.height() as f64;

        self.line_cache.clear();
        
        self.resize_window(&config::get());

        self.collect_events();
    }

    fn cursor_transaction<F>(&mut self, cb: F, dir: component::scroll::EnsureCursorInViewDirection) -> gtk::Inhibit
    where F: FnOnce(&mut component::cursor::CursorView) {
        cb(&mut self.cursor_view);
        self.scroll.ensure_cursor_is_in_view(&mut self.window, &self.cursor_view, dir);
        gtk::Inhibit(true)
    }

    fn cursor_transaction_fallible<F>(&mut self, cb: F, dir: component::scroll::EnsureCursorInViewDirection) -> gtk::Inhibit
    where F: FnOnce(&mut component::cursor::CursorView) -> bool {
        if cb(&mut self.cursor_view) {
            self.scroll.ensure_cursor_is_in_view(&mut self.window, &self.cursor_view, dir);
            gtk::Inhibit(true)
        } else {
            gtk::Inhibit(false)
        }
    }
    
    fn key_press_event(&mut self, ek: &gdk::EventKey) -> gtk::Inhibit {
        let document_host = self.document_host.clone();
        
        let r = match (ek.keyval(), ek.state().intersects(gdk::ModifierType::SHIFT_MASK), ek.state().intersects(gdk::ModifierType::CONTROL_MASK)) {
            /* basic cursor   key    shift  ctrl  */
            (gdk::keys::constants::Left,  false, false) => self.cursor_transaction(|c| c.move_left(),  component::scroll::EnsureCursorInViewDirection::Up),
            (gdk::keys::constants::Right, false, false) => self.cursor_transaction(|c| c.move_right(), component::scroll::EnsureCursorInViewDirection::Down),
            (gdk::keys::constants::Up,    false, false) => self.cursor_transaction(|c| c.move_up(),    component::scroll::EnsureCursorInViewDirection::Up),
            (gdk::keys::constants::Down,  false, false) => self.cursor_transaction(|c| c.move_down(),  component::scroll::EnsureCursorInViewDirection::Down),

            /* fast cursor    key    shift  ctrl  */
            (gdk::keys::constants::Left,  false, true ) => self.cursor_transaction(|c| c.move_left_large(),    component::scroll::EnsureCursorInViewDirection::Up),
            (gdk::keys::constants::Right, false, true ) => self.cursor_transaction(|c| c.move_right_large(),   component::scroll::EnsureCursorInViewDirection::Down),
            (gdk::keys::constants::Up,    false, true ) => self.cursor_transaction(|c| c.move_up_to_break(),   component::scroll::EnsureCursorInViewDirection::Up),
            (gdk::keys::constants::Down,  false, true ) => self.cursor_transaction(|c| c.move_down_to_break(), component::scroll::EnsureCursorInViewDirection::Down),

            /* basic scroll   key         shift  ctrl  */
            (gdk::keys::constants::Page_Up,   false, false) => { self.scroll.page_up(&self.window); gtk::Inhibit(true) },
            (gdk::keys::constants::Page_Down, false, false) => { self.scroll.page_down(&self.window); gtk::Inhibit(true) },
            
            _ => self.cursor_transaction_fallible(|c| c.entry(&document_host, ek), component::scroll::EnsureCursorInViewDirection::Any),
        };

        self.collect_events();

        r
    }

    fn motion_notify_event(&mut self, em: &gdk::EventMotion) -> gtk::Inhibit {
        self.hover = em.position();
        self.da.queue_draw();
        self.collect_events();
        
        match self.drag_start_address {
            Some(_) => gtk::Inhibit(true),
            None => gtk::Inhibit(false),
        }
    }
    
    fn estimate_address_at(&self, x: f64, y: f64, bias: AddressEstimationBias) -> Option<addr::Address> {
        let cfg = config::get();

        let offset_y = y - cfg.padding + self.fonts.extents.ascent + self.fonts.extents.height * self.scroll.get_position();
        let line_no = ((offset_y) / self.fonts.extents.height - 1.0) as isize;

        self.window.get_line(line_no).or(if bias == AddressEstimationBias::Strict { None } else { self.window.line_groups.back() }).and_then(|lg| {
            match lg {
                listing::line_group::LineGroup::Hex(hlg) => {
                    let mut chr = (x - self.layout.addr_pane_width - cfg.padding) / self.fonts.extents.max_x_advance;

                    let hexdump_bytes = hlg.hbrk.line_size.round_up().bytes as usize;
                    let hexdump_width = hexdump_bytes * 3 + (hexdump_bytes - 1) / 8;
                    let gutter_width = 2;
                    
                    if chr < 0.0 {
                        if bias == AddressEstimationBias::Strict {
                            return None
                        } else {
                            chr = 0.0;
                        }
                    }

                    let offset = if chr < hexdump_width as f64 {
                        /* hex */
                        chr-= (chr as usize / 25) as f64; /* spacing */
                        
                        match bias {
                            AddressEstimationBias::Strict if chr % 3.0 >= 2.0 => return None,
                            AddressEstimationBias::Strict => (chr / 3.0) as u64,
                            AddressEstimationBias::Left => (chr / 3.0) as u64,
                            AddressEstimationBias::Right => (chr / 3.0) as u64 + 1,
                            AddressEstimationBias::AwayFrom(a) => {
                                if a < hlg.extent.begin {
                                    (chr / 3.0) as u64 + 1
                                } else if a >= hlg.extent.end {
                                    (chr / 3.0) as u64
                                } else {
                                    let abyte = (a - hlg.extent.begin).bytes;
                                    let achr = (abyte * 3 + abyte / 8) as f64; /* divide by 8 for spacing */
                                    
                                    if chr < achr {
                                        (chr / 3.0) as u64
                                    } else {
                                        (chr / 3.0) as u64 + 1
                                    }
                                }
                            }
                        }
                    } else {
                        /* ascii */
                        chr-= (hexdump_width + gutter_width) as f64;
                        let spacers_left = (chr as isize / 9) as f64;
                        let spacers_right = ((chr - 1.0) as isize / 9) as f64;
                        
                        match bias {
                            AddressEstimationBias::Strict if chr < 0.0 => return None,
                            AddressEstimationBias::Strict if chr % 9.0 > 8.0 => return None, /* if we fall in a spacer */
                            AddressEstimationBias::Strict => (chr - spacers_left) as u64,
                            AddressEstimationBias::Left => (chr - spacers_left) as u64,
                            AddressEstimationBias::Right => (chr + 1.0 - spacers_right) as u64,
                            AddressEstimationBias::AwayFrom(a) => {
                                if a < hlg.extent.begin { /* bias right */
                                    (chr + 1.0 - spacers_right) as u64
                                } else if a >= hlg.extent.end { /* bias left */
                                    (chr - spacers_left) as u64
                                } else {
                                    let abyte = (a - hlg.extent.begin).bytes;
                                    let achr = (abyte + abyte / 8) as f64; /* divide by 8 for spacing */

                                    if chr < achr { /* bias left */
                                        (chr - spacers_left) as u64
                                    } else { /* bias right */
                                        (chr + 1.0 - spacers_right) as u64
                                    }
                                }
                            }
                        }
                    };

                    /* convert offset to address, clamping to bounds */
                    match bias {
                        AddressEstimationBias::Strict if addr::Size::from(offset) >= hlg.extent.length() => None,
                        _ => Some(hlg.extent.begin + std::cmp::min(addr::Size::from(offset), hlg.extent.length()))
                    }
                },
                listing::line_group::LineGroup::BreakHeader(bhlg) => {
                    match bias {
                        AddressEstimationBias::Strict => None,
                        _ => Some(bhlg.brk.addr),
                    }
                },
            }
        })
    }
    
    fn update(&mut self, cx: &mut task::Context) {
        let mut updated = false;
        let document = self.document_host.get_document();
        self.document_host.wait(cx);
        self.update_notifier.enroll(cx);

        /* We don't care about updates from the datapath. If the datapath
         * fetched any data that we actually cared about, we will find out when
         * we update the line groups. */
        document.datapath.poll(cx);
        
        updated = self.window.update(&document, cx) || updated;
        updated = self.cursor_view.cursor.update(&document, cx) || updated;

        // TODO: trigger animation with glib::idle_add
        self.has_updated = self.has_updated || updated;
    }
}

struct ListingWidgetFuture {
    lw: sync::Weak<parking_lot::RwLock<ListingWidget>>,
}

impl ListingWidgetFuture {
    fn new(lw: &sync::Arc<parking_lot::RwLock<ListingWidget>>) -> ListingWidgetFuture {
        ListingWidgetFuture {
            lw: sync::Arc::downgrade(lw)
        }
    }
}

impl std::future::Future for ListingWidgetFuture {
    type Output = ();

    fn poll(self: std::pin::Pin<&mut Self>, cx: &mut task::Context) -> task::Poll<()> {
        match self.lw.upgrade() {
            Some(lw) => {
                let mut lw = lw.write();
                lw.update(cx);
                task::Poll::Pending
            }
            None => task::Poll::Ready(())
        }
    }
}

trait DrawableLineGroup {
    fn draw<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>, cr: &cairo::Context, cursor_view: Option<&component::cursor::CursorView>);
    fn draw_selection<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>, cr: &cairo::Context, selection: &addr::Extent);
    fn draw_hover<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>, cr: &cairo::Context, hover: addr::Address);
}

impl DrawableLineGroup for listing::line_group::LineGroup {
    fn draw<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>, cr: &cairo::Context, cursor_view: Option<&component::cursor::CursorView>) {
        match self {
            line_group::LineGroup::Hex(hlg) => {
                hlg.draw(c, cr, cursor_view);
            },
            line_group::LineGroup::BreakHeader(bhlg) => {
                bhlg.draw(c, cr, cursor_view);
            },
        }
    }

    fn draw_selection<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>, cr: &cairo::Context, selection: &addr::Extent) {
        match self {
            line_group::LineGroup::Hex(hlg) => {
                hlg.draw_selection(c, cr, selection);
            },
            line_group::LineGroup::BreakHeader(bhlg) => {
                bhlg.draw_selection(c, cr, selection);
            },
        }
    }

    fn draw_hover<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>, cr: &cairo::Context, hover: addr::Address) {
        match self {
            line_group::LineGroup::Hex(hlg) => {
                hlg.draw_hover(c, cr, hover);
            },
            line_group::LineGroup::BreakHeader(bhlg) => {
                bhlg.draw_hover(c, cr, hover);
            },
        }
    }
}

impl DrawableLineGroup for brk::hex::HexLineGroup {
    fn draw<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>, cr: &cairo::Context, cursor_view: Option<&component::cursor::CursorView>) {
        /* draw ridge */
        if ((self.extent.begin - self.get_break().addr) / self.hbrk.line_size) % 16 == 0 {
            cr.set_source_gdk_rgba(c.cfg.ridge_color);
            cr.move_to(c.layout.addr_pane_width, c.font_extents().descent);
            cr.line_to(c.layout.width, c.font_extents().descent);
            cr.stroke().unwrap();
        }
        
        /* draw address in addr pane */
        cr.set_scaled_font(&c.fonts.bold_scaled);
        cr.set_source_gdk_rgba(c.cfg.addr_color);
        cr.move_to(c.pad, c.font_extents().height);
        cr.show_text(&format!("{}", self.extent.begin)).unwrap();

        self.get_patched_bytes(&mut c.byte_cache);

        let cursor = cursor_view.and_then(|cv| match &cv.cursor.class {
            cursor::CursorClass::Hex(hc) => Some((hc, cv)),
            _ => None
        });
        
        /* hexdump */
        cr.move_to(c.layout.addr_pane_width + c.pad, c.font_extents().height);
        cr.set_scaled_font(&c.fonts.mono_scaled);
        cr.set_source_gdk_rgba(c.cfg.text_color);

        let fe = &c.fonts.extents;
        let mut utf8_buffer: [u8; 4] = [0; 4];
        
        for i in 0..(self.hbrk.line_size.round_up().bytes as usize) {
            if i > 0 && i % 8 == 0 {
                cr.show_text(" ").unwrap();
            }
            
            for low_nybble in &[false, true] {
                let pbr = if i < c.byte_cache.len() {
                    c.byte_cache[i]
                } else {
                    datapath::ByteRecord::default()
                };
                
                let chr = if pbr.has_any_value() {
                    util::nybble_to_hex((pbr.value >> if *low_nybble { 0 } else { 4 }) & 0xf)
                } else {
                    ' '
                };

                let byte_color = if pbr.has_direct_edit() {
                    c.cfg.patch_color
                } else {
                    c.cfg.text_color
                };

                cr.set_source_gdk_rgba(byte_color);
                
                if let Some((hc, cv)) = cursor {
                    if hc.offset.bytes == i as u64 && hc.low_nybble == *low_nybble {
                        let (x, y) = cr.current_point().expect("there should be a current point I think");

                        if cv.insert {
                            if cv.get_blink() {
                                cr.set_source_gdk_rgba(c.cfg.addr_color);
                                cr.rectangle(x.round() + cv.get_bonk(), fe.height - fe.ascent, 2.0, fe.height);
                                cr.fill().unwrap();

                                cr.set_source_gdk_rgba(byte_color);
                            }
                        } else if cv.has_focus {
                            if cv.get_blink() {
                                cr.set_source_gdk_rgba(c.cfg.addr_color);
                                cr.rectangle(x.round() + cv.get_bonk(), fe.height - fe.ascent, fe.max_x_advance, fe.height);
                                cr.fill().unwrap();

                                cr.set_source_gdk_rgba(c.cfg.background_color);
                            }
                        } else {
                            cr.set_source_gdk_rgba(c.cfg.addr_color);
                            cr.set_line_width(1.0);
                            cr.rectangle(x.round() + 0.5 + cv.get_bonk(), fe.height - fe.ascent + 0.5, fe.max_x_advance - 1.0, fe.height - 1.0);
                            cr.stroke().unwrap();
                            
                            cr.set_source_gdk_rgba(byte_color);
                        }

                        cr.move_to(x, y);
                    }
                }
                
                cr.show_text(chr.encode_utf8(&mut utf8_buffer)).unwrap();
            }
            cr.show_text(" ").unwrap();
        }

        cr.set_source_gdk_rgba(c.cfg.text_color);
        cr.set_scaled_font(&c.fonts.mono_scaled);
        cr.show_text("| ").unwrap();

        /* asciidump */
        for i in 0..(self.hbrk.line_size.round_up().bytes as usize) {
            if i > 0 && i % 8 == 0 {
                cr.show_text(" ").unwrap();
            }
            
            if i < c.byte_cache.len() {
                let b = c.byte_cache[i].value;
                cr.set_source_gdk_rgba(c.cfg.text_color);
                if (b as char).is_ascii_graphic() {
                    cr.show_text((b as char).encode_utf8(&mut utf8_buffer)).unwrap();
                } else {
                    cr.show_text(".").unwrap();
                }
            } else {
                cr.show_text(" ").unwrap();
            }
        }

        cr.set_source_gdk_rgba(c.cfg.text_color);
        cr.show_text(" | ").unwrap();
        cr.show_text(self.describe_state()).unwrap();
    }

    fn draw_selection<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>, cr: &cairo::Context, selection: &addr::Extent) {
        if let Some(intersect) = self.extent.intersection(*selection) {
            let byte_begin = (intersect.begin - self.extent.begin).bytes as usize;
            let byte_end = (intersect.end - self.extent.begin).bytes as usize;

            { /* hex */
                 let mut chr_begin = byte_begin * 3;
                 chr_begin+= byte_begin / 8; /* spacing */

                 let mut chr_end = byte_end * 3 - 1; // TODO: debug an intermittent failure here
                 chr_end+= (byte_end - 1) / 8; /* spacing */

                 let fe = c.font_extents();
                 
                 cr.set_source_gdk_rgba(c.cfg.addr_color);
                 cr.rectangle(c.layout.addr_pane_width + c.pad + chr_begin as f64 * fe.max_x_advance, fe.height - fe.ascent, (chr_end - chr_begin) as f64 * fe.max_x_advance, fe.height);
                 cr.fill().unwrap();
            }
            
            { /* ascii */
                 let hexdump_bytes = self.hbrk.line_size.round_up().bytes as usize;
                 let hexdump_width = hexdump_bytes * 3 + (hexdump_bytes - 1) / 8;
                 let gutter_width = 2;
                 
                 let chr_begin = hexdump_width + gutter_width + byte_begin + byte_begin / 8;
                 let chr_end = hexdump_width + gutter_width + byte_end + (byte_end - 1) / 8;

                 let fe = c.font_extents();
                 
                 cr.set_source_gdk_rgba(c.cfg.addr_color);
                 cr.rectangle(c.layout.addr_pane_width + c.pad + chr_begin as f64 * fe.max_x_advance, fe.height - fe.ascent, (chr_end - chr_begin) as f64 * fe.max_x_advance, fe.height);
                 cr.fill().unwrap();
            }
        }
    }

    fn draw_hover<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>, cr: &cairo::Context, hover: addr::Address) {
        if self.extent.includes(hover) {
            let byte = (hover - self.extent.begin).bytes as usize;

            { /* hex */
                 let mut chr = byte * 3;
                 chr+= byte / 8; /* spacing */

                 let fe = c.font_extents();
                 
                 cr.set_source_rgba(1.0, 1.0, 1.0, 0.2);
                 cr.rectangle(c.layout.addr_pane_width + c.pad + chr as f64 * fe.max_x_advance, fe.height - fe.ascent, 2.0 * fe.max_x_advance, fe.height);
                 cr.fill().unwrap();
            }

            { /* ascii */
                 let hexdump_bytes = self.hbrk.line_size.round_up().bytes as usize;
                 let hexdump_width = hexdump_bytes * 3 + (hexdump_bytes - 1) / 8;
                 let gutter_width = 2;
                 
                 let chr = hexdump_width + gutter_width + byte + byte / 8;

                 let fe = c.font_extents();
                
                 cr.set_source_rgba(1.0, 1.0, 1.0, 0.2);
                 cr.rectangle(c.layout.addr_pane_width + c.pad + chr as f64 * fe.max_x_advance, fe.height - fe.ascent, fe.max_x_advance, fe.height);
                 cr.fill().unwrap();
            }
        }
    }
}

impl DrawableLineGroup for brk::BreakHeaderLineGroup {
    fn draw<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>, cr: &cairo::Context, cursor: Option<&component::cursor::CursorView>) {
        /* draw label */

        // TODO: rearchitect line rendering so we can cache things like this
        let label = match &self.brk.label {
            Some(l) => l.clone() + ":",
            None => "unlabeled:".to_string()
        };
        
        let fe = c.font_extents();

        cr.set_source_gdk_rgba(c.cfg.text_color);
        if let Some(cv) = cursor {
            let te = c.fonts.bold_scaled.text_extents(label.as_str());
            
            if cv.has_focus {
                if cv.get_blink() {
                    cr.set_source_gdk_rgba(c.cfg.addr_color);
                    cr.rectangle(c.layout.addr_pane_width + c.pad + cv.get_bonk(), fe.height * 2.0 - fe.ascent, te.x_advance, fe.height);
                    cr.fill().unwrap();
                    
                    cr.set_source_gdk_rgba(c.cfg.background_color);
                }
            } else {
                cr.set_source_gdk_rgba(c.cfg.addr_color);
                cr.set_line_width(1.0);
                cr.rectangle(c.layout.addr_pane_width + c.pad + cv.get_bonk() + 0.5, fe.height * 2.0 - fe.ascent + 0.5, te.x_advance - 1.0, fe.height - 1.0);
                
                cr.stroke().unwrap();
                
                cr.set_source_gdk_rgba(c.cfg.text_color);
            }
        }
        
        cr.set_scaled_font(&c.fonts.bold_scaled);
        cr.move_to(c.layout.addr_pane_width + c.pad, fe.height * 2.0);
        cr.show_text(label.as_str()).unwrap();
    }

    fn draw_selection<'a, 'b, 'c>(&'a self, _c: &'b mut InternalRenderingContext<'c>, _cr: &cairo::Context, _selection: &addr::Extent) {
        // TODO
    }

    fn draw_hover<'a, 'b, 'c>(&'a self, _c: &'b mut InternalRenderingContext<'c>, _cr: &cairo::Context, _hover: addr::Address) {
        // TODO
    }
}
