use std::sync;
use std::task;
use std::option;
use std::vec;
use std::time;
use std::collections::HashMap;

use crate::addr;
use crate::config;
use crate::listing;
use crate::listing::brk;
use crate::listing::line_group;

use crate::ext::CairoExt;

use gtk::prelude::*;
use gio::prelude::*;

mod action;
mod component;

const MICROSECONDS_PER_SECOND: f64 = 1000000.0;
const MICROSECONDS_PER_SECOND_INT: i64 = 1000000;

struct InternalRenderingContext<'a> {
    cr: &'a cairo::Context,
    cfg: &'a config::Config,
    fonts: &'a Fonts,
    layout: &'a Layout,

    perf: time::Instant,
    
    byte_cache: vec::Vec<u8>,
    
    pad: f64,
}

impl<'a> InternalRenderingContext<'a> {
    fn font_extents(&self) -> &'a cairo::FontExtents {
        &self.fonts.extents
    }
}

struct Fonts {
    mono_face: cairo::FontFace,
    bold_face: cairo::FontFace,
    mono_scaled: cairo::ScaledFont,
    bold_scaled: cairo::ScaledFont,

    extents: cairo::FontExtents,
}

impl Fonts {
    fn new(cfg: &config::Config) -> Fonts {
        let mono_face = cairo::FontFace::toy_create("monospace", cairo::FontSlant::Normal, cairo::FontWeight::Normal);
        let bold_face = cairo::FontFace::toy_create("monospace", cairo::FontSlant::Normal, cairo::FontWeight::Bold);
        
        let mut font_matrix = cairo::Matrix::identity();
        font_matrix.scale(cfg.font_size, cfg.font_size);
        let ctm = cairo::Matrix::identity();
        let options = cairo::FontOptions::new();

        let mono_scaled = cairo::ScaledFont::new(&mono_face, &font_matrix, &ctm, &options);
        let bold_scaled = cairo::ScaledFont::new(&bold_face, &font_matrix, &ctm, &options);
        
        Fonts {
            extents: mono_scaled.extents(),
            
            mono_face,
            bold_face,
            mono_scaled,
            bold_scaled,
        }
    }
}

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

pub struct ListingWidget {
    window: listing::window::ListingWindow,
    rt: tokio::runtime::Handle,
    update_task: option::Option<tokio::task::JoinHandle<()>>,

    last_config_version: usize,
    last_animation_time: i64,
    
    has_focus: bool,

    fonts: send_wrapper::SendWrapper<Fonts>,
    layout: Layout,
    //cursor: component::cursor::Cursor,
    scroll: component::scroll::Scroller,

    line_cache: HashMap<listing::line_group::CacheId, send_wrapper::SendWrapper<LineCacheEntry>>,
    last_frame_duration: Option<time::Duration>,
}

impl ListingWidget {
    pub fn new(
        listing: sync::Arc<listing::Listing>,
        rt: tokio::runtime::Handle) -> ListingWidget {
        let cfg = config::get();
        ListingWidget {
            window: listing::window::ListingWindow::new(listing.clone()),
            rt,
            update_task: None,

            last_config_version: 0,
            last_animation_time: 0,

            has_focus: false,

            fonts: send_wrapper::SendWrapper::new(Fonts::new(&cfg)),
            layout: Layout {
                width: 0.0,
                height: 0.0,
                addr_pane_width: 0.0,
            },
            //cursor: component::cursor::Cursor::new(),
            scroll: component::scroll::Scroller::new(),

            line_cache: HashMap::new(),
            last_frame_duration: None,
        }
    }
    
    pub fn attach(self, da: &gtk::DrawingArea) {
        let rc: sync::Arc<parking_lot::RwLock<Self>> = sync::Arc::new(parking_lot::RwLock::new(self));

        let mut lw = rc.write();

        lw.update_task = Some(
            lw.rt.spawn(ListingWidgetFuture::new(&rc)));

        lw.has_focus = da.is_focus();
        
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
        { let rc_clone = rc.clone(); da.connect_button_press_event(move |da, eb| rc_clone.write().button_event(da, eb)); }
        { let rc_clone = rc.clone(); da.connect_button_release_event(move |da, eb| rc_clone.write().button_event(da, eb)); }
        { let rc_clone = rc.clone(); da.connect_draw(move |da, cr| rc_clone.write().draw(da, cr)); }
        { let rc_clone = rc.clone(); da.connect_focus_in_event(move |da, ef| rc_clone.write().focus_change_event(da, ef)); }
        { let rc_clone = rc.clone(); da.connect_focus_out_event(move |da, ef| rc_clone.write().focus_change_event(da, ef)); }
        { let rc_clone = rc.clone(); da.connect_key_press_event(move |da, ek| rc_clone.write().key_press_event(da, ek)); }
        { let rc_clone = rc.clone(); da.connect_scroll_event(move |da, es| rc_clone.write().scroll_event(da, es)); }
        { let rc_clone = rc.clone(); da.connect_size_allocate(move |da, al| rc_clone.write().size_allocate(da, al)); }
        { let rc_clone = rc.clone(); da.add_tick_callback(move |da, fc| rc_clone.write().tick_callback(da, fc)); }

        da.add_events(
            gdk::EventMask::FOCUS_CHANGE_MASK |
            gdk::EventMask::SCROLL_MASK |
            gdk::EventMask::SMOOTH_SCROLL_MASK |
            gdk::EventMask::KEY_PRESS_MASK |
            gdk::EventMask::KEY_RELEASE_MASK |
            gdk::EventMask::BUTTON_PRESS_MASK |
            gdk::EventMask::BUTTON_RELEASE_MASK
        );

        /* misc flags */
        da.set_can_focus(true);
        da.set_focus_on_click(true);

        /* actions */
        let ag = gio::SimpleActionGroup::new();
        //ag.add_action(&action::goto::create(&rc, da));
        ag.add_action(&action::insert_break::create(lw.window.listing.clone(), da));

        //ag.add_action(&action::movement::create_goto_start_of_line(&rc, da));
        //ag.add_action(&action::movement::create_goto_end_of_line(&rc, da));
        
        da.insert_action_group("listing", Some(&ag));
        
        lw.reconfigure(da, &config::get());
    }

    pub fn draw<'a>(&'a mut self, da: &gtk::DrawingArea, cr: &cairo::Context) -> gtk::Inhibit {
        let frame_begin = time::Instant::now();
        
        let cfg = config::get();
                
        let mut irc = InternalRenderingContext {
            cr,
            cfg: &cfg,
            fonts: &self.fonts,
            layout: &self.layout,

            perf: frame_begin,
            byte_cache: vec::Vec::new(),

            pad: cfg.padding, // it is helpful to have a shorthand for this one
        };

        /* fill background */
        cr.set_source_gdk_rgba(irc.cfg.background_color);
        cr.paint();

        /* fill address pane */
        cr.set_source_gdk_rgba(cfg.addr_pane_color);
        cr.rectangle(0.0, 0.0, self.layout.addr_pane_width, self.layout.height);
        cr.fill();

        /* render lines */
        cr.save();
        cr.translate(0.0, irc.pad + irc.font_extents().height * -self.scroll.get_position());
            
        for (lineno, lg) in self.window.iter() {
            cr.save();
            cr.translate(0.0, irc.font_extents().height * (lineno as f64));

            let lc = &mut self.line_cache;
            match lg.get_cache_id().and_then(|cache_id| {
                match lc.entry(cache_id) {
                    std::collections::hash_map::Entry::Occupied(occu) => Some(occu.into_mut()),
                    std::collections::hash_map::Entry::Vacant(vac) => {
                        
                        /* This line is not in our cache, so try to render
                         * it. If we fail, no big deal- just draw the line
                         * straight to the screen. */
                        
                        da.get_parent_window().and_then(|window| {
                            // super important for performance! avoid uploading to GPU every frame!
                            window.create_similar_surface(cairo::Content::ColorAlpha, irc.layout.width as i32, (irc.fonts.extents.height + irc.fonts.extents.descent) as i32)
                        }).and_then(|surface| {
                            let cache_cr = cairo::Context::new(&surface);
                            
                            lg.draw(&mut irc, &cache_cr);

                            surface.flush();
                            
                            Some(vac.insert(send_wrapper::SendWrapper::new(LineCacheEntry {
                                pattern: cairo::SurfacePattern::create(&surface),
                                surface,
                                touched: true
                            })))
                        })
                    }
                }
            }) {
                // We either found it in the cache, or just rendered it to the cache and are ready to use the cached image.
                Some(entry) => {
                    entry.touched = true;
                    cr.set_source(&entry.pattern);
                    cr.paint();
                    
                },
                // Line was not cacheable or errored trying to allocate surface to put it in cache. Render it directly.
                None => {
                    lg.draw(&mut irc, &cr);
                }
            }

            cr.restore();
        }
        cr.restore();

        // evict stale entries
        self.line_cache.retain(|_, lce| std::mem::replace(&mut lce.touched, false));

        /* fill address pane */
        cr.set_source_gdk_rgba(cfg.addr_pane_color);
        cr.rectangle(0.0, 0.0, self.layout.width, self.fonts.extents.height);
        cr.fill();
        
        /* DEBUG */
        let debug = vec![
            //            format!("{:#?}", self.cursor),
            format!("last frame duration: {}", self.last_frame_duration.map(|d| d.as_micros() as f64).unwrap_or(0.0) / 1000.0),
            format!("{:#?}", self.scroll),
        ];
        let mut lineno = 0;
        for group in debug {
            for line in group.split("\n") {
                cr.set_source_rgba(1.0, 1.0, 0.0, 1.0);
                cr.move_to(self.layout.width - 400.0, 40.0 + (irc.font_extents().height * lineno as f64));
                cr.show_text(&line);
                lineno+= 1;
            }
        }

        self.last_frame_duration = Some(time::Instant::now() - frame_begin);
        
        gtk::Inhibit(false)
    }
    
    fn collect_draw_events(&mut self, da: &gtk::DrawingArea) {
        //self.cursor.events.collect_draw(da);
        self.scroll.events.collect_draw(da);

        if self.window.clear_has_loaded() {
            da.queue_draw();
        }
    }
    
    fn scroll_event(&mut self, da: &gtk::DrawingArea, es: &gdk::EventScroll) -> gtk::Inhibit {
        self.scroll.scroll_wheel_impulse(es.get_delta().1);
        self.collect_draw_events(da);
        
        gtk::Inhibit(true)
    }
    
    fn tick_callback(&mut self, da: &gtk::DrawingArea, fc: &gdk::FrameClock) -> Continue {
        let cfg = config::get();

        if cfg.version > self.last_config_version {
            self.reconfigure(da, &cfg);
        }

        if self.window.update() {
            self.line_cache = HashMap::new();
            da.queue_draw();
        }
        
        if fc.get_frame_time() - self.last_animation_time > (MICROSECONDS_PER_SECOND as i64) {
            // if we fall too far behind, just drop frames
            self.last_animation_time = fc.get_frame_time();
        }

        while self.last_animation_time < fc.get_frame_time() {
            let ais_micros = std::cmp::min(fc.get_frame_time() - self.last_animation_time, MICROSECONDS_PER_SECOND_INT / 20); // don't go below 20 TPS or the integration error gets bad
            let ais:f64 = ais_micros as f64 / MICROSECONDS_PER_SECOND;

            self.scroll.animate(&mut self.window, ais);            
            //self.cursor.animate(&cfg, ais);

            self.last_animation_time+= ais_micros;
        }

        self.collect_draw_events(da);
        
        Continue(true)
    }

    fn reconfigure(&mut self, da: &gtk::DrawingArea, cfg: &config::Config) {
        self.fonts = send_wrapper::SendWrapper::new(Fonts::new(cfg));

        self.layout.width = da.get_allocated_width() as f64;
        self.layout.height = da.get_allocated_height() as f64;
        self.layout.addr_pane_width = {
            let addr_pane_extents = self.fonts.bold_scaled.text_extents("0x0000000000000000.0");
            cfg.padding * 2.0 + addr_pane_extents.width
        };
        
        self.resize_window(&cfg);
        self.last_config_version = cfg.version;
        self.line_cache = HashMap::new();
        
        da.queue_draw();
    }
    
    fn resize_window(&mut self, cfg: &config::Config) {
        let lines = f64::max((self.layout.height - 2.0 * cfg.padding) / self.fonts.extents.height, 0.0) as usize;
        let window_size =
            lines
            + 1 // round-up (TODO: what??)
            + 1 // to accomodate scrolling
            + (2 * cfg.lookahead); // lookahead works in both directions
        
        self.window.resize_window(window_size);
    }

    fn goto(&mut self, _addr: addr::Address) {
        todo!();
        //self.cursor.goto(&self.engine, addr);
        //self.scroll.ensure_cursor_is_in_view(self, component::cursor::EnsureInViewDirection::Any);
    }
    
    fn button_event(mut self: parking_lot::RwLockWriteGuard<Self>, da: &gtk::DrawingArea, eb: &gdk::EventButton) -> gtk::Inhibit {
        match eb.get_event_type() {
            gdk::EventType::ButtonPress => {
                std::mem::drop(self); // grab_focus triggers focus_change_event before we return
                da.grab_focus();
                return gtk::Inhibit(false);
            },
            _ => ()
        }
        
        self.collect_draw_events(da);

        gtk::Inhibit(false)
    }

    fn focus_change_event(&mut self, da: &gtk::DrawingArea, ef: &gdk::EventFocus) -> gtk::Inhibit {
        //self.cursor.blink();
        self.has_focus = ef.get_in();
        
        self.collect_draw_events(da);

        gtk::Inhibit(false)
    }
    
    fn size_allocate(&mut self, da: &gtk::DrawingArea, al: &gtk::Rectangle) {
        self.layout.width = al.width as f64;
        self.layout.height = al.height as f64;
        
        self.resize_window(&config::get());

        self.collect_draw_events(da);
    }

    /*
    fn cursor_transaction<F>(&mut self, cb: F, dir: component::cursor::EnsureInViewDirection) -> gtk::Inhibit
    where F: FnOnce(&mut component::cursor::Cursor, &listing::ListingEngine) {
        cb(&mut self.cursor, &self.engine);
        self.scroll.ensure_cursor_is_in_view(self, dir);
        gtk::Inhibit(true)
    }
*/
    
    fn key_press_event(&mut self, da: &gtk::DrawingArea, ek: &gdk::EventKey) -> gtk::Inhibit {
        let r = match (ek.get_keyval(), ek.get_state().intersects(gdk::ModifierType::SHIFT_MASK), ek.get_state().intersects(gdk::ModifierType::CONTROL_MASK)) {
            /*
            /* basic cursor   key    shift  ctrl  */
            (gdk::enums::key::Left,  false, false) => self.cursor_transaction(|c,l| c.move_left(l),  component::cursor::EnsureInViewDirection::Up),
            (gdk::enums::key::Right, false, false) => self.cursor_transaction(|c,l| c.move_right(l), component::cursor::EnsureInViewDirection::Down),
            (gdk::enums::key::Up,    false, false) => self.cursor_transaction(|c,l| c.move_up(l),    component::cursor::EnsureInViewDirection::Up),
            (gdk::enums::key::Down,  false, false) => self.cursor_transaction(|c,l| c.move_down(l),  component::cursor::EnsureInViewDirection::Down),

            /* fast cursor    key    shift  ctrl  */
            (gdk::enums::key::Left,  false, true ) => self.cursor_transaction(|c,l| c.move_left_by_qword(l),  component::cursor::EnsureInViewDirection::Up),
            (gdk::enums::key::Right, false, true ) => self.cursor_transaction(|c,l| c.move_right_by_qword(l), component::cursor::EnsureInViewDirection::Down),
            (gdk::enums::key::Up,    false, true ) => self.cursor_transaction(|c,l| c.move_up_to_break(l),    component::cursor::EnsureInViewDirection::Up),
            (gdk::enums::key::Down,  false, true ) => self.cursor_transaction(|c,l| c.move_down_to_break(l),  component::cursor::EnsureInViewDirection::Down),

            /* basic scroll   key         shift  ctrl  */
            (gdk::enums::key::Page_Up,   false, false) => { self.scroll.page_up(self); gtk::Inhibit(true) },
            (gdk::enums::key::Page_Down, false, false) => { self.scroll.page_down(self); gtk::Inhibit(true) },
             */
            
            _ => gtk::Inhibit(false)
        };

        self.collect_draw_events(da);

        r
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
            Some(lw) => std::pin::Pin::new(&mut lw.write().window).poll(cx),
            None => task::Poll::Ready(())
        }
    }
}

trait DrawableLineGroup {
    fn draw<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>, cr: &cairo::Context);
}

impl DrawableLineGroup for listing::line_group::LineGroup {
    fn draw<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>, cr: &cairo::Context) {
        match self {
            line_group::LineGroup::Hex(hlg) => {
                hlg.draw(c, cr);
            },
            line_group::LineGroup::BreakHeader(bhlg) => {
                bhlg.draw(c, cr);
            },
        }
    }
}

impl DrawableLineGroup for brk::hex::HexLineGroup {
    fn draw<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>, cr: &cairo::Context) {
        // draw ridge
        if ((self.extent.begin - self.get_break().addr) / self.hbrk.line_size) % 16 == 0 {
            cr.set_source_gdk_rgba(c.cfg.ridge_color);
            cr.move_to(c.layout.addr_pane_width, c.font_extents().descent);
            cr.line_to(c.layout.width, c.font_extents().descent);
            cr.stroke();
        }
        
        // draw address in addr pane
        cr.set_scaled_font(&c.fonts.bold_scaled);
        cr.set_source_gdk_rgba(c.cfg.addr_color);
        cr.move_to(c.pad, c.font_extents().height);
        cr.show_text(&format!("{}", self.extent.begin));

        self.get_bytes(&mut c.byte_cache);

        // hexdump
        cr.move_to(c.layout.addr_pane_width + c.pad, c.font_extents().height);
        cr.set_scaled_font(&c.fonts.mono_scaled);
        cr.set_source_gdk_rgba(c.cfg.text_color);
        for i in 0..(self.hbrk.line_size.round_up().bytes as usize) {
            if i == 8 {
                cr.show_text(" ");
            }

            if i < c.byte_cache.len() {
                cr.show_text(&format!("{:02x} ", c.byte_cache[i]));
            } else {
                cr.show_text("   ");
            }
        }

        cr.set_source_gdk_rgba(c.cfg.text_color);
        cr.set_scaled_font(&c.fonts.mono_scaled);
        cr.show_text("| ");

        // asciidump
        for i in 0..(self.hbrk.line_size.round_up().bytes as usize) {
            if i == 8 {
                cr.show_text(" ");
            }
            
            if i < c.byte_cache.len() {
                let b = c.byte_cache[i];
                cr.set_source_gdk_rgba(c.cfg.text_color);
                if (b as char).is_ascii_graphic() {
                    cr.show_text(unsafe { std::str::from_utf8_unchecked(std::slice::from_ref(&b))});
                } else {
                    cr.show_text(".");
                }
            } else {
                cr.show_text(" ");
            }
        }

        cr.set_source_gdk_rgba(c.cfg.text_color);
        cr.show_text(" | ");
        cr.show_text(self.describe_state());
    }
}

impl DrawableLineGroup for brk::BreakHeaderLineGroup {
    fn draw<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>, cr: &cairo::Context) {
        // draw label
        cr.set_scaled_font(&c.fonts.bold_scaled);
        cr.set_source_gdk_rgba(c.cfg.text_color);
        cr.move_to(c.layout.addr_pane_width + c.pad, c.font_extents().height * 2.0);
        cr.show_text(match &self.brk.label {
            Some(l) => l.as_str(),
            None => "unlabeled"
        });
        cr.show_text(":");
    }
}
