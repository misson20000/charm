use std::sync;
use std::task;
use std::option;
use std::vec;

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

#[derive(Debug, Copy, Clone)]
enum InternalRenderingPhase {
    Extents,
    Background,
    Foreground
}

// TODO: can I make this non-pub
pub struct InternalRenderingContext<'a> {
    lw: &'a ListingWidget,
    cr: &'a cairo::Context,
    phase: InternalRenderingPhase,
    cfg: &'a config::Config,
    fonts: &'a Fonts,
    
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

pub struct ListingWidget {
    view: listing::view::ListingView,
    rt: tokio::runtime::Handle,
    update_task: option::Option<tokio::task::JoinHandle<()>>,

    last_config_version: usize,
    last_animation_time: i64,
    
    has_focus: bool,

    fonts: send_wrapper::SendWrapper<Fonts>,
    layout: Layout,
    //cursor: component::cursor::Cursor,
    scroll: component::scroll::Scroller
}

impl ListingWidget {
    pub fn new(
        listing: sync::Arc<listing::Listing>,
        rt: tokio::runtime::Handle) -> ListingWidget {
        let cfg = config::get();
        ListingWidget {
            view: listing::view::ListingView::new(listing.clone()),
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
        ag.add_action(&action::insert_break::create(lw.view.listing.clone(), da));

        //ag.add_action(&action::movement::create_goto_start_of_line(&rc, da));
        //ag.add_action(&action::movement::create_goto_end_of_line(&rc, da));
        
        da.insert_action_group("listing", Some(&ag));
        
        lw.reconfigure(da, &config::get());
    }

    pub fn draw<'a>(&'a mut self, _da: &gtk::DrawingArea, cr: &cairo::Context) -> gtk::Inhibit {
        let cfg = config::get();
                
        let mut irc = InternalRenderingContext {
            lw: &self,
            cr,
            phase: InternalRenderingPhase::Extents,
            cfg: &cfg,
            fonts: &self.fonts,

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

        /* render lines in each pass */
        cr.save();
        cr.translate(0.0, irc.pad + irc.font_extents().height * -self.scroll.get_position());
        for phase in &[
            InternalRenderingPhase::Extents,
            InternalRenderingPhase::Background,
            InternalRenderingPhase::Foreground] {
            irc.phase = *phase;
            
            for (lineno, lg) in self.view.iter() {
                cr.save();
                cr.translate(0.0, irc.font_extents().height * (lineno as f64));

                match &lg {
                    line_group::LineGroup::Hex(hlg) => {
                        hlg.draw(&mut irc);
                    },
                    line_group::LineGroup::BreakHeader(bhlg) => {
                        bhlg.draw(&mut irc);
                    },
                }

                cr.restore();
            }
        }
        cr.restore();

        /* fill address pane */
        cr.set_source_gdk_rgba(cfg.addr_pane_color);
        cr.rectangle(0.0, 0.0, self.layout.width, self.fonts.extents.height);
        cr.fill();
        
        /* DEBUG */
        let debug = vec![
//            format!("{:#?}", self.cursor),
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
        
        gtk::Inhibit(false)
    }
    
    fn collect_draw_events(&mut self, da: &gtk::DrawingArea) {
        //self.cursor.events.collect_draw(da);
        self.scroll.events.collect_draw(da);

        if self.view.clear_has_loaded() {
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

        if self.view.update() {
            da.queue_draw();
        }
        
        if fc.get_frame_time() - self.last_animation_time > (MICROSECONDS_PER_SECOND as i64) {
            // if we fall too far behind, just drop frames
            self.last_animation_time = fc.get_frame_time();
        }

        while self.last_animation_time < fc.get_frame_time() {
            let ais_micros = std::cmp::min(fc.get_frame_time() - self.last_animation_time, MICROSECONDS_PER_SECOND_INT / 20); // don't go below 20 TPS or the integration error gets bad
            let ais:f64 = ais_micros as f64 / MICROSECONDS_PER_SECOND;

            self.scroll.animate(&mut self.view, ais);            
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
        
        da.queue_draw();
    }
    
    fn resize_window(&mut self, cfg: &config::Config) {
        let lines = f64::max((self.layout.height - 2.0 * cfg.padding) / self.fonts.extents.height, 0.0) as usize;
        let window_size =
            lines
            + 1 // round-up (TODO: what??)
            + 1 // to accomodate scrolling
            + (2 * cfg.lookahead); // lookahead works in both directions
        
        self.view.resize_window(window_size);
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
            Some(lw) => std::pin::Pin::new(&mut lw.write().view).poll(cx),
            None => task::Poll::Ready(())
        }
    }
}

trait DrawableLineGroup {
    fn draw<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>);
}

impl DrawableLineGroup for brk::hex::HexLineGroup {
    fn draw<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>) {
        match c.phase {
            InternalRenderingPhase::Extents => {
            },
            InternalRenderingPhase::Background => {
                // draw ridge
                if ((self.extent.begin - self.get_break().addr) / self.hbrk.line_size) % 16 == 0 {
                    c.cr.set_source_gdk_rgba(c.cfg.ridge_color);
                    c.cr.move_to(c.lw.layout.addr_pane_width, c.font_extents().descent);
                    c.cr.line_to(c.lw.layout.width, c.font_extents().descent);
                    c.cr.stroke();
                }
            },
            InternalRenderingPhase::Foreground => {
                // draw address in addr pane
                c.cr.set_scaled_font(&c.fonts.bold_scaled);
                c.cr.set_source_gdk_rgba(c.cfg.addr_color);
                c.cr.move_to(c.pad, c.font_extents().height);
                c.cr.show_text(&format!("{}", self.extent.begin));

                self.get_bytes(&mut c.byte_cache);

                // hexdump
                c.cr.move_to(c.lw.layout.addr_pane_width + c.pad, c.font_extents().height);
                c.cr.set_scaled_font(&c.fonts.mono_scaled);
                for i in 0..(self.hbrk.line_size.round_up().bytes as usize) {
                    if i == 8 {
                        c.cr.show_text(" ");
                    }

                    if i < c.byte_cache.len() {
                        c.cr.set_source_gdk_rgba(c.cfg.text_color);
                        c.cr.show_text(&format!("{:02x} ", c.byte_cache[i]));
                    } else {
                        c.cr.show_text("   ");
                    }
                }

                c.cr.set_scaled_font(&c.fonts.mono_scaled);
                c.cr.show_text("| ");

                // asciidump
                for i in 0..(self.hbrk.line_size.round_up().bytes as usize) {
                    if i == 8 {
                        c.cr.show_text(" ");
                    }
                    
                    if i < c.byte_cache.len() {
                        let b = c.byte_cache[i];
                        c.cr.set_source_gdk_rgba(c.cfg.text_color);
                        if (b as char).is_ascii_graphic() {
                            c.cr.show_text(unsafe { std::str::from_utf8_unchecked(std::slice::from_ref(&b))});
                        } else {
                            c.cr.show_text(".");
                        }
                    } else {
                        c.cr.show_text(" ");
                    }
                }
                
                c.cr.show_text(" | ");
                c.cr.show_text(self.describe_state());
            }
        }
    }
}

impl DrawableLineGroup for brk::BreakHeaderLineGroup {
    fn draw<'a, 'b, 'c>(&'a self, c: &'b mut InternalRenderingContext<'c>) {
        match c.phase {
            InternalRenderingPhase::Foreground => {
                // draw label
                c.cr.set_scaled_font(&c.fonts.bold_scaled);
                c.cr.set_source_gdk_rgba(c.cfg.text_color);
                c.cr.move_to(c.lw.layout.addr_pane_width + c.pad, c.font_extents().height * 2.0);
                c.cr.show_text(match &self.brk.label {
                    Some(l) => l.as_str(),
                    None => "unlabeled"
                });
                c.cr.show_text(":");
            },
            _ => ()
        }
    }
}
