use std::sync;

use gtk::gdk;
use gtk::pango;

use lazy_static::lazy_static;
use hex_literal::hex;

use crate::view::ext::RGBAExt;

#[derive(Clone)]
pub struct Config {
    pub file_access_delay: u64, /* milliseconds */
    
    pub lookahead: usize, /* lines */
    pub scroll_wheel_impulse: f64, /* lines/second */
    pub scroll_deceleration: f64, /* lines/second^2 */
    pub scroll_spring: f64, /* 1/second^2 */
    pub scroll_spring_damping: f64, /* viscous damping coefficient */
    
    pub scroll_align_integer: bool,
    pub scroll_align_integer_spring: f64,
    pub scroll_align_integer_spring_damping: f64,
    pub scroll_align_position_tolerance: f64,
    pub scroll_align_velocity_tolerance: f64,
    pub page_navigation_leadup: usize, /* lines */
    
    pub padding: f64, /* pixels */
    pub mode_line_padding: f64, /* pixels */
    pub font_size: f64, /* pixels */

    pub indentation_width: f32, /* characters */

    pub background_color: gdk::RGBA,
    pub addr_pane_color: gdk::RGBA,
    pub ridge_color: gdk::RGBA,

    pub addr_color: gdk::RGBA,
    pub text_color: gdk::RGBA,
    pub patch_color: gdk::RGBA,
    pub placeholder_color: gdk::RGBA,
    pub selection_color: gdk::RGBA,

    pub addr_pane_bold: bool,
    pub breaks_bold: bool,

    pub cursor_bg_color: gdk::RGBA,
    pub cursor_fg_color: gdk::RGBA,
    pub cursor_blink_period: f64,

    pub mode_line_color: gdk::RGBA,

    pub mode_defocused_color: gdk::RGBA,
    pub mode_command_color: gdk::RGBA,
    pub mode_entry_color: gdk::RGBA,
    pub mode_text_entry_color: gdk::RGBA,

    pub monospace_font: pango::FontDescription,

    pub show_token_bounds: bool,
}

impl Default for Config {
    fn default() -> Config {
        Config {
            file_access_delay: 0,
            
            lookahead: 0,
            scroll_wheel_impulse: 60.0,
            scroll_deceleration: 620.0,
            scroll_spring: 240.0,
            scroll_spring_damping: 17.0,
            
            scroll_align_integer: true,
            scroll_align_integer_spring: 50.0,
            scroll_align_integer_spring_damping: 80.0,
            scroll_align_position_tolerance: 0.05,
            scroll_align_velocity_tolerance: 2.0,
            page_navigation_leadup: 5,

            padding: 15.0,
            mode_line_padding: 8.0,
            font_size: 14.0,

            indentation_width: 2.0,

            background_color: gdk::RGBA::bytes(hex!("090909ff")),
            addr_pane_color: gdk::RGBA::bytes(hex!("ffffff12")),
            ridge_color: gdk::RGBA::bytes(hex!("0000000c")),

            addr_color: gdk::RGBA::bytes(hex!("8891efff")),
            text_color: gdk::RGBA::bytes(hex!("dbdbe6ff")),
            patch_color: gdk::RGBA::bytes(hex!("ffff00ff")),
            placeholder_color: gdk::RGBA::bytes(hex!("141414ff")),
            selection_color: gdk::RGBA::bytes(hex!("8888ff60")),

            addr_pane_bold: true,
            breaks_bold: true,

            cursor_bg_color: gdk::RGBA::bytes(hex!("8891efff")),
            cursor_fg_color: gdk::RGBA::bytes(hex!("090909ff")),
            cursor_blink_period: 1.0,

            mode_line_color: gdk::RGBA::bytes(hex!("404040ff")),
            mode_defocused_color: gdk::RGBA::bytes(hex!("606060ff")),

            mode_command_color: gdk::RGBA::bytes(hex!("8891efff")),
            mode_entry_color: gdk::RGBA::bytes(hex!("ffbf48ff")),
            mode_text_entry_color: gdk::RGBA::bytes(hex!("ff4921ff")),

            monospace_font: pango::FontDescription::from_string("Monospace Regular 12"),

            show_token_bounds: true,
        }
    }
}

lazy_static! {
    static ref INSTANCE: arc_swap::ArcSwap<Config> = arc_swap::ArcSwap::from_pointee(Config::default());
}

pub type Guard = arc_swap::Guard<sync::Arc<Config>>;
pub type Copy = sync::Arc<Config>;

pub fn borrow() -> Guard {
    INSTANCE.load()
}

pub fn copy() -> Copy {
    INSTANCE.load_full()
}

pub fn update(new: Config) {
    INSTANCE.store(sync::Arc::new(new));
}
