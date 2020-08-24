use std::sync;

use lazy_static::lazy_static;
use hex_literal::hex;
use crate::ext::RGBAExt;

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
    pub font_size: f64, /* pixels */

    pub background_color: gdk::RGBA,
    pub addr_pane_color: gdk::RGBA,
    pub ridge_color: gdk::RGBA,

    pub addr_color: gdk::RGBA,
    pub text_color: gdk::RGBA,

    pub addr_pane_bold: bool,
    pub breaks_bold: bool,

    pub cursor_bg_color: gdk::RGBA,
    pub cursor_fg_color: gdk::RGBA,
    pub cursor_blink_period: f64,

    pub mode_line_color: gdk::RGBA,

    pub mode_command_color: gdk::RGBA,
    pub mode_entry_color: gdk::RGBA,
    pub mode_text_entry_color: gdk::RGBA,
    
    pub version: usize, /* incremented when config changes */
}

lazy_static! {
    static ref INSTANCE: sync::RwLock<Config> = sync::RwLock::new(Config {
        file_access_delay: 0,
        
        lookahead: 25,
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
        font_size: 14.0,

        background_color: gdk::RGBA::bytes(hex!("090909ff")),
        addr_pane_color: gdk::RGBA::bytes(hex!("ffffff12")),
        ridge_color: gdk::RGBA::bytes(hex!("0000000c")),

        addr_color: gdk::RGBA::bytes(hex!("8891efff")),
        text_color: gdk::RGBA::bytes(hex!("dbdbe6ff")),

        addr_pane_bold: true,
        breaks_bold: true,

        cursor_bg_color: gdk::RGBA::bytes(hex!("8891efff")),
        cursor_fg_color: gdk::RGBA::bytes(hex!("090909ff")),
        cursor_blink_period: 1.0,

        mode_line_color: gdk::RGBA::bytes(hex!("404040ff")),

        mode_command_color: gdk::RGBA::bytes(hex!("8891efff")),
        mode_entry_color: gdk::RGBA::bytes(hex!("ffbf48ff")),
        mode_text_entry_color: gdk::RGBA::bytes(hex!("ff4921ff")),
        
        version: 0,
    });
}

pub fn get() -> sync::RwLockReadGuard<'static, Config> {
    INSTANCE.read().unwrap()
}

pub fn set() -> sync::RwLockWriteGuard<'static, Config> {
    INSTANCE.write().unwrap()
}
