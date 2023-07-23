use std::sync;

use gtk::gdk;
use gtk::pango;

use lazy_static::lazy_static;
use hex_literal::hex;

use crate::model::versioned;
use crate::view::ext::RGBAExt;

pub trait ItemVisitor<ConfigType: versioned::Versioned, ItemType> {
    fn visit(&mut self, acc: Accessor<ConfigType, ItemType>);
}

pub struct Accessor<ConfigType: versioned::Versioned, ItemType> {
    pub description: &'static str,
    pub reader: fn(&ConfigType) -> &ItemType,
    pub changer: fn(ItemType) -> <ConfigType as versioned::Versioned>::Change,
}

macro_rules! declare_config {
    [ $typename:ident {
        $( $description:literal $name:ident : $type:path = $default:expr),* $(,)?
    } ] => {
        #[derive(Clone)]
        pub struct $typename {
            $(
                pub $name: $type,
            )*

            version: $crate::model::versioned::Version::<$typename>
        }
        
        impl Default for $typename {
            fn default() -> Self {
                Self {
                    $(
                        $name: $default,
                    )*

                    version: core::default::Default::default()
                }
            }
        }
        
        impl $typename {
            #[allow(unused_variables)]
            pub fn visit_all<Visitor>(visitor: &mut Visitor) where $(Visitor: $crate::view::config::ItemVisitor::<Self, $type>,)* {
                $(
                    $crate::view::config::ItemVisitor::<Self, $type>::visit(visitor, $crate::view::config::Accessor::<Self, $type> {
                        description: $description,
                        reader: |config| &config.$name,
                        changer: |value| Change::$name(value),
                    });
                )*
            }
        }

        #[allow(non_camel_case_types)]
        #[derive(Debug, Clone)]
        pub enum Change {
            $(
                $name($type),
            )*
        }
        
        impl $crate::model::versioned::Versioned for $typename {
            type Change = Change;

            fn version(&self) -> &versioned::Version::<Config> {
                &self.version
            }

            fn version_mut(&mut self) -> &mut versioned::Version::<Config> {
                &mut self.version
            }
        }

        impl $crate::model::versioned::Change::<$typename> for Change {
            type ApplyError = ();
            type ApplyRecord = ();

            fn apply(self, object: &mut $typename) -> Result<(Self, Self::ApplyRecord), Self::ApplyError> {
                match &self {
                    $(
                        Change::$name(value) => object.$name = value.clone(),
                    )*
                };
                Ok((self, ()))
            }
        }
    };
}

declare_config![Config {
    "File Access Delay" file_access_delay: u64 = 0, /* milliseconds */
    
    "Lookahead" lookahead: usize = 0, /* lines */
    "Scroll Wheel Impulse" scroll_wheel_impulse: f64 = 60.0, /* lines/second */

    "Scroll Deceleration" scroll_deceleration: f64 = 620.0, /* lines/second^2 */
    "Scroll Spring" scroll_spring: f64 = 240.0, /* 1/second^2 */
    "Scroll Spring Damping" scroll_spring_damping: f64 = 17.0, /* viscous damping coefficient */
    
    "Scroll Align Integer" scroll_align_integer: bool = true,
    "Scroll Align Integer Spring" scroll_align_integer_spring: f64 = 50.0,
    "Scroll Align Integer Spring Damping" scroll_align_integer_spring_damping: f64 = 80.0,
    "Scroll Align Position Tolerance" scroll_align_position_tolerance: f64 = 0.05,
    "Scroll Align Velocity Tolerance" scroll_align_velocity_tolerance: f64 = 2.0,
    
    "Page Navigation Leadup" page_navigation_leadup: usize = 5, /* lines */
    
    "Padding" padding: f64 = 15.0, /* pixels */
    "Mode Line Padding" mode_line_padding: f64 = 8.0, /* pixels */
    "Font Size" font_size: f64 = 14.0, /* pixels */

    "Indentation Width" indentation_width: f32 = 2.0, /* characters */

    "Background Color" background_color: gdk::RGBA = gdk::RGBA::bytes(hex!("090909ff")),
    "Address Pane Color" addr_pane_color: gdk::RGBA = gdk::RGBA::bytes(hex!("ffffff12")),
    "Ridge Color" ridge_color: gdk::RGBA = gdk::RGBA::bytes(hex!("0000000c")),

    "Address Color" addr_color: gdk::RGBA = gdk::RGBA::bytes(hex!("8891efff")),
    "Text Color" text_color: gdk::RGBA = gdk::RGBA::bytes(hex!("dbdbe6ff")),
    "Patch Color" patch_color: gdk::RGBA = gdk::RGBA::bytes(hex!("ffff00ff")),
    "Placeholder Color" placeholder_color: gdk::RGBA = gdk::RGBA::bytes(hex!("141414ff")),
    "Selection Color" selection_color: gdk::RGBA = gdk::RGBA::bytes(hex!("8888ff60")),

    "Address Pane is Bold" addr_pane_bold: bool = true,

    "Cursor Background Color" cursor_bg_color: gdk::RGBA = gdk::RGBA::bytes(hex!("8891efff")),
    "Cursor Foreground Color" cursor_fg_color: gdk::RGBA = gdk::RGBA::bytes(hex!("090909ff")),
    "Cursor Blink Period" cursor_blink_period: f64 = 1.0,

    "Mode Line Color" mode_line_color: gdk::RGBA = gdk::RGBA::bytes(hex!("404040ff")),

    "Mode Defocused Color" mode_defocused_color: gdk::RGBA = gdk::RGBA::bytes(hex!("606060ff")),
    "Mode Command Color" mode_command_color: gdk::RGBA = gdk::RGBA::bytes(hex!("8891efff")),
    "Mode Entry Color" mode_entry_color: gdk::RGBA = gdk::RGBA::bytes(hex!("ffbf48ff")),
    "Mode Text Entry Color" mode_text_entry_color: gdk::RGBA = gdk::RGBA::bytes(hex!("ff4921ff")),

    "Monospace Font" monospace_font: pango::FontDescription = pango::FontDescription::from_string("Monospace Regular 12"),

    "Show Token Bounds" show_token_bounds: bool = false,
}];

pub type Host = versioned::Host<Config>;

lazy_static! {
    pub static ref INSTANCE: sync::Arc<Host> = sync::Arc::new(Host::default());
}
