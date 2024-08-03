use std::sync;

use gtk::gdk;
use gtk::pango;
use gtk::prelude::*;
use glib::clone;

use lazy_static::lazy_static;
use hex_literal::hex;

use crate::model::versioned;
use crate::model::versioned::Versioned;
use crate::view::ext::RGBAExt;

#[derive(Clone, Debug)]
pub struct Color {
    pub light: gdk::RGBA,
    pub dark: gdk::RGBA,
}

pub trait Item: Sized + Clone {
    type Binding;
    
    fn bind<F: Fn(Self) + Clone + 'static>(builder: &gtk::Builder, id: &str, changer: F) -> Self::Binding;
    fn update_binding(&self, binding: &Self::Binding);
}

pub trait ItemVisitor<ConfigType: versioned::Versioned, ItemType> {
    fn visit(&mut self, acc: Accessor<ConfigType, ItemType>);
}

pub struct Accessor<ConfigType: versioned::Versioned, ItemType> {
    pub reader: fn(&ConfigType) -> &ItemType,
    pub changer: fn(ItemType) -> <ConfigType as versioned::Versioned>::Change,
}

macro_rules! color {
    ($light:literal, $dark:literal) => {
        Color {
            light: gdk::RGBA::bytes(hex!($light)),
            dark: gdk::RGBA::bytes(hex!($dark)),
        }
    };
    ($both:literal) => {
        Color {
            light: gdk::RGBA::bytes(hex!($both)),
            dark: gdk::RGBA::bytes(hex!($both)),
        }
    };
}

macro_rules! bind2asyncsubscriber {
    ($literal:literal) => {$crate::view::helpers::AsyncSubscriber};
}

macro_rules! declare_config {
    [ $typename:ident {
        $($(#[bind($bind_name:literal)])? $name:ident : $type:path = $default:expr),* $(,)?
    } ] => {
        #[derive(Clone, Debug)]
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
                        reader: |config| &config.$name,
                        changer: |value| Change::$name(value),
                    });
                )*
            }

            #[allow(unused_variables)]
            pub fn bind(builder: &gtk::Builder, host: sync::Arc<$crate::model::versioned::Host::<$typename>>) -> ($($(bind2asyncsubscriber!($bind_name),)?)*) {
                let initial_config = host.get();
                
                let subscribers = ($(
                    $({
                        let host_clone = host.clone();
                        let current = sync::Arc::new(sync::Mutex::new(initial_config.clone()));
                        let current_clone = current.clone();
                        
                        let binding = <$type as $crate::view::config::Item>::bind(builder, $bind_name, move |new_value| {
                            let new_config = host_clone.change(<Self as $crate::model::versioned::Versioned>::Change::$name(new_value)).unwrap();
                            /* ensures we don't try to send the update back to the binding */
                            *current_clone.lock().unwrap() = new_config;
                        });

                        initial_config.$name.update_binding(&binding);

                        $crate::view::helpers::subscribe_to_updates(
                            sync::Arc::downgrade(&current),
                            host.clone(),
                            initial_config.clone(),
                            move |current, new_config| {
                                let mut guard = current.lock().unwrap();
                                new_config.changes_since(&**guard, &mut |config, change| {
                                    match change {
                                        Change::$name(new_value) => new_value.update_binding(&binding),
                                        _ => {},
                                    };
                                });
                                *guard = new_config.clone();
                            })
                    },)?
                )*);

                subscribers
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
            type ApplyRecord = Self;

            fn apply(self, object: &mut $typename) -> Result<(Self, Self::ApplyRecord), Self::ApplyError> {
                match &self {
                    $(
                        Change::$name(value) => object.$name = value.clone(),
                    )*
                };
                Ok((self.clone(), self))
            }
        }
    };
}

declare_config![Config {
    file_access_delay: u64 = 0, /* milliseconds */
    
    lookahead: usize = 20, /* lines */
    scroll_wheel_impulse: f64 = 60.0, /* lines/second */

    scroll_deceleration: f64 = 400.0, /* lines/second^2 */
    scroll_spring: f64 = 240.0, /* 1/second^2 */
    scroll_spring_damping: f64 = 17.0, /* viscous damping coefficient */
    
    scroll_align_integer: bool = true,
    scroll_align_integer_spring: f64 = 50.0,
    scroll_align_integer_spring_damping: f64 = 80.0,
    scroll_align_position_tolerance: f64 = 0.05,
    scroll_align_velocity_tolerance: f64 = 2.0,
    
    page_navigation_leadup: usize = 5, /* lines */
    
    padding: f64 = 15.0, /* pixels */

    indentation_width: f32 = 2.0, /* characters */

    #[bind("color-background")]
    background_color: Color = color!("ffffff00", "090909ff"),

    #[bind("color-address-pane")]
    addr_pane_color: Color = color!("00000012", "ffffff12"),

    #[bind("color-address")]
    addr_color: Color = color!("545ca7ff", "8891efff"),

    #[bind("color-text")]
    text_color: Color = color!("121214ff", "dbdbe6ff"),

    #[bind("color-placeholder")]
    placeholder_color: Color = color!("141414ff"),

    #[bind("color-selection")]
    selection_color: Color = color!("8888ff60"),

    #[bind("color-cursor-background")]
    cursor_bg_color: Color = color!("8891efff"),
    
    #[bind("color-cursor-foreground")]
    cursor_fg_color: Color = color!("090909ff"),
    
    cursor_blink_period: f64 = 1.0,

    #[bind("font")]
    monospace_font: pango::FontDescription = pango::FontDescription::from_string("Monospace Regular 12"),

    show_token_bounds: bool = false,
}];

pub type Host = versioned::Host<Config>;

lazy_static! {
    pub static ref INSTANCE: sync::Arc<Host> = sync::Arc::new(Host::default());
}

impl Item for Color {
    type Binding = (gtk::ColorButton, gtk::ColorButton);
    
    fn bind<F: Fn(Self) + Clone + 'static>(builder: &gtk::Builder, id: &str, changer: F) -> Self::Binding {
        let light_button = builder.object::<gtk::ColorButton>(&format!("{}-light", id)).unwrap_or_else(|| panic!("builder did not have ColorButton '{}-light'", id));
        let dark_button = builder.object::<gtk::ColorButton>(&format!("{}-dark", id)).unwrap_or_else(|| panic!("builder did not have ColorButton '{}-dark'", id));

        light_button.set_show_editor(true);
        dark_button.set_show_editor(true);
        
        light_button.connect_rgba_notify(clone!(@weak dark_button, @strong changer => move |lb| {
            changer(Color {
                light: lb.rgba(),
                dark: dark_button.rgba()
            });
        }));
        
        dark_button.connect_rgba_notify(clone!(@weak light_button => move |db| {
            changer(Color {
                light: light_button.rgba(),
                dark: db.rgba()
            });
        }));
                                         
        (light_button, dark_button)
    }

    fn update_binding(&self, binding: &Self::Binding) {
        binding.0.set_rgba(&self.light);
        binding.1.set_rgba(&self.dark);
    }
}

impl Item for pango::FontDescription {
    type Binding = gtk::FontButton;
    
    fn bind<F: Fn(Self) + 'static>(builder: &gtk::Builder, id: &str, changer: F) -> Self::Binding {
        let button = builder.object::<gtk::FontButton>(id).unwrap();

        button.set_filter_func(|family, face| {
            family.is_monospace()
        });
        
        button.connect_font_set(move |b| {
            if let Some(desc) = b.font_desc() {
                changer(desc);
            }
        });
                                         
        button
    }

    fn update_binding(&self, binding: &Self::Binding) {
        binding.set_font_desc(self);
    }
}

impl Color {
    pub fn rgba(&self) -> &gdk::RGBA {
        &self.dark
    }
}
