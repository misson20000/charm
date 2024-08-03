use std::sync;

use gtk::gdk;
use gtk::pango;
use gtk::prelude::*;
use glib::clone;

use lazy_static::lazy_static;
use hex_literal::hex;
use serde::ser::SerializeMap;
use serde::de::Error;

use crate::model::versioned;
use crate::model::versioned::Versioned;
use crate::view::ext::RGBAExt;
use crate::view::helpers;

#[derive(Clone, Debug)]
pub struct Color {
    pub light: gdk::RGBA,
    pub dark: gdk::RGBA,
}

#[derive(Clone, Debug)]
pub struct Font(pub pango::FontDescription);

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

        impl serde::Serialize for $typename {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: serde::Serializer {
                let mut map = serializer.serialize_map(None)?;
                $(
                    map.serialize_entry(stringify!($name), &self.$name)?;
                )*
                map.end()
            }
        }
        
        impl<'de> serde::Deserialize<'de> for $typename {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: serde::Deserializer<'de> {
                const FIELDS: &[&str] = &[$(stringify!($name),)*];

                struct ConfigVisitor;

                impl<'de> serde::de::Visitor<'de> for ConfigVisitor {
                    type Value = Config;

                    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                        formatter.write_str(stringify!($typename))
                    }
                    
                    fn visit_map<V>(self, mut map: V) -> Result<Self::Value, V::Error> where V: serde::de::MapAccess<'de> {
                        let mut config = Config::default();
                        
                        while let Some(key) = map.next_key::<String>()? {
                            match key.as_str() {
                                $(
                                    stringify!($name) => {
                                        match map.next_value() {
                                            Ok(v) => config.$name = v,
                                            Err(e) => {
                                                println!("failed to deserialize {} in {}: {}", stringify!($name), stringify!($typename), e);
                                            }
                                        }
                                    },
                                )*
                                _ => {/* ignore unknown keys*/},
                            }
                        }
                        
                        Ok(config)
                    }
                }
                
                deserializer.deserialize_struct("Config", FIELDS, ConfigVisitor)
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
    monospace_font: Font = Font(pango::FontDescription::from_string("Monospace Regular 12")),

    show_token_bounds: bool = false,

    #[bind("dark-mode")]
    dark_mode: bool = true,
}];

fn config_path() -> Option<std::path::PathBuf> {
    match xdg::BaseDirectories::new() {
        Ok(bd) => match bd.place_config_file("charm.toml") {
            Ok(path) => Some(path),
            Err(e) => {
                println!("failed to place configuration file within directory: {}", e);
                None
            },
        },
        Err(e) => {
            println!("failed to find configuration directory: {}", e);
            None
        }
    }
}

fn load_config() -> Option<Config> {
    let config_str = match std::fs::read_to_string(config_path()?) {
        Ok(config_str) => config_str,
        /* don't bother printing an error if the config file didn't exist */
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return None,
        Err(e) => {
            println!("failed to read configuration: {}", e);
            return None;
        }
    };
    
    match toml::from_str(&config_str) {
        Ok(config) => Some(config),
        Err(e) => {
            println!("failed to deserialize configuration: {}", e);
            None
        }
    }
}

pub fn create_persist_config_task() -> Option<helpers::AsyncSubscriber> {
    let path = config_path()?;

    let initial = INSTANCE.get();
    Some(helpers::subscribe_to_updates(
        std::marker::PhantomData::<()>,
        INSTANCE.clone(),
        initial.clone(),
        move |_, new_config| {
            if !sync::Arc::ptr_eq(&initial, new_config) {
                let config_str = match toml::to_string(&**new_config) {
                    Ok(s) => s,
                    Err(e) => {
                        println!("failed to serialize configuration: {}", e);
                        return
                    },
                };

                match std::fs::write(&path, config_str) {
                    Ok(_) => {},
                    Err(e) => {
                        println!("failed to write configuration: {}", e);
                    },
                }
            }
        }))
}

pub fn create_sync_dark_mode_task() -> helpers::AsyncSubscriber {
    let initial = INSTANCE.get();
    let style_manager = adw::StyleManager::default();

    let current = sync::Arc::new(sync::Mutex::new(initial.clone()));
    let current_clone = current.clone();

    style_manager.set_color_scheme(match initial.dark_mode {
        true => adw::ColorScheme::ForceDark,
        false => adw::ColorScheme::ForceLight,
    });
    
    style_manager.connect_dark_notify(move |style_manager| {
        let mut guard = current_clone.lock().unwrap();
        if style_manager.is_dark() != guard.dark_mode {
            let new_config = INSTANCE.change(Change::dark_mode(style_manager.is_dark())).unwrap();
            *guard = new_config;
        }
    });

    helpers::subscribe_to_updates(
        sync::Arc::downgrade(&current),
        INSTANCE.clone(),
        initial.clone(),
        move |current, new_config| {
            let mut guard = current.lock().unwrap();
            let mut dark_mode = guard.dark_mode;
            new_config.changes_since(&**guard, &mut |_config, change| {
                match change {
                    Change::dark_mode(value) => dark_mode = *value,
                    _ => {},
                };
            });
            *guard = new_config.clone();

            std::mem::drop(guard);
            
            style_manager.set_color_scheme(match dark_mode {
                true => adw::ColorScheme::ForceDark,
                false => adw::ColorScheme::ForceLight,
            })
        })
}

pub type Host = versioned::Host<Config>;

lazy_static! {
    pub static ref INSTANCE: sync::Arc<Host> = sync::Arc::new(Host::new(load_config().unwrap_or_else(Config::default)));
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

impl serde::Serialize for Color {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: serde::Serializer {
        let mut map = serializer.serialize_map(Some(2))?;
        map.serialize_entry("light", &self.light.to_string())?;
        map.serialize_entry("dark", &self.dark.to_string())?;
        map.end()
    }
}

impl<'de> serde::Deserialize<'de> for Color {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: serde::Deserializer<'de> {
        const FIELDS: &[&str] = &["light", "dark"];

        struct ColorVisitor;

        impl<'de> serde::de::Visitor<'de> for ColorVisitor {
            type Value = Color;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("struct Color or single color")
            }
            
            fn visit_map<V>(self, mut map: V) -> Result<Self::Value, V::Error> where V: serde::de::MapAccess<'de> {
                let mut light = None;
                let mut dark = None;
                
                while let Some(key) = map.next_key::<String>()? {
                    match key.as_str() {
                        "light" => light = Some(gdk::RGBA::parse(map.next_value::<String>()?).map_err(|e| V::Error::custom(e.message))?),
                        "dark" => dark = Some(gdk::RGBA::parse(map.next_value::<String>()?).map_err(|e| V::Error::custom(e.message))?),
                        _ => {/* ignore unknown fields */},
                    }
                }

                Ok(match (light, dark) {
                    (None, None) => return Err(V::Error::missing_field("light or dark")),
                    (Some(light), Some(dark)) => Color { light, dark },
                    (Some(light), None) => Color { light: light.clone(), dark: light },
                    (None, Some(dark)) => Color { light: dark.clone(), dark: dark },
                })
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E> where E: serde::de::Error {
                let rgba = gdk::RGBA::parse(v).map_err(|e| E::custom(e.message))?;

                Ok(Color {
                    light: rgba.clone(),
                    dark: rgba
                })
            }
        }
        
        deserializer.deserialize_any(ColorVisitor)
    }
}

impl serde::Serialize for Font {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: serde::Serializer {
        serializer.serialize_str(&self.0.to_str())
    }
}

impl<'de> serde::Deserialize<'de> for Font {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: serde::Deserializer<'de> {
        struct FontVisitor;

        impl<'de> serde::de::Visitor<'de> for FontVisitor {
            type Value = Font;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("font description string")
            }
            
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E> where E: serde::de::Error {
                Ok(Font(pango::FontDescription::from_string(v)))
            }
        }

        deserializer.deserialize_str(FontVisitor)
    }
}

impl Item for Font {
    type Binding = gtk::FontButton;
    
    fn bind<F: Fn(Self) + 'static>(builder: &gtk::Builder, id: &str, changer: F) -> Self::Binding {
        let button = builder.object::<gtk::FontButton>(id).unwrap();

        button.set_filter_func(|family, _face| {
            family.is_monospace()
        });
        
        button.connect_font_set(move |b| {
            if let Some(desc) = b.font_desc() {
                changer(Font(desc));
            }
        });
                                         
        button
    }

    fn update_binding(&self, binding: &Self::Binding) {
        binding.set_font_desc(&self.0);
    }
}

impl Color {
    pub fn rgba(&self) -> &gdk::RGBA {
        if adw::StyleManager::default().is_dark() {
            &self.dark
        } else {
            &self.light
        }
    }
}

impl Item for bool {
    type Binding = gtk::Switch;
    
    fn bind<F: Fn(Self) + Clone + 'static>(builder: &gtk::Builder, id: &str, changer: F) -> Self::Binding {
        let switch = builder.object::<gtk::Switch>(id).unwrap();

        switch.connect_active_notify(move |b| {
            let state = b.is_active();
            changer(state);
            b.set_state(state);
        });
                                         
        switch
    }

    fn update_binding(&self, binding: &Self::Binding) {
        binding.set_state(*self)
    }
}
