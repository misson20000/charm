use crate::view::config;

use gtk::gdk;

pub enum Mode {
    Command,
    Entry,
    TextEntry,
}

impl Mode {
    pub fn color<'a>(&self, config: &'a config::Config) -> &'a gdk::RGBA {
        match self {
            Mode::Command => config.mode_command_color.rgba(),
            Mode::Entry => config.mode_entry_color.rgba(),
            Mode::TextEntry => config.mode_text_entry_color.rgba(),
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Mode::Command => "COMMAND",
            Mode::Entry => "ENTRY",
            Mode::TextEntry => "TEXT ENTRY",
        }
    }
}

impl Default for Mode {
    fn default() -> Self {
        Mode::Command
    }
}

impl glib::variant::ToVariant for Mode {
    fn to_variant(&self) -> glib::Variant {
        match self {
            Mode::Command => "command",
            Mode::Entry => "entry",
            Mode::TextEntry => "utf8",
        }.to_variant()
    }
}
