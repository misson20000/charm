use std::rc;

use crate::view::helpers;
use crate::view::listing;
use crate::view::listing::mode::Mode;
use crate::view::window;

use gtk::prelude::*;

struct ModeAction {
    lw: listing::ListingWidget,
}

pub fn add_action(window_context: &window::WindowContext) {
    let action = rc::Rc::new(ModeAction {
        lw: window_context.lw.clone(),
    });

    window_context.action_group.add_action(&helpers::create_stateful_action_strong(action, "mode", "command".to_string(), |gio, action, state| {
        let mode = match state.as_ref().map(|s| s.as_str()) {
            Some("command") => Mode::Command,
            Some("entry") => Mode::Entry,
            Some("utf8") => Mode::TextEntry,
            _ => return
        };

        gio.set_state(&mode.to_variant());
        action.lw.set_mode(mode);
    }));
}
