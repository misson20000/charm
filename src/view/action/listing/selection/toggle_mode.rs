use std::rc;

use crate::view::helpers;
use crate::view::listing;
use crate::view::listing::selection_mode::SelectionMode;
use crate::view::window;

use gtk::prelude::*;

struct SelectionModeAction {
    lw: listing::ListingWidget,
}

pub fn add_action(window_context: &window::WindowContext) {
    let action = rc::Rc::new(SelectionModeAction {
        lw: window_context.lw.clone(),
    });

    window_context.action_group.add_action(&helpers::create_stateful_action_strong(action, "selection-mode", window_context.lw.get_selection_mode().name().to_string(), |gio, action, state| {
        let mode = match state.as_ref().map(|s| s.as_str()) {
            /* If no parameter (or empty parameter) was given, cycle. */
            None | Some("") => match action.lw.get_selection_mode() {
                SelectionMode::Structure => SelectionMode::Address,
                SelectionMode::Address => SelectionMode::Structure,
            },

            Some(x) => match SelectionMode::from_name(x) {
                Some(x) => x,
                None => return,
            },
        };

        gio.set_state(&mode.to_variant());
        action.lw.set_selection_mode(mode);
    }));
}
