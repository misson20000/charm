use std::sync;

use crate::config;
use crate::widget;

pub fn create_goto_start_of_line(rc: &sync::Arc<sync::RwLock<widget::listing::ListingWidget>>, _da: &gtk::DrawingArea) -> gio::SimpleAction {
    let action = gio::SimpleAction::new("goto_start_of_line", None);
    
    let rc_clone = rc.clone();
    action.set_enabled(true);
    action.connect_activate(move |_act, _par| {
        let mut lw = rc_clone.write().unwrap();
        let engine = lw.engine.clone();
        lw.cursor.move_to_start_of_line(&engine);
        lw.ensure_cursor_is_in_view(&config::get(), widget::listing::CursorEnsureInViewDirection::Up);
    });

    action
}

pub fn create_goto_end_of_line(rc: &sync::Arc<sync::RwLock<widget::listing::ListingWidget>>, _da: &gtk::DrawingArea) -> gio::SimpleAction {
    let action = gio::SimpleAction::new("goto_end_of_line", None);
    
    let rc_clone = rc.clone();
    action.set_enabled(true);
    action.connect_activate(move |_act, _par| {
        let mut lw = rc_clone.write().unwrap();
        let engine = lw.engine.clone();
        lw.cursor.move_to_end_of_line(&engine);
        lw.ensure_cursor_is_in_view(&config::get(), widget::listing::CursorEnsureInViewDirection::Down);
    });

    action
}
