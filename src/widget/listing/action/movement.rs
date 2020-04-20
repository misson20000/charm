use std::sync;

use crate::widget;

pub fn create_goto_start_of_line(rc: &sync::Arc<parking_lot::RwLock<widget::listing::ListingWidget>>, da: &gtk::DrawingArea) -> gio::SimpleAction {
    let action = gio::SimpleAction::new("goto_start_of_line", None);
    
    let rc_clone = rc.clone();
    let da_clone = da.clone(); // TODO: strong pointer here might be problematic
    action.set_enabled(true);
    action.connect_activate(move |_act, _par| {
        let mut lw = rc_clone.write();
        let lww = &mut *lw; /* compiler has a hard time seeing splitting borrow through RwLockWriteGuard */
        
        lww.cursor_view.move_to_start_of_line();
        lww.scroll.ensure_cursor_is_in_view(&mut lww.window, &lww.cursor_view, widget::listing::component::scroll::EnsureCursorInViewDirection::Up);
        lww.collect_draw_events(&da_clone);
    });

    action
}

pub fn create_goto_end_of_line(rc: &sync::Arc<parking_lot::RwLock<widget::listing::ListingWidget>>, da: &gtk::DrawingArea) -> gio::SimpleAction {
    let action = gio::SimpleAction::new("goto_end_of_line", None);
    
    let rc_clone = rc.clone();
    let da_clone = da.clone(); // TODO: strong pointer here might be problematic
    action.set_enabled(true);
    action.connect_activate(move |_act, _par| {
        let mut lw = rc_clone.write();
        let lww = &mut *lw; /* compiler has a hard time seeing splitting borrow through RwLockWriteGuard */
        
        lww.cursor_view.move_to_end_of_line();
        lww.scroll.ensure_cursor_is_in_view(&mut lww.window, &lww.cursor_view, widget::listing::component::scroll::EnsureCursorInViewDirection::Down);
        lww.collect_draw_events(&da_clone);
    });

    action
}
