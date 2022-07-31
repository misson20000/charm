use gtk::prelude::*;
use crate::view::config;

extern crate conv;
use conv::ApproxInto;

fn update_config<F: 'static, T>(cb: &F, val: T) where
    F: Fn(&mut config::Config, T) {
    let mut cfg = config::set();
    cb(&mut cfg, val);
    cfg.version+= 1;
}

fn edit_spinbutton_integer<T, F: 'static>(label: &str, default: T, setter: F) -> gtk::ListBoxRow where
    F: Fn(&mut config::Config, T),
    T: conv::ApproxInto<f64, conv::DefaultApprox>,
    T: conv::ApproxFrom<i32, conv::DefaultApprox> {
    let lbr = gtk::ListBoxRow::new();
    let bx = gtk::Box::new(gtk::Orientation::Horizontal, 0);
    bx.pack_start(&gtk::Label::new(Some(label)), false, false, 0);

    let sb = gtk::SpinButton::new(
        Some(&gtk::Adjustment::new(
            default.approx_into().unwrap(),
            0.0,
            f64::MAX,
            1.0,
            10.0,
            10.0
        )), 0.001, 3);
    sb.set_numeric(true);
    sb.set_snap_to_ticks(true);
    sb.set_update_policy(gtk::SpinButtonUpdatePolicy::IfValid);
    sb.set_wrap(false);
    sb.connect_value_changed(move |sb| update_config(&setter, sb.value_as_int().approx_into().unwrap()));
    
    bx.pack_end(&sb, false, false, 0);
    lbr.add(&bx);
    
    lbr
}

fn edit_spinbutton_f64<F: 'static>(label: &str, default: f64, setter: F) -> gtk::ListBoxRow where
    F: Fn(&mut config::Config, f64) {
    let lbr = gtk::ListBoxRow::new();
    let bx = gtk::Box::new(gtk::Orientation::Horizontal, 0);
    bx.pack_start(&gtk::Label::new(Some(label)), false, false, 0);

    let sb = gtk::SpinButton::new(
        Some(&gtk::Adjustment::new(
            default,
            0.0,
            f64::MAX,
            1.0,
            10.0,
            10.0
        )), 0.001, 3);
    sb.set_numeric(true);
    sb.set_update_policy(gtk::SpinButtonUpdatePolicy::IfValid);
    sb.set_wrap(false);
    sb.connect_value_changed(move |sb| update_config(&setter, sb.value()));
    
    bx.pack_end(&sb, false, false, 0);
    lbr.add(&bx);
    
    lbr
}

fn edit_color<F: 'static>(label: &str, default: gdk::RGBA, setter: F) -> gtk::ListBoxRow where
    F: Fn(&mut config::Config, gdk::RGBA) {
    let lbr = gtk::ListBoxRow::new();
    let bx = gtk::Box::new(gtk::Orientation::Horizontal, 0);
    bx.pack_start(&gtk::Label::new(Some(label)), false, false, 0);

    let cb: gtk::ColorButton = gtk::builders::ColorButtonBuilder::new()
        .rgba(&default)
        .show_editor(true)
        .title(label)
        .use_alpha(true)
        .build();
    gtk::traits::ColorChooserExt::connect_rgba_notify(&cb, move |cb| update_config(&setter, cb.rgba()));
    
    bx.pack_end(&cb, false, false, 0);
    lbr.add(&bx);
    
    lbr
}

pub fn build_config_editor() -> gtk::ListBox {
    let lb = gtk::ListBox::new();

    let current = config::get();

    lb.add(&edit_spinbutton_integer("File Access Delay", current.file_access_delay, |cfg, v| { cfg.file_access_delay = v; }));
    
    lb.add(&edit_spinbutton_integer("Lookahead", current.lookahead, |cfg, v| { cfg.lookahead = v; }));
    lb.add(&edit_spinbutton_f64("Scroll Wheel Impulse", current.scroll_wheel_impulse, |cfg, v| { cfg.scroll_wheel_impulse = v; }));
    lb.add(&edit_spinbutton_f64("Scroll Deceleration", current.scroll_deceleration, |cfg, v| { cfg.scroll_deceleration = v; }));
    lb.add(&edit_spinbutton_f64("Scroll Spring", current.scroll_spring, |cfg, v| { cfg.scroll_spring = v; }));
    lb.add(&edit_spinbutton_f64("Scroll Spring Damping", current.scroll_spring_damping, |cfg, v| { cfg.scroll_spring_damping = v; }));

    lb.add(&edit_spinbutton_f64("Scroll Align Integer Spring", current.scroll_align_integer_spring, |cfg, v| { cfg.scroll_align_integer_spring = v; }));
    lb.add(&edit_spinbutton_f64("Scroll Align Position Tolerance", current.scroll_align_position_tolerance, |cfg, v| { cfg.scroll_align_position_tolerance = v; }));
    lb.add(&edit_spinbutton_f64("Scroll Align Velocity Tolerance", current.scroll_align_velocity_tolerance, |cfg, v| { cfg.scroll_align_velocity_tolerance = v; }));

    lb.add(&edit_spinbutton_f64("Padding", current.padding, |cfg, v| { cfg.padding = v; }));
    lb.add(&edit_spinbutton_f64("Font Size", current.font_size, |cfg, v| { cfg.font_size = v; }));

    lb.add(&edit_color("Background Color", current.background_color, |cfg, v| { cfg.background_color = v; }));
    lb.add(&edit_color("Address Pane Color", current.addr_pane_color, |cfg, v| { cfg.addr_pane_color = v; }));
    lb.add(&edit_color("Ridge Color", current.ridge_color, |cfg, v| { cfg.ridge_color = v; }));

    lb.add(&edit_color("Address Color", current.addr_color, |cfg, v| { cfg.addr_color = v; }));
    lb.add(&edit_color("Text Color", current.text_color, |cfg, v| { cfg.text_color = v; }));

    lb.add(&edit_color("Cursor Background Color", current.cursor_bg_color, |cfg, v| { cfg.cursor_bg_color = v; }));
    lb.add(&edit_color("Cursor Text Color", current.cursor_fg_color, |cfg, v| { cfg.cursor_fg_color = v; }));
    
    lb.show_all();
    
    return lb;
}
