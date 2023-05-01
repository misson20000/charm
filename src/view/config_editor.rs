use crate::view::config;

use gtk::gdk;
use gtk::glib;
use gtk::pango;
use gtk::prelude::*;

use std::sync;

struct ConfigEditorBuilder<'a> {
    host: sync::Arc<config::Host>,
    current: &'a config::Config,
    lb: gtk::ListBox,
}

impl<'a> ConfigEditorBuilder<'a> {
    fn add_widget<T, W: glib::object::IsA<gtk::Widget>>(&mut self, acc: config::Accessor<config::Config, T>, widget: W) {
        let lbr = gtk::ListBoxRow::new();
        let bx = gtk::Box::new(gtk::Orientation::Horizontal, 0);
        bx.prepend(&gtk::Label::new(Some(acc.description)));

        widget.set_halign(gtk::Align::End);
        widget.set_hexpand(true);
        
        bx.append(&widget);
        
        lbr.set_child(Some(&bx));
        lbr.set_hexpand(true);

        self.lb.append(&lbr);
    }

    fn create_changer<T>(&self, acc: &config::Accessor<config::Config, T>) -> impl Fn(T) {
        let host = self.host.clone();
        let changer = acc.changer;
        move |new_value| { host.change((changer)(new_value)).expect("configuration updates shouldn't fail"); }
    }
    
    fn visit_spinbutton<T: Clone + 'static>(&mut self, acc: config::Accessor<config::Config, T>) where
        T: conv::ApproxInto<f64, conv::DefaultApprox>,
        T: conv::ApproxFrom<f64, conv::DefaultApprox> {
        let default = (acc.reader)(self.current).clone();
        
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

        let changer = self.create_changer(&acc);
        sb.connect_value_changed(move |sb| changer(conv::ApproxInto::approx_into(sb.value()).unwrap()));

        self.add_widget(acc, sb);
    }
}

impl<'a> config::ItemVisitor<config::Config, f64> for ConfigEditorBuilder<'a> {
    fn visit(&mut self, acc: config::Accessor<config::Config, f64>) {
        self.visit_spinbutton(acc);
    }
}

impl<'a> config::ItemVisitor<config::Config, f32> for ConfigEditorBuilder<'a> {
    fn visit(&mut self, acc: config::Accessor<config::Config, f32>) {
        self.visit_spinbutton(acc);
    }
}

impl<'a> config::ItemVisitor<config::Config, u64> for ConfigEditorBuilder<'a> {
    fn visit(&mut self, acc: config::Accessor<config::Config, u64>) {
        self.visit_spinbutton(acc);
    }
}

impl<'a> config::ItemVisitor<config::Config, usize> for ConfigEditorBuilder<'a> {
    fn visit(&mut self, acc: config::Accessor<config::Config, usize>) {
        self.visit_spinbutton(acc);
    }
}

impl<'a> config::ItemVisitor<config::Config, bool> for ConfigEditorBuilder<'a> {
    fn visit(&mut self, acc: config::Accessor<config::Config, bool>) {
        let cb: gtk::CheckButton = gtk::CheckButton::new();
        cb.set_active(*(acc.reader)(self.current));

        let changer = self.create_changer(&acc);
        cb.connect_active_notify(move |cb| changer(cb.is_active()));

        self.add_widget(acc, cb);
    }    
}

impl<'a> config::ItemVisitor<config::Config, gdk::RGBA> for ConfigEditorBuilder<'a> {
    fn visit(&mut self, acc: config::Accessor<config::Config, gdk::RGBA>) {
        let cb: gtk::ColorButton = gtk::ColorButton::builder()
            .rgba((acc.reader)(self.current))
            .show_editor(true)
            .title(acc.description)
            .use_alpha(true)
            .build();

        let changer = self.create_changer(&acc);
        gtk::traits::ColorChooserExt::connect_rgba_notify(&cb, move |cb| changer(cb.rgba()));

        self.add_widget(acc, cb);
    }    
}

impl<'a> config::ItemVisitor<config::Config, pango::FontDescription> for ConfigEditorBuilder<'a> {
    fn visit(&mut self, acc: config::Accessor<config::Config, pango::FontDescription>) {
        let fb: gtk::FontButton = gtk::FontButton::builder()
            .font_desc((acc.reader)(self.current))
            .use_font(true)
            .build();

        let changer = self.create_changer(&acc);
        fb.connect_font_set(move |fb| if let Some(d) = fb.font_desc() { changer(d)});

        self.add_widget(acc, fb);
    }    
}

pub fn build_config_editor(config_host: sync::Arc<config::Host>) -> gtk::ListBox {
    let lb = gtk::ListBox::new();

    let current = config_host.borrow();

    let mut builder_visitor = ConfigEditorBuilder {
        host: config_host,
        current: &*current,
        lb,
    };

    config::Config::visit_all(&mut builder_visitor);
    
    builder_visitor.lb
}
