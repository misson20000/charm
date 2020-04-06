#![allow(dead_code)]

extern crate cairo;
extern crate gio;
extern crate gtk;
extern crate gdk;
extern crate futures;

#[cfg(feature = "test_listing")]
extern crate ncurses;

mod addr;
mod listing;
mod space;
mod widget;

use gio::prelude::*;
use gtk::prelude::*;

fn draw_fn(_a: &gtk::DrawingArea, cr: &cairo::Context) -> gtk::prelude::Inhibit {
    cr.select_font_face("Sans", cairo::FontSlant::Normal, cairo::FontWeight::Normal);
    cr.set_font_size(24.0);
    cr.move_to(20.0, 20.0);
    cr.show_text("DA Test");

    Inhibit(false)
}

fn build_ui(application: &gtk::Application, aspace: std::sync::Arc<dyn space::AddressSpace>) {
    let window = gtk::ApplicationWindow::new(application);

    window.set_title("Charm");
    window.set_border_width(10);
    window.set_position(gtk::WindowPosition::Center);
    window.set_default_size(800, 600);

    let pane = gtk::Paned::new(gtk::Orientation::Horizontal);
    let tree = gtk::TreeView::new();
    let da = gtk::DrawingArea::new();

    widget::listing::ListingWidget::new(aspace)
        .attach(&da);

    pane.add(&da);
    pane.add(&tree);

    window.add(&pane);

    window.show_all();
}

fn main() {
    println!("Hello, world!");

    let application =
        gtk::Application::new(Some("net.xenotoad.charm"), Default::default())
        .expect("Initialization failed..");

    let fas = std::sync::Arc::new(space::file::FileAddressSpace::open("/proc/self/exe").unwrap());
    
    application.connect_activate(move |app| {
        build_ui(app, fas.clone());
    });

    application.run(&std::env::args().collect::<Vec<_>>());
}
