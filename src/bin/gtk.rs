#![allow(dead_code)]

use gtk::gio;

fn setup_tracing() {
    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .pretty()
        .with_max_level(tracing::Level::TRACE)
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");
}

fn main() {
    setup_tracing();
    
    charm::view::CharmApplication::launch(gio::ApplicationFlags::HANDLES_OPEN, |app| {
        app.action_new_window();
    });
}
