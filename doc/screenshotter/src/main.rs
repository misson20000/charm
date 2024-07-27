use charm;

use std::cell;
use std::rc;
use std::sync;

use gtk::gio;
use gtk::glib;
use gtk::prelude::*;

async fn screenshot<F: FnOnce(&charm::view::window::WindowContext)>(
    app: &rc::Rc<charm::view::CharmApplication>,
    project: charm::view::project::Project,
    setup: F,
    path: impl AsRef<std::path::Path> + 'static,
    width: i32,
    height: i32) {
    let w = app.new_window();
        
    w.window.set_default_size(width, height);

    /* Present the window and wait for it to be mapped */
    let lw = {
        let (tx, rx) = tokio::sync::oneshot::channel();
        let tx = rc::Rc::new(cell::RefCell::new(Some(tx)));
        let shid = w.window.connect_map(move |_w| {
            tx.borrow_mut().take().unwrap().send(()).unwrap();
        });

        /* Opening a project presents the window */
        w.open_project(project, true);    
        let lw = {
            let ctx_guard = w.context();
            let ctx = ctx_guard.as_ref().unwrap();
            setup(ctx);
            ctx.lw.clone()
        };
        
        w.present();

        rx.await.unwrap();

        w.window.disconnect(shid);

        lw
    };

    println!("shown");
    
    /* Wait for all the visible data to load */
    lw.complete_work().await;

    let wp = gtk::WidgetPaintable::new(Some(&w.window));

    /* Wait for the listing widget to re-render */
    {
        let (tx, rx) = tokio::sync::oneshot::channel();
        let tx = rc::Rc::new(cell::RefCell::new(Some(tx)));
        let shid = wp.connect_invalidate_contents(move |_wp| {
            println!("contents invalidated");
            tx.borrow_mut().take().unwrap().send(()).unwrap();
        });

        println!("reanimating"); // TODO: probably unnecessary
        lw.animate(&lw.frame_clock().unwrap());
        
        println!("redrawing");
        lw.queue_draw();

        rx.await.unwrap();

        wp.disconnect(shid);
    }
        
    /* Take the screenshot */
    let snapshot = gtk::Snapshot::new();
    wp.snapshot(&snapshot, wp.intrinsic_width() as f64, wp.intrinsic_height() as f64);
    let rn = snapshot.to_node().expect("Nothing was rendered");
    let renderer = w.window.renderer().expect("Window did not have a renderer");
    let texture = renderer.render_texture(&rn, None);
    texture.save_to_png(path).expect("Failed to save texture to PNG");
    w.close_window();
    w.window.destroy();
}

fn main() {    
    charm::view::CharmApplication::launch(gio::ApplicationFlags::NON_UNIQUE, move |app| {
        let app = app.clone();
        let hold = app.application.hold();

        /* Disable cursor blinking */
        charm::view::config::INSTANCE.change(charm::view::config::Change::cursor_blink_period(0.0)).unwrap();

        /* Just so we have some data to show, open /proc/self/exe */
        let space = charm::model::space::file::FileAddressSpace::new("/proc/self/exe".into(), "/proc/self/exe");
        space.try_open().expect("Failed to open /proc/self/exe");
        let space = sync::Arc::new(space.into());
        
        let project = charm::view::project::Project::new_unsaved(charm::model::document::Builder::new(
            charm::model::document::structure::Node::builder()
                .name("root")
                .size(0x4000)
                .build()
        ).load_space(space).build());

        glib::MainContext::default().spawn_local(async move {
            screenshot(&app, project, |ctx| {
                ctx.listing_selection_host.change(
                    charm::model::selection::listing::Change::AssignStructure(
                        charm::model::selection::listing::StructureMode::Range(
                            charm::model::selection::listing::StructureRange {
                                path: vec![],
                                begin: (0x4.into(), 0),
                                end: (0x18.into(), 0),
                            }))).unwrap();

                ctx.lw.goto(&*ctx.project.document_host.borrow(), &vec![], 0x800.into(), charm::model::listing::cursor::PlacementHint::default());
            }, "test.png", 1600, 800).await;

            std::mem::drop(hold);
        });
    });
}
