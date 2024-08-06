use charm;

use std::cell;
use std::rc;

use gtk::gio;
use gtk::glib;
use gtk::prelude::*;

mod prelude;
mod screenshots;

pub enum Element {
    Window(i32, i32),
    Listing,
}

pub async fn screenshot<F: FnOnce(&charm::view::window::WindowContext)>(
    app: &rc::Rc<charm::view::CharmApplication>,
    project: charm::view::project::Project,
    setup: F,
    element: Element,
    path: impl AsRef<std::path::Path> + 'static) {
    let w = match element {
        Element::Window(_, _) => {
            app.new_window(false)
        },
        Element::Listing => {
            app.new_window(true)
        }
    };
    
    /* Present the window and wait for it to be mapped */
    let lw = {
        let (tx, rx) = tokio::sync::oneshot::channel();
        let tx = rc::Rc::new(cell::RefCell::new(Some(tx)));
        let shid = w.window.connect_map(move |_w| {
            tx.borrow_mut().take().unwrap().send(()).unwrap();
        });

        w.open_project(project, true, false);
        let lw = {
            let ctx_guard = w.context();
            let ctx = ctx_guard.as_ref().unwrap();
            setup(ctx);
            ctx.lw.clone()
        };

        match element {
            Element::Window(width, height) => {
                w.window.set_default_size(width, height);
            },
            Element::Listing => {
                let used_height = lw.used_height();
                w.window.set_default_size(lw.measure(gtk::Orientation::Horizontal, -1).1, used_height as i32);
            },
        }
        
        w.present();

        rx.await.unwrap();

        w.window.disconnect(shid);

        lw
    };

    /* Wait for all the visible data to load */
    lw.complete_work().await;

    let wp = gtk::WidgetPaintable::new(Some(&w.window));

    /* Wait for the listing widget to re-render */
    {
        let (tx, rx) = tokio::sync::oneshot::channel();
        let tx = rc::Rc::new(cell::RefCell::new(Some(tx)));
        let shid = wp.connect_invalidate_contents(move |_wp| {
            tx.borrow_mut().take().unwrap().send(()).unwrap();
        });

        // TODO: probably unnecessary
        //lw.animate(&lw.frame_clock().unwrap());
        
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

        glib::MainContext::default().spawn_local(async move {
            let _hold = hold;
            
            screenshots::structure::title_display(&app).await;
            screenshots::structure::content_display(&app).await;
        });
    });
}
