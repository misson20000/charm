use crate::prelude::*;

pub async fn demo(app: &rc::Rc<CharmApplication>) {
    crate::screenshot(app, crate::load_project("projects/demo.charm"), |_ctx| {
    }, crate::Element::Window(800, 800), "demo.png").await;
}
