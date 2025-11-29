use crate::prelude::*;

pub async fn shadowing(app: &rc::Rc<CharmApplication>) {
    let mut data = vec![0; 1024];
    data[50..54].copy_from_slice(&[1, 2, 3, 4]);
    data[100..104].copy_from_slice(&[1, 2, 3, 4]);
    let space = space::memory::InMemoryAddressSpace::new(data, "Example File");
    let space: sync::Arc<space::AddressSpace> = sync::Arc::new(space.into());
    
    let project = project::Project::new_unsaved(document::Builder::new(
        structure::Node::builder()
            .name("Example File")
            .content_display(structure::ContentDisplay::default())
            .size(128)
            .build()
    ).load_space(space.clone(), None).build());
    
    crate::screenshot(app, project, |ctx| {
        ctx.lw.set_cursor_hidden(true);
    }, crate::Element::Listing, "shadowing-before.png").await;
    
    let project = project::Project::new_unsaved(document::Builder::new(
        structure::Node::builder()
            .name("Example File")
            .content_display(structure::ContentDisplay::default())
            .size(128)
            .child(50, |b| b
                   .name("Interesting Field 1")
                   .content_display(structure::ContentDisplay::Hexdump {
                       line_pitch: 16.into(),
                       gutter_pitch: 8.into(),
                   })
                   .size(0x4))
            .child(100, |b| b
                   .name("Interesting Field 2")
                   .content_display(structure::ContentDisplay::Hexdump {
                       line_pitch: 16.into(),
                       gutter_pitch: 8.into(),
                   })
                   .size(0x4))
            .build()
    ).load_space(space, None).build());
    
    crate::screenshot(app, project.clone(), |ctx| {
        ctx.lw.set_cursor_hidden(true);
    }, crate::Element::Listing, "shadowing.png").await;

    crate::screenshot(app, project, |ctx| {
        let document = ctx.project.document_host.get();
        let sel = ctx.tree_selection_host.change(selection::tree::Change::SetSingle(document, vec![0])).unwrap();
        /* Synchronously force update */
        ctx.tree_selection_model.update(sel);
    }, crate::Element::PropsEditor, "props-editor.png").await;
}

pub async fn title_display(app: &rc::Rc<CharmApplication>) {
    /* Just so we have some data to show, open /proc/self/exe */
    let space = space::file::FileAddressSpace::new("/proc/self/exe".into(), "/proc/self/exe");
    space.try_open().expect("Failed to open /proc/self/exe");
    let space = sync::Arc::new(space.into());
    
    let project = project::Project::new_unsaved(document::Builder::new(
        structure::Node::builder()
            .name("root")
            .size(0x40)
            .child(0x0, |b| b
                   .name("inline")
                   .title_display(structure::TitleDisplay::Inline)
                   .size(0x8))
            .child(0x0, |b| b
                   .name("minor")
                   .title_display(structure::TitleDisplay::Minor)
                   .size(0x30))
            .child(0x0, |b| b
                   .name("major")
                   .title_display(structure::TitleDisplay::Major)
                   .size(0x30))
            .build()
    ).load_space(space, None).build());
    
    crate::screenshot(app, project, |ctx| {
        ctx.lw.set_cursor_hidden(true);
    }, crate::Element::Listing, "structure_title_display.png").await;
}

pub async fn content_display(app: &rc::Rc<CharmApplication>) {
    /* Just so we have some data to show, open /proc/self/exe */
    let space = space::file::FileAddressSpace::new("/proc/self/exe".into(), "/proc/self/exe");
    space.try_open().expect("Failed to open /proc/self/exe");
    let space = sync::Arc::new(space.into());
    
    let project = project::Project::new_unsaved(document::Builder::new(
        structure::Node::builder()
            .name("hidden")
            .content_display(structure::ContentDisplay::None)
            .size(0x40)
            .child(0x10, |b| b
                   .name("hexdump")
                   .content_display(structure::ContentDisplay::Hexdump {
                       line_pitch: 16.into(),
                       gutter_pitch: 8.into(),
                   })
                   .size(0x20))
            .child(0x30, |b| b
                   .name("hexstring")
                   .content_display(structure::ContentDisplay::Hexstring)
                   .size(0x10))
            .build()
    ).load_space(space, None).build());
    
    crate::screenshot(app, project, |ctx| {
        ctx.lw.set_cursor_hidden(true);
    }, crate::Element::Listing, "structure_content_display.png").await;
}
