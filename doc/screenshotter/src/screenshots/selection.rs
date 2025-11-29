use crate::prelude::*;

pub async fn address(app: &rc::Rc<CharmApplication>) {
    let mut data = vec![0; 1024];
    data[50..58].copy_from_slice(&[1, 2, 3, 4, 5, 6, 7, 8]);
    let space = space::memory::InMemoryAddressSpace::new(data, "Example File");
    let space: sync::Arc<space::AddressSpace> = sync::Arc::new(space.into());
    
    let project = project::Project::new_unsaved(document::Builder::new(
        structure::Node::builder()
            .name("Example File")
            .content_display(structure::ContentDisplay::default())
            .size(128)
            .child(54, |b| b
                   .name("Child Node")
                   .size(0x4))
            .build()
    ).load_space(space.clone(), None).build());
    
    crate::screenshot(app, project, |ctx| {
        let selection = ctx.listing_selection_host.change(selection::listing::Change::AssignAddress(addr::AbsoluteExtent::sized(52, 0x4))).unwrap();
        ctx.lw.selection_updated(&selection);
        ctx.lw.set_cursor_hidden(true);
    }, crate::Element::Listing, "selection-address.png").await;
}

pub async fn address_overlap(app: &rc::Rc<CharmApplication>) {
    let mut data = vec![0; 1024];
    data[0x44..0x4c].copy_from_slice(&[1, 2, 3, 4, 5, 6, 7, 8]);
    let space = space::memory::InMemoryAddressSpace::new(data, "Example File");
    let space: sync::Arc<space::AddressSpace> = sync::Arc::new(space.into());
    
    let project = project::Project::new_unsaved(document::Builder::new(
        structure::Node::builder()
            .name("Example File")
            .content_display(structure::ContentDisplay::default())
            .size(128)
            .child(0x30, |b| b
                   .name("Overlapping Node 1")
                   .size(0x30))
            .child(0x40, |b| b
                   .name("Overlapping Node 2")
                   .size(0x10))
            .build()
    ).load_space(space.clone(), None).build());
    
    crate::screenshot(app, project, |ctx| {
        let selection = ctx.listing_selection_host.change(selection::listing::Change::AssignAddress(addr::AbsoluteExtent::sized(0x44, 0x8))).unwrap();
        ctx.lw.selection_updated(&selection);
        ctx.lw.set_cursor_hidden(true);
    }, crate::Element::Listing, "selection-address-overlap.png").await;
}

pub async fn structure(app: &rc::Rc<CharmApplication>) {
    let mut data = vec![0; 1024];
    data[0x44..0x4c].copy_from_slice(&[1, 2, 3, 4, 5, 6, 7, 8]);
    let space = space::memory::InMemoryAddressSpace::new(data, "Example File");
    let space: sync::Arc<space::AddressSpace> = sync::Arc::new(space.into());
    
    let project = project::Project::new_unsaved(document::Builder::new(
        structure::Node::builder()
            .name("Example File")
            .content_display(structure::ContentDisplay::default())
            .size(128)
            .child(0x30, |b| b
                   .name("Overlapping Node 1")
                   .size(0x30)
                   .child(0x10, |b| b
                          .name("Child 1")
                          .inline()
                          .size(0x4))
                   .child(0x1c, |b| b
                          .name("Child 2")
                          .inline()
                          .size(0x4)))
            .child(0x40, |b| b
                   .name("Overlapping Node 2")
                   .size(0x10))
            .build()
    ).load_space(space.clone(), None).build());
    
    crate::screenshot(app, project, |ctx| {
        let selection = ctx.listing_selection_host.change(selection::listing::Change::AssignStructure(
            selection::listing::StructureMode::Range(selection::listing::StructureRange {
                path: vec![0],
                begin: (0xe.into(), 0),
                end: (0x22.into(), 2),
            })
        )).unwrap();
        ctx.lw.selection_updated(&selection);
        ctx.lw.set_cursor_hidden(true);
    }, crate::Element::Listing, "selection-structure.png").await;
}
