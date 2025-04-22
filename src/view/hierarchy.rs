use std::sync;

use gtk::prelude::*;
use gtk::subclass::prelude::*;

use crate::catch_panic;
use crate::model::document;

pub mod gtkmodel;

pub use gtkmodel::NodeItem;

/// Our TreeListModel doesn't update itself. This is so that we can guarantee
/// that the selection model updates first by having it be the one that
/// subscribes to SelectionHost updates and informs us when the document
/// updates. We still need a document_host though so we can change the document
/// when properties are edited.
pub fn create_tree_list_model(document_host: sync::Arc<document::DocumentHost>, document: sync::Arc<document::Document>, autoexpand: bool) -> gtk::TreeListModel {
    let root_model = gtkmodel::RootListModel::new(document_host, document);
    
    let model = gtk::TreeListModel::new(root_model, false, autoexpand, |obj| catch_panic! {
        @default(None);

        Some(obj.downcast_ref::<NodeItem>().unwrap().imp().expand().upcast())
    });
    
    model
}

#[cfg(test)]
mod tests {
    use super::*;
    use rusty_fork::rusty_fork_test;

    use crate::model::document::structure;
    
    fn assert_tlr_node_correct(document: &sync::Arc<document::Document>, node: &sync::Arc<structure::Node>, iter: &mut impl std::iter::Iterator<Item = gtk::TreeListRow>, path: &mut structure::Path) {
        let tlr = iter.next().expect("tree list model ended early");
        let item = tlr.item().unwrap().downcast::<NodeItem>().unwrap();
        let info = item.imp().info.get().unwrap().borrow();

        assert_eq!(&info.path, path);
        assert!(sync::Arc::ptr_eq(&info.document, document), "expected node info document to be up to date");
        assert!(sync::Arc::ptr_eq(&info.node, node), "expected to see '{}', but saw '{}' (path {:?})", node.props.name, info.props.name, info.path);
        assert_eq!(info.props, node.props, "(path {:?})", info.path);

        if tlr.is_expanded() {
            for (i, childhood) in node.children.iter().enumerate() {
                path.push(i);
                assert_tlr_node_correct(document, &childhood.node, iter, path);
                path.pop();
            }
        }
    }

    fn assert_tlm_correct(document: &sync::Arc<document::Document>, tlm: &gtk::TreeListModel) {
        let mut iter = tlm.iter().map(Result::unwrap);
        assert_tlr_node_correct(document, &document.root, &mut iter, &mut vec![]);

        if let Some(_) = iter.next() {
            panic!("tree list model had extra items");
        }
    }
    
    fn tree_list_node_items_iter(tlm: &gtk::TreeListModel) -> impl std::iter::Iterator<Item = NodeItem> + '_ {
        tlm.iter::<gtk::TreeListRow>().map(|tlr| tlr.unwrap().item().unwrap().downcast::<NodeItem>().unwrap())
    }

    /* GTK doesn't like being initialized in a process more than once. */
    rusty_fork_test! {
        #[test]
        fn test_reexpand() {
            gtk::init().unwrap();

            let root = structure::Node::builder()
                .name("root")
                .size(0x40)
                .child(0x0, |b| b
                       .name("container")
                       .size(0x40)
                       .child(0x0, |b| b
                              .name("child0")
                              .size(0x8))
                       .child(0x8, |b| b
                              .name("child1")
                              .size(0x8)))
                .build();
            
            let document_host = sync::Arc::new(document::Builder::new(root).host());
            let mut document = document_host.get();
            
            let tlm = create_tree_list_model(document_host.clone(), document.clone(), true);
            assert_tlm_correct(&document, &tlm);

            document = document_host.change(document.delete_range(structure::SiblingRange::new(vec![0], 0, 0))).unwrap();
            tlm.model().downcast::<gtkmodel::RootListModel>().unwrap().update_document(&document);
            assert_tlm_correct(&document, &tlm);

            tlm.item(1).unwrap().downcast::<gtk::TreeListRow>().unwrap().set_expanded(false);
            assert_tlm_correct(&document, &tlm);

            tlm.item(1).unwrap().downcast::<gtk::TreeListRow>().unwrap().set_expanded(true);
            assert_tlm_correct(&document, &tlm);
        }

        #[test]
        fn test_alter_node() {
            gtk::init().unwrap();

            let root = structure::Node::builder()
                .name("root")
                .size(0x40)
                .child(0x0, |b| b
                       .name("container")
                       .size(0x40)
                       .child(0x0, |b| b
                              .name("child0")
                              .size(0x8))
                       .child(0x8, |b| b
                              .name("child1")
                              .size(0x8)))
                .build();
            
            let document_host = sync::Arc::new(document::Builder::new(root).host());
            let mut document = document_host.get();
            
            let tlm = create_tree_list_model(document_host.clone(), document.clone(), true);
            assert_tlm_correct(&document, &tlm);

            document = document_host.change(document.alter_node(vec![0], structure::Properties {
                name: "foo".to_string(),
                title_display: structure::TitleDisplay::Inline,
                children_display: structure::ChildrenDisplay::None,
                content_display: structure::ContentDisplay::None,
                locked: true,
            })).unwrap();
            tlm.model().downcast::<gtkmodel::RootListModel>().unwrap().update_document(&document);
            assert_tlm_correct(&document, &tlm);
        }
    }
}
