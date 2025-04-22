use std::sync;

use crate::model::addr;
use crate::model::document;
use crate::model::document::structure;

pub mod node_item;
pub mod structure_list_model;
pub mod root_list_model;

pub use node_item::NodeItem;
pub use structure_list_model::StructureListModel;
pub use root_list_model::RootListModel;

#[derive(Debug, Clone)]
pub struct NodeInfo {
    pub node: sync::Arc<structure::Node>,
    
    /* when we update a gobject property, it needs to be reflected immediately,
     * before document update happens. */
    pub props: structure::Properties,
    
    pub path: structure::Path,
    pub offset: addr::Offset,
    pub address: addr::AbsoluteAddress,
    pub document_host: sync::Arc<document::DocumentHost>,
    pub document: sync::Arc<document::Document>,
}
