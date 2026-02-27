//! This is mostly useful for testcases */

use super::*;

#[derive(Default)]
pub struct StructureBuilder {
    node: Node
}

impl StructureBuilder {
    pub fn props(mut self, props: Properties) -> Self {
        self.node.props = props;
        self
    }

    pub fn name<S: ToString>(mut self, name: S) -> Self {
        self.node.props.name = name.to_string();
        self
    }

    pub fn title_display(mut self, value: props::TitleDisplay) -> Self {
        self.node.props.title_display = value;
        self
    }

    /// Convenience alias for setting title display to inline
    pub fn inline(mut self) -> Self {
        self.node.props.title_display = props::TitleDisplay::Inline;
        self
    }
    
    pub fn children_display(mut self, value: props::ChildrenDisplay) -> Self {
        self.node.props.children_display = value;
        self
    }

    pub fn content_display(mut self, value: props::ContentDisplay) -> Self {
        self.node.props.content_display = value;
        self
    }

    pub fn lock(mut self) -> Self {
        self.node.props.locked = true;
        self
    }

    pub fn unlock(mut self) -> Self {
        self.node.props.locked = false;
        self
    }

    pub fn size<S: Into<addr::Offset>>(mut self, size: S) -> Self {
        self.node.size = size.into();
        self
    }

    pub fn child<A: Into<addr::Offset>, F: FnOnce(StructureBuilder) -> StructureBuilder>(mut self, offset: A, builder: F) -> Self {
        self.node.children.push(Childhood {
            offset: offset.into(),
            node: builder(Self::default()).build()
        });
        self
    }

    pub fn build(&self) -> sync::Arc<Node> {
        self.node.assert_validity();
        sync::Arc::new(self.node.clone())
    }

    pub fn build_child<T: Into<addr::Offset>>(&self, offset: T) -> Childhood {
        self.node.assert_validity();
        Childhood::new(sync::Arc::new(self.node.clone()), offset.into())
    }
}
