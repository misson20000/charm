use std::marker::PhantomData;
use std::sync;

use crate::model::document::change;
use crate::model::document::structure;
use crate::model::selection;

pub struct RebuiltTree<T> {
    pub root: sync::Arc<structure::Node>,
    pub target: sync::Arc<structure::Node>,
    pub output: T,
}

pub trait TargetVisitor {
    type Output;
    type Error;
    type AncestorVisitor: AncestorVisitor<Output = Self::Output, Error = Self::Error>;
    
    fn visit_target(self, target: &mut structure::Node) -> Result<Self::AncestorVisitor, Self::Error>;
}
                                                                  
pub trait AncestorVisitor {
    type Output;
    type Error;

    /// Ancestry tells you how many generations are between this ancestor and
    /// the target node. The target node would have an ancestry of 0 if it was
    /// ever processed through this function. The direct parent of the target
    /// node has an ancestry of 1. By the time this function is called,
    /// ancestor.children[child_index] has already been replaced with a new
    /// childhood for the already-rebuilt part of the node tree.
    fn visit_ancestor(&mut self, ancestor: &mut structure::Node, ancestry: usize, old_childhood: structure::Childhood, child_index: usize) -> Result<(), Self::Error>;
    
    fn finalize(self) -> Self::Output;
}

pub struct NoopAncestorVisitor<T, E>(T, PhantomData<E>);

impl<T, E, F: FnOnce(&mut structure::Node) -> Result<T, E>> TargetVisitor for F {
    type Output = T;
    type Error = E;
    type AncestorVisitor = NoopAncestorVisitor<T, E>;

    fn visit_target(self, target: &mut structure::Node) -> Result<Self::AncestorVisitor, Self::Error> {
        Ok(NoopAncestorVisitor(self(target)?, PhantomData))
    }
}

impl<T, E> AncestorVisitor for NoopAncestorVisitor<T, E> {
    type Output = T;
    type Error = E;
    
    fn visit_ancestor(&mut self, _ancestor: &mut structure::Node, _ancestry: usize, _old_childhood: structure::Childhood, _child_index: usize) -> Result<(), E> {
        Ok(())
    }

    fn finalize(self) -> T {
        self.0
    }
}

fn rebuild_partial_node_tree_visiting_path<Visitor, Iter: std::iter::Iterator<Item = usize>>
    (target: &structure::Node,
     mut path_segment: Iter,
     visitor: Visitor) -> Result<(RebuiltTree<Visitor::AncestorVisitor>, usize), Visitor::Error> where
    Visitor: TargetVisitor {
    match path_segment.next() {
        Some(index) => {
            /* Recurse to rebuild the child, then rebuild the target with the new child. */
            let child = &target.children[index];
            let (mut new_result, mut ancestry) = rebuild_partial_node_tree_visiting_path(&child.node, path_segment, visitor)?;
            ancestry+= 1;
            let mut new_target = (*target).clone();
            let old_childhood = std::mem::replace(&mut new_target.children[index], structure::Childhood {
                offset: child.offset,
                node: new_result.root
            });
            new_result.output.visit_ancestor(&mut new_target, ancestry, old_childhood, index)?;
            new_target.assert_validity();
            Ok((RebuiltTree {
                root: sync::Arc::new(new_target),
                target: new_result.target,
                output: new_result.output,
            }, ancestry))
        },
        None => {
            /* Reached the end of the path. Just modify this node directly. */
            let mut new_target = (*target).clone();
            let ancestor_visitor = visitor.visit_target(&mut new_target)?;
            new_target.assert_validity();
            let new_target = sync::Arc::new(new_target);
            Ok((RebuiltTree {
                root: new_target.clone(),
                target: new_target,
                output: ancestor_visitor,
            }, 0))
        }
    }
}
                                                                  
pub fn rebuild_node_tree_visiting_path<Visitor>
    (root: &structure::Node,
     path: structure::PathSlice,
     visitor: Visitor) -> Result<RebuiltTree<Visitor::Output>, Visitor::Error> where
    Visitor: TargetVisitor {
    let result = rebuild_partial_node_tree_visiting_path(root, path.iter().copied(), visitor)?.0;

    Ok(RebuiltTree {
        root: result.root,
        target: result.target,
        output: result.output.finalize(),
    })
}

/// This exists to help type inference if you're trying to use a closure, especially when you don't have a failure path and it can't figure out the error type.
pub fn rebuild_node_tree_visiting_path_simple<T, Visitor>
    (root: &structure::Node,
     path: structure::PathSlice,
     visitor: Visitor) -> Result<RebuiltTree<T>, change::ApplyErrorType> where
    Visitor: FnOnce(&mut structure::Node) -> Result<T, change::ApplyErrorType> {
    rebuild_node_tree_visiting_path(root, path, visitor)
}
    
struct TreeRewritingVisitor<F: Fn(&mut structure::Node) -> Result<(), E>, E>(F);

impl<'a, F: Fn(&mut structure::Node) -> Result<(), E>, E: 'a> selection::tree::TreeVisitor<'a> for TreeRewritingVisitor<F, E> {
    type NodeContext = structure::Node;
    type VisitResult = Result<(), E>;

    fn root_context(&mut self, root: &'a sync::Arc<structure::Node>) -> Self::NodeContext {
        (**root).clone()
    }

    fn descend<'b>(&mut self, _parent: &'b mut Self::NodeContext, child: &'a sync::Arc<structure::Node>, _child_index: usize, _child_selected: bool) -> Option<Self::NodeContext> {
        Some((**child).clone())
    }

    fn visit_child<'b>(&mut self, parent_context: &'b mut Self::NodeContext, child: &'a sync::Arc<structure::Node>, child_index: usize) -> Self::VisitResult {
        let mut new_child = (**child).clone();
        self.0(&mut new_child)?;
        new_child.assert_validity();
        parent_context.children[child_index].node = sync::Arc::new(new_child);
        Ok(())
    }

    fn visit<'b>(&mut self, context: &'b mut Self::NodeContext, _node: &'a sync::Arc<structure::Node>) -> Self::VisitResult {
        self.0(context)
    }

    fn ascend<'b>(&mut self, parent: &'b mut Self::NodeContext, child: Self::NodeContext, child_index: usize) {
        parent.children[child_index].node = sync::Arc::new(child);
    }
}

pub fn rebuild_node_tree_visiting_tree<F, E>(tree: &selection::TreeSelection, target_visitor: F) -> Result<sync::Arc<structure::Node>, E> where
    F: Fn(&mut structure::Node) -> Result<(), E> {
    let mut walker = tree.walker(TreeRewritingVisitor(target_visitor));

    while let Some(r) = walker.visit_next() {
        r?;
    }

    Ok(sync::Arc::new(walker.take()))
}

