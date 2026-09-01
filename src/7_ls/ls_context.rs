use std::sync::Arc;

use tracing::warn;

use crate::{
    builder::{self},
    resolver::{self, FileAnnotations},
    solution::Solution,
    types::*,
};

use super::ls::SolutionDependent;

pub struct LSContext<'sol> {
    pub uri: Arc<Url>,
    pub position: &'sol Position,

    pub ctx: resolver::Context<'sol>,
    pub annotations: &'sol FileAnnotations<'sol>,

    pub nodes: Vec<builder::Node<'sol>>,
    pub lowest_node: builder::Node<'sol>,
}

impl<'sol> LSContext<'sol> {
    pub fn new(
        sol: &'sol Solution,
        dep: &'sol SolutionDependent<'sol>,
        uri: &Url,
        pos: &'sol Position,
    ) -> Option<LSContext<'sol>> {
        // TODO(perf): use the arc pointer to index maps instead of hashing the url
        let uri = Arc::new(uri.clone());

        let Some(file) = sol.files.get(&uri) else {
            // TODO(errors): return error here
            warn!(%uri, "file not found");
            return None;
        };
        let Some(annotations) = dep.annotations.get(&uri) else {
            // TODO(errors): return error here
            warn!(%uri, "annotations not found");
            return None;
        };

        let ctx = resolver::Context::new(sol, file, &pos);

        let (top_level, nodes) = file.inner().get_nodes_at(pos)?;
        let lowest_node = nodes.last()?.clone();

        Some(LSContext {
            uri,
            position: pos,

            ctx,
            annotations,

            nodes,
            lowest_node,
        })
    }
}

impl<'sol> std::ops::Deref for LSContext<'sol> {
    type Target = resolver::Context<'sol>;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}
