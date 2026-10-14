use tracing::{info, warn};

use crate::{
    builder::{self},
    resolver::{self, FileAnnotations},
    solution::Solution,
    types::*,
};

use super::ls::SolutionDependent;

pub struct LSContext<'sol> {
    pub file_id: FileID,
    pub position: &'sol Position,

    pub ctx: resolver::Context<'sol>,
    pub annotations: &'sol FileAnnotations<'sol>,

    pub top_level: &'sol builder::TopLevel<'sol>,
    pub nodes: Vec<builder::Node<'sol>>,
}

impl<'sol> LSContext<'sol> {
    pub fn new(
        sol: &'sol Solution,
        dep: &'sol SolutionDependent<'sol>,
        uri: Url,
        pos: &'sol Position,
    ) -> Option<LSContext<'sol>> {
        // TODO(perf): use the arc pointer to index maps instead of hashing the path
        let file_id = FileID::new(uri)?;

        let Some(file) = sol.files.get(&file_id) else {
            // this happens when the user opens a file that is not part of the project
            info!(%file_id, "file not found");
            return None;
        };
        let Some(annotations) = dep.annotations.get(&file_id) else {
            // TODO(errors): return error here
            warn!(%file_id, "annotations not found");
            return None;
        };

        let ctx = resolver::Context::new(sol, file, &pos);

        let (top_level, nodes) = file.inner().get_nodes_at(pos)?;

        Some(LSContext {
            file_id,
            position: pos,

            ctx,
            annotations,

            top_level,
            nodes,
        })
    }
}

impl<'sol> std::ops::Deref for LSContext<'sol> {
    type Target = resolver::Context<'sol>;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}
