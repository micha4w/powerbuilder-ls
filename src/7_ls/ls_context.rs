use crate::{
    builder::{self, File},
    project::Project,
    resolver::{self, FileAnnotations},
    types::*,
};

use super::ls::ProjectDependent;

pub struct LSContext<'proj> {
    pub uri: &'proj Url,
    pub position: &'proj Position,

    pub ctx: resolver::Context<'proj>,
    pub annotations: &'proj FileAnnotations<'proj>,

    pub nodes: Vec<builder::Node<'proj>>,
    pub lowest_node: builder::Node<'proj>,
}

impl<'proj> LSContext<'proj> {
    pub fn new(
        proj: &'proj Project,
        dep: &'proj ProjectDependent<'proj>,
        uri: &'proj Url,
        pos: &'proj Position,
    ) -> Option<LSContext<'proj>> {
        let Some(File::Built(file)) = proj.files.get(&uri) else {
            eprintln!("[WARN] File not found or not built");
            return None;
        };
        let Some(annotations) = dep.annotations.get(&uri) else {
            eprintln!("[WARN] Annotations not found");
            return None;
        };

        let ctx = resolver::Context::new(proj, file, &pos);

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

impl<'proj> std::ops::Deref for LSContext<'proj> {
    type Target = resolver::Context<'proj>;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}
