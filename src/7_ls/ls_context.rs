use crate::{
    builder::{self, File}, project::Project, resolver::{self, FileAnnotations}, types::*,
};

use super::ls::ProjectDependent;

pub struct LSContext<'a> {
    pub uri: &'a Url,
    pub position: &'a Position,

    pub ctx: resolver::Context<'a>,
    pub annotations: &'a FileAnnotations<'a>,

    pub nodes: Vec<builder::Node<'a>>,
    pub lowest_node: builder::Node<'a>,
}

impl<'a> LSContext<'a> {
    pub fn new(
        proj: &'a Project,
        dep: &'a ProjectDependent<'a>,
        uri: &'a Url,
        pos: &'a Position,
    ) -> Option<LSContext<'a>> {
        let Some(File::Built(file)) = proj.files.get(&uri) else {
            eprintln!("[WARN] File not found or not built");
            return None;
        };
        let Some(annotations) = dep.annotations.get(&uri) else {
            eprintln!("[WARN] Annotations not found");
            return None;
        };

        let ctx = resolver::Context::new(proj, file, &pos);

        let (top_level, nodes) = file.get_nodes_at(pos)?;
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

impl<'a> std::ops::Deref for LSContext<'a> {
    type Target = resolver::Context<'a>;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}
