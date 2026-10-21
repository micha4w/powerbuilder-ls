use tower_lsp::{jsonrpc, lsp_types};

use crate::{resolver::ResolvedType, solution, types::*};

use super::{ls::PowerBuilderLS, ls_context::LSContext};

impl PowerBuilderLS {
    pub(super) fn goto_type_capabilities(&self, caps: &mut lsp_types::ServerCapabilities) {
        caps.type_definition_provider =
            Some(lsp_types::TypeDefinitionProviderCapability::Simple(true));
    }

    pub(super) fn goto_type_impl(&self, ctx: &LSContext<'_>) -> jsonrpc::Result<Option<Range>> {
        let Some(lowest_node) = ctx.nodes.last() else {
            return Ok(None);
        };

        let range = ctx
            .annotations
            .datatype(lowest_node.get_range())
            .map(|dt| match dt.unnested() {
                ResolvedType::Class(class) => Some(class.class.parsed.class.name.range.clone()),
                _ => None,
            })
            .flatten();

        Ok(range)
    }
}
