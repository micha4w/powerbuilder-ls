use tower_lsp::{jsonrpc, lsp_types};

use crate::{
    builder, project,
    resolver::{ResolvedLValue, ResolvedType},
    types::*,
};

use super::{ls::PowerBuilderLS, ls_context::LSContext};

impl PowerBuilderLS {
    pub(super) fn goto_definition_capabilities(&self, caps: &mut lsp_types::ServerCapabilities) {
        caps.definition_provider = Some(lsp_types::OneOf::Left(true));
    }

    pub(super) fn goto_definition_impl(
        &self,
        ctx: &LSContext<'_>,
    ) -> jsonrpc::Result<Option<Range>> {
        let mut range = None;

        match &ctx.lowest_node {
            builder::Node::VariableDeclaration(var) => {
                range = Some(var.access.name.range.clone());
            }
            builder::Node::InstanceVariableDeclaration(var) => {
                range = Some(var.variable.access.name.range.clone());
            }
            builder::Node::ScopedVariableDeclaration(var) => {
                range = Some(var.variable.access.name.range.clone());
            }
            builder::Node::Label(token) => {
                if let Some(body) = &ctx.body {
                    if let Some(label) = body.labels.get(&(&token.content).into()) {
                        range = Some(label.range.clone());
                    }
                }
            }
            builder::Node::FunctionDeclaration(func) => range = Some(func.name.range.clone()),
            builder::Node::EventDeclaration(event) => range = Some(event.name.range.clone()),
            builder::Node::LValue(lvalue) => {
                if let Found::Yes(resolved) = ctx.annotations.lvalue(lvalue) {
                    match resolved {
                        ResolvedLValue::This(fc) => {
                            range = Some(fc.class.parsed.class.name.range.clone());
                        }
                        ResolvedLValue::Super(complex) | ResolvedLValue::Parent(complex) => {
                            if let project::Complex::Class(fc) = complex {
                                range = Some(fc.class.parsed.class.name.range.clone());
                            }
                        }
                        ResolvedLValue::Variable(_, var) => {
                            range = Some(var.parsed().access.name.range.clone());
                        }
                        ResolvedLValue::Function(_, func) => {
                            range = Some(func.header().parsed.name.range.clone());
                        }
                        ResolvedLValue::Member(_, var) => {
                            range = Some(var.parsed().access.name.range.clone());
                        }
                        ResolvedLValue::Method(_, callable) => match callable {
                            OneOf::Left(func) => {
                                range = Some(func.header().parsed.name.range.clone());
                            }
                            OneOf::Right(event) => {
                                range = Some(event.header().parsed.name.range.clone());
                            }
                        },
                    }
                }
            }
            builder::Node::Expression(_expr) => {}
            builder::Node::Statement(_) => {}
            builder::Node::DataType(dt) => {
                if let Some(resolved) = ctx.annotations.datatype(&dt.range) {
                    if let ResolvedType::Complex(project::Complex::Class(fc)) = resolved.unnested()
                    {
                        range = Some(fc.class.parsed.class.name.range.clone());
                    };
                };
            }
        }

        Ok(range)
    }
}
