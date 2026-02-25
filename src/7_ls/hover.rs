use tower_lsp::{jsonrpc, lsp_types};

use crate::{
    builder, parser, project,
    resolver::{ResolvedLValue, ResolvedType},
    tokenizer,
    types::*,
};

use super::{ls::PowerBuilderLS, ls_context::LSContext};

impl PowerBuilderLS {
    pub(super) fn hover_capabilities(&self, caps: &mut lsp_types::ServerCapabilities) {
        caps.hover_provider = Some(lsp_types::HoverProviderCapability::Simple(true));
    }

    pub(super) fn hover_impl(
        &self,
        ctx: &LSContext<'_>,
    ) -> jsonrpc::Result<Option<(String, Range)>> {
        let mut title = None;
        let mut description = None;
        let mut range = ctx.lowest_node.get_range();

        match ctx.lowest_node {
            builder::Node::VariableDeclaration(var) => {
                title = Some(var.to_string());
                description = var.help.clone();
            }
            builder::Node::InstanceVariableDeclaration(var) => {
                if let Some(fc) = ctx.class {
                    title = Some(format!("{}\n{}", fc.class, var.variable));
                } else {
                    title = Some(var.to_string());
                }
                description = var.variable.help.clone();
            }
            builder::Node::ScopedVariableDeclaration(var) => {
                if let Some(fc) = ctx.class {
                    title = Some(format!("{}\n{}", fc.class, var.variable));
                } else {
                    title = Some(var.to_string());
                }
                description = var.variable.help.clone();
            }
            builder::Node::Label(token) => {
                title = Some(token.content.clone() + ":");
            }
            builder::Node::FunctionDeclaration(func) => {
                title = Some(func.to_string());
                description = func.help.clone();
            }
            builder::Node::EventDeclaration(event) => {
                title = Some(event.to_string());
                description = event.help.clone();
            }
            builder::Node::LValue(lvalue) => {
                if let Found::Yes(resolved) = ctx.annotations.lvalue(lvalue) {
                    match resolved {
                        ResolvedLValue::This(fc) => {
                            title = Some(fc.class.name().clone() + " this = <reserved>");
                        }
                        ResolvedLValue::Super(complex) | ResolvedLValue::Parent(complex) => {
                            let class_name = match complex {
                                project::Complex::Class(fc) => fc.class.name(),
                                _ => "<error>",
                            };
                            let var_name = match resolved {
                                ResolvedLValue::Super(_) => "super",
                                ResolvedLValue::Parent(_) => "parent",
                                _ => unreachable!(),
                            };
                            title = Some(format!("{class_name} {var_name} = <reserved>"));
                        }
                        ResolvedLValue::Variable(fc, var) => {
                            title = Some(
                                fc.map_or_default(|fc| format!("{}\n", fc.class))
                                    + &var.parsed().to_string(),
                            );
                            description = var.parsed().help.clone();
                        }
                        ResolvedLValue::Function(fc, func) => {
                            title = Some(
                                fc.map_or_default(|fc| format!("{}\n", fc.class))
                                    + &func.header().parsed.to_string(),
                            );
                            description = func.help().cloned();
                        }
                        ResolvedLValue::Member(fc, var) => {
                            title = Some(format!("{}\n{}", fc.class, var.parsed()));
                            description = var.parsed().help.clone();
                        }
                        ResolvedLValue::Method(fc, callable) => match callable {
                            OneOf::Left(func) => {
                                title = Some(format!("{}\n{}", fc.class, func.header().parsed));
                                description = func.help().cloned();
                            }
                            OneOf::Right(event) => {
                                title = Some(format!("{}\n{}", fc.class, event.header().parsed));
                                description = event.help().cloned();
                            }
                        },
                    }
                }
            }
            builder::Node::Expression(expr) => title = Some(expr.expression_type.to_string()),
            builder::Node::Statement(_) => {}
            builder::Node::DataType(dt) => {
                if let Some(resolved) = ctx.annotations.datatype(&dt.range) {
                    let (type_title, type_description) = Self::hover_type(resolved);
                    title = Some(type_title);
                    description = type_description;
                };
            }
        }

        let Some(title) = title else { return Ok(None) };

        let mut content = format!("```powerbuilder\n{}\n```", title);
        if let Some(desc) = description {
            content += "\n---\n";
            content += &desc;
        }

        Ok(Some((content, range.clone())))
    }

    fn hover_type(resolved: &ResolvedType<'_>) -> (String, Option<String>) {
        let title;
        let mut description = None;
        match resolved {
            ResolvedType::Complex(project::Complex::Class(fc)) => {
                title = fc.class.to_string();
                description = fc.class.help.clone();
            }
            ResolvedType::Complex(project::Complex::Enum(en)) => {
                title = format!("type {} from enumerated", en.name);
                description = en.help.clone();
            }
            ResolvedType::Base(base_type) => {
                title = format!("type {} from primitives", base_type);
            }
            ResolvedType::Array(inner) => {
                let (inner_title, inner_description) = Self::hover_type(&inner);
                title = format!("({})[]", inner_title);
                description = inner_description;
            }
            ResolvedType::Void => unreachable!(),
            ResolvedType::Unknown => unreachable!(),
        };
        (title, description)
    }
}
