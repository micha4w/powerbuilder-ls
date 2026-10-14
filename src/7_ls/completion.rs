use strum::IntoEnumIterator;
use tower_lsp::{jsonrpc, lsp_types};

use crate::{
    builder, check_enum, parser,
    resolver::{FunctionFilter, ResolvedType, SearchFilter, VariableFilter},
    solution, tokenizer,
};

use super::{ls::PowerBuilderLS, ls_context::LSContext};

impl PowerBuilderLS {
    pub(super) fn completion_capabilities(&self, caps: &mut lsp_types::ServerCapabilities) {
        caps.completion_provider = Some(lsp_types::CompletionOptions {
            // resolve_provider: Some(true),
            // completion_item: Some(lsp_types::CompletionOptionsCompletionItem {
            //     label_details_support: Some(true),
            // }),
            ..Default::default()
        });
    }

    fn add_variable(
        items: &mut Vec<lsp_types::CompletionItem>,
        class: &Option<solution::ClassRef>,
        var: &builder::Variable,
        err: Option<String>,
    ) {
        let name = var.parsed().access.name.content.clone();
        let data_type = var.data_type.to_string();
        let variable_type = match &var.variable_type {
            builder::VariableType::Local(_) => "Local Variable".into(),
            builder::VariableType::Scoped(scoped_variable) => match scoped_variable.scope {
                tokenizer::ScopeModif::GLOBAL => "Global Variable".into(),
                tokenizer::ScopeModif::SHARED => "Shared Variable".into(),
            },
            builder::VariableType::Argument(_) => "Argument".into(),
            builder::VariableType::Instance(_) => match class {
                Some(fc) => {
                    format!("Instance variable of {}", fc.class.name())
                }
                None => "Instance Variable".into(),
            },
        };

        let detail = format!("{} {} // {}", name, data_type, variable_type);

        items.push(lsp_types::CompletionItem {
            label: name,
            label_details: Some(lsp_types::CompletionItemLabelDetails {
                detail: None,
                description: Some(data_type),
            }),
            detail: Some(detail),
            deprecated: Some(err.is_some()),
            documentation: err.map(lsp_types::Documentation::String),
            kind: Some(lsp_types::CompletionItemKind::VARIABLE),
            ..Default::default()
        });
    }

    fn add_function(
        items: &mut Vec<lsp_types::CompletionItem>,
        class: &Option<solution::ClassRef>,
        func: &builder::Function,
        err: Option<String>,
    ) {
        let mut name = func.header().parsed.name.content.clone() + "(";
        let has_args = func.header().arguments.len() > 0 || func.header().parsed.vararg.is_some();
        if has_args {
            name += "…";
        }
        name += ")";

        let mut documentation = "```powerbuilder\n".to_string();
        if let Some(fc) = class {
            documentation += &format!("{}\n", fc.class);
        }
        documentation += &format!("{}\n```", func.header().parsed);
        if let Some(help) = &func.header().parsed.help {
            if !help.trim().is_empty() {
                documentation += "\n---\n";
                documentation += help;
            }
        }

        items.push(lsp_types::CompletionItem {
            label: name,
            deprecated: Some(err.is_some()),
            documentation: Some(lsp_types::Documentation::MarkupContent(
                lsp_types::MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: documentation,
                },
            )),
            kind: Some(lsp_types::CompletionItemKind::FUNCTION),
            // insert_text: Some(insert),
            ..Default::default()
        });
    }

    fn add_complex(
        items: &mut Vec<lsp_types::CompletionItem>,
        complex: &solution::Complex,
        err: Option<String>,
    ) {
        let name = complex.name().to_string();
        let mut documentation = format!(
            "```powerbuilder\n{}\n```",
            match complex {
                solution::Complex::Class(fc) => fc.class.to_string(),
                solution::Complex::Enum(en) => en.to_string(),
            }
        );
        if let Some(help) = &complex.help() {
            if !help.trim().is_empty() {
                documentation += "\n---\n";
                documentation += help;
            }
        }

        items.push(lsp_types::CompletionItem {
            label: name,
            deprecated: Some(err.is_some()),
            documentation: Some(lsp_types::Documentation::MarkupContent(
                lsp_types::MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: documentation,
                },
            )),
            kind: Some(match complex {
                solution::Complex::Class(_) => lsp_types::CompletionItemKind::CLASS,
                solution::Complex::Enum(_) => lsp_types::CompletionItemKind::ENUM,
            }),
            ..Default::default()
        });
    }

    fn add_builtin_types(items: &mut Vec<lsp_types::CompletionItem>) {
        for t in builder::BaseType::iter() {
            items.push(lsp_types::CompletionItem {
                label: t.to_string(),
                documentation: Some(lsp_types::Documentation::MarkupContent(
                    lsp_types::MarkupContent {
                        kind: lsp_types::MarkupKind::Markdown,
                        value: format!("```powerbuilder\n{}\n```", t),
                    },
                )),
                kind: Some(lsp_types::CompletionItemKind::CLASS),
                ..Default::default()
            });
        }
    }
    fn add_builtin_variables(items: &mut Vec<lsp_types::CompletionItem>, ctx: &LSContext<'_>) {
        let Some(fc) = &ctx.class else {
            return;
        };

        items.push(lsp_types::CompletionItem {
            label: "this".to_string(),
            detail: Some(fc.class.name().clone()),
            kind: Some(lsp_types::CompletionItemKind::KEYWORD),
            ..Default::default()
        });

        items.push(lsp_types::CompletionItem {
            label: "super".to_string(),
            detail: Some(fc.class.base().to_string()),
            kind: Some(lsp_types::CompletionItemKind::KEYWORD),
            ..Default::default()
        });

        if let Some(parent) = fc.class.within() {
            items.push(lsp_types::CompletionItem {
                label: "parent".to_string(),
                detail: Some(parent.to_string()),
                kind: Some(lsp_types::CompletionItemKind::KEYWORD),
                ..Default::default()
            });
        }
    }

    fn resolve_args<'sol>(
        ctx: &'sol LSContext<'sol>,
        args: &Vec<parser::Expression>,
    ) -> Vec<&'sol ResolvedType<'sol>> {
        args.iter()
            .map(|expr| ctx.annotations.must_type(&expr.range))
            .collect()
    }

    pub(super) fn completion_impl(
        &self,
        ctx: &LSContext<'_>,
    ) -> jsonrpc::Result<Vec<lsp_types::CompletionItem>> {
        let mut items = Vec::new();
        // TODO(perf): completionItem/resolve

        match ctx.nodes.last() {
            Some(builder::Node::InstanceVariableDeclaration(_var)) => {}
            Some(builder::Node::ScopedVariableDeclaration(_var)) => {}
            Some(builder::Node::VariableDeclaration(_var)) => {}
            Some(builder::Node::Label(_token)) => {}
            Some(builder::Node::FunctionDeclaration(_func)) => {}
            Some(builder::Node::EventDeclaration(_event)) => {}
            opt @ (None | Some(builder::Node::DataType(_) | builder::Node::LValue(_))) => match opt
                .and_then(|n| check_enum!(n, builder::Node::LValue))
                .map(|lvalue| &lvalue.lvalue_type)
            {
                None
                | Some(
                    parser::LValueType::This
                    | parser::LValueType::Super
                    | parser::LValueType::Parent
                    | parser::LValueType::Variable(_),
                ) => {
                    // TODO(completion): correct behaviour for this, super, parent?

                    for (found, err) in ctx.variables(VariableFilter {
                        access: SearchFilter::error((tokenizer::AccessType::PUBLIC, false)),
                        ..Default::default()
                    }) {
                        Self::add_variable(
                            &mut items,
                            &found.class_ref(),
                            found.variable,
                            err.try_into().ok(),
                        );
                    }

                    for ((class, func), err) in ctx.functions(FunctionFilter {
                        access: SearchFilter::error(tokenizer::AccessType::PRIVATE),
                        ..Default::default()
                    }) {
                        Self::add_function(&mut items, &class, func, err.try_into().ok());
                    }

                    for (class, err) in ctx.classes(Default::default()) {
                        Self::add_complex(&mut items, &class, err.try_into().ok());
                    }

                    for en in ctx.sol.builtins().enums.values() {
                        for val in &en.values {
                            items.push(lsp_types::CompletionItem {
                                label: val.clone(),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: format!(
                                            "```powerbuilder\n{}\n{} {}\n```",
                                            en, en.name, val
                                        ),
                                    },
                                )),
                                kind: Some(lsp_types::CompletionItemKind::ENUM_MEMBER),
                                ..Default::default()
                            });
                        }
                    }

                    Self::add_builtin_types(&mut items);
                    Self::add_builtin_variables(&mut items, ctx);
                }
                Some(parser::LValueType::Member(lvalue, access)) => {
                    if let ResolvedType::Complex(solution::Complex::Class(fc)) =
                        ctx.annotations.must_type(&lvalue.range)
                    {
                        for ((class, var), err) in ctx.variables_in_class(
                            *fc,
                            VariableFilter {
                                access: SearchFilter::error((
                                    tokenizer::AccessType::PUBLIC,
                                    access.is_write,
                                )),
                                ..Default::default()
                            },
                        ) {
                            Self::add_variable(&mut items, &Some(class), var, err.try_into().ok());
                        }

                        for ((class, func), err) in ctx.functions_in_class(
                            *fc,
                            FunctionFilter {
                                access: SearchFilter::error(tokenizer::AccessType::PUBLIC),
                                ..Default::default()
                            },
                        ) {
                            Self::add_function(&mut items, &Some(class), func, err.try_into().ok());
                        }
                    }
                }
                Some(parser::LValueType::Function(call)) => {
                    for ((class, func), err) in ctx.functions(FunctionFilter {
                        arguments: SearchFilter::error(&Self::resolve_args(ctx, &call.arguments)),
                        access: SearchFilter::error(tokenizer::AccessType::PRIVATE),
                        ..Default::default()
                    }) {
                        Self::add_function(&mut items, &class, func, err.try_into().ok());
                    }
                }
                Some(parser::LValueType::Method(lvalue, call)) => {
                    if let ResolvedType::Complex(solution::Complex::Class(fc)) =
                        ctx.annotations.must_type(&lvalue.range)
                    {
                        for ((class, func), err) in ctx.functions_in_class(
                            *fc,
                            FunctionFilter {
                                arguments: SearchFilter::error(&Self::resolve_args(
                                    ctx,
                                    &call.arguments,
                                )),
                                access: SearchFilter::error(tokenizer::AccessType::PUBLIC),
                                ..Default::default()
                            },
                        ) {
                            Self::add_function(&mut items, &Some(class), func, err.try_into().ok());
                        }
                    }
                }
                Some(parser::LValueType::Index(..)) => {}
                Some(parser::LValueType::SQLAccess(..)) => {}
            },
            Some(builder::Node::Expression(_expr)) => {}
            Some(builder::Node::Statement(_)) => {}
        }

        Ok(items)
    }

    // fn completion_resolve_impl(
    //     &self,
    //     sol: &SolutionCell,
    //     mut params: lsp_types::CompletionItem,
    // ) -> jsonrpc::Result<lsp_types::CompletionItem> {
    //     let Some(data) = &params.data else {
    //         return Ok(params);
    //     };

    //     Ok(params)
    // }
}

// #[derive(serde::Serialize, serde::Deserialize)]
// struct CompletionDataClass {
//     file: Url,
//     name: String,
// }

// #[derive(serde::Serialize, serde::Deserialize)]
// struct CompletionDataFunction {
//     class: CompletionDataClass,
//     name: String,
//     signature: String,
// }

// #[derive(serde::Serialize, serde::Deserialize)]
// enum CompletionDataBody {
//     Function(CompletionDataFunction),
// }

// #[derive(serde::Serialize, serde::Deserialize)]
// struct CompletionDataVariable {
//     body: CompletionDataFunction,
//     name: String,
// }

// #[derive(serde::Serialize, serde::Deserialize)]
// enum CompletionData {
//     Variable(CompletionDataVariable),
//     Function(CompletionDataFunction),
//     Class(CompletionDataClass),

//     Enum(String),
//     EnumVariant(String, String),
// }
