use std::any::Any;

use tower_lsp::{jsonrpc, lsp_types};

use crate::{
    builder, parser, project,
    resolver::{self, ResolvedType},
    tokenizer,
    types::Found,
};

use super::{ls::PowerBuilderLS, ls_context::LSContext};

impl PowerBuilderLS {
    pub(super) fn completion_capabilities(&self, caps: &mut lsp_types::ServerCapabilities) {
        caps.completion_provider = Some(lsp_types::CompletionOptions {
            completion_item: Some(lsp_types::CompletionOptionsCompletionItem {
                label_details_support: Some(true),
            }),
            ..Default::default()
        });
    }

    fn add_variable(
        items: &mut Vec<lsp_types::CompletionItem>,
        class: &Option<project::ClassRef>,
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
        class: &Option<project::ClassRef>,
        func: &builder::Function,
        err: Option<String>,
    ) {
        let mut name = func.header().parsed.name.content.clone() + "(";
        let insert = name.clone() + ")";
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
            label_details: None,
            detail: None,
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

    // fn add_data_type(
    //     items: &mut Vec<lsp_types::CompletionItem>,
    //     data_type: &parser::DataType,
    //     err: Option<String>,
    // ) {
    //     let name = data_type.to_string();

    //     items.push(lsp_types::CompletionItem {
    //         label: name,
    //         // label_details: Some(CompletionItemLabelDetails {
    //         // detail: None,
    //         // description: Some(data_type),
    //         // }),
    //         // detail: Some(detail),
    //         deprecated: Some(err.is_some()),
    //         documentation: err.map(lsp_types::Documentation::String),
    //         kind: Some(lsp_types::CompletionItemKind::CLASS),
    //         ..Default::default()
    //     });
    // }

    pub(super) fn completion_impl(
        &self,
        ctx: &LSContext<'_>,
    ) -> jsonrpc::Result<Vec<lsp_types::CompletionItem>> {
        let mut items = Vec::new();

        match ctx.lowest_node {
            builder::Node::InstanceVariableDeclaration(var) => {}
            builder::Node::ScopedVariableDeclaration(var) => {}
            builder::Node::VariableDeclaration(var) => {}
            builder::Node::Label(token) => {}
            builder::Node::FunctionDeclaration(func) => {}
            builder::Node::EventDeclaration(event) => {}
            builder::Node::LValue(lvalue) => match &lvalue.lvalue_type {
                parser::LValueType::This
                | parser::LValueType::Super
                | parser::LValueType::Parent
                | parser::LValueType::Variable(_) => {
                    // TODO(completion): correct behaviour for this, super, parent?
                    // TODO(completion): also add builtins to the completion list

                    for var in ctx.variables(resolver::VariableFilter::All) {
                        let found = var.expect("filter All does not error");
                        Self::add_variable(&mut items, &found.class_ref(), found.variable, None);
                    }

                    for func in ctx.functions(resolver::FunctionFilter::All) {
                        let (class, func) = func.expect("filter All does not error");
                        Self::add_function(&mut items, &class, func, None);
                    }

                    for (_, file) in &ctx.proj.files {
                        for class in &file.meta().classes {
                            // TODO(completion): add class
                            // TODO(proj): also store class base and within
                        }
                    }
                }
                parser::LValueType::Member(lvalue, access) => {
                    if let ResolvedType::Complex(project::Complex::Class(fc)) =
                        ctx.annotations.must_type(&lvalue.range)
                    {
                        // TODO(completion): error when member is not accessible
                        for var in ctx.variables_in_class(*fc, resolver::VariableFilter::All) {
                            let (class, var) = var.expect("filter All does not error");
                            Self::add_variable(&mut items, &Some(class), var, None);
                        }

                        for func in ctx.functions_in_class(*fc, resolver::FunctionFilter::All) {
                            let (class, func) = func.expect("filter All does not error");
                            Self::add_function(&mut items, &Some(class), func, None);
                        }
                    }
                }
                parser::LValueType::Function(call) => {
                    // TODO(completion): error when arguments do not match
                    for func in ctx.functions(resolver::FunctionFilter::All) {
                        let (class, func) = func.expect("filter All does not error");
                        Self::add_function(&mut items, &class, func, None);
                    }
                }
                parser::LValueType::Method(lvalue, call) => {
                    if let ResolvedType::Complex(project::Complex::Class(fc)) =
                        ctx.annotations.must_type(&lvalue.range)
                    {
                        for func in ctx.functions_in_class(*fc, resolver::FunctionFilter::All) {
                            let (class, func) = func.expect("filter All does not error");
                            Self::add_function(&mut items, &Some(class), func, None);
                        }
                    }
                }
                parser::LValueType::Index(..) => {}
                parser::LValueType::SQLAccess(..) => {}
            },
            builder::Node::Expression(expr) => {}
            builder::Node::Statement(_) => {}
            builder::Node::DataType(dt) => {}
        }

        Ok(items)
    }
}
