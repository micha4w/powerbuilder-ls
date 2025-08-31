use std::collections::HashMap;

use tower_lsp::{jsonrpc, lsp_types::*};

use crate::{linter, parser, tokenizer};

use super::ls::PowerBuilderLS;

impl PowerBuilderLS {
    pub(super) fn hover_capabilities(&self, caps: &mut ServerCapabilities) {
        caps.hover_provider = Some(HoverProviderCapability::Simple(true));
    }

    pub(super) async fn hover_impl(&self, params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        self.get_file(&uri).await?;
        let proj = self.m.proj.read().await;
        let file_lock = proj.files.get(&uri).unwrap();
        let file = file_lock.read().await;

        let pos = params.text_document_position_params.position.into();
        let Some((top_level_type, nodes)) = proj.get_node_at(&file, &pos) else {
            return Ok(None);
        };
        let Some(node) = nodes.last() else {
            return Ok(None);
        };

        let mut linter = linter::Linter {
            proj: &proj,
            class: top_level_type.get_class().cloned(),
            variables: top_level_type
                .get_variables()
                .await
                .unwrap_or_else(HashMap::new),
            return_type: parser::DataTypeType::Void,
            file: file_lock,
            diagnose: false,
        };

        let mut title = None;
        let mut description = None;
        let range = node.get_range();
        match node {
            linter::Node::InstanceVariableDeclaration(var) => {
                let mut name = var.to_string();

                if let Some(class) = linter.class {
                    name = format!("type {}\n{}", class.lock().await.name, name);
                }
                title = Some(name);
            }
            linter::Node::ScopedVariableDeclaration(var) => title = Some(var.to_string()),
            linter::Node::VariableAccess(access) => {
                for (name, var) in linter
                    .get_variables(&access.name.range.end, access.is_write)
                    .await
                {
                    if name == (&access.name.content).into() {
                        let lock = var.lock().await;
                        if let linter::VariableType::Instance((class_wk, _)) = &lock.variable_type {
                            if let Some(class) = class_wk.upgrade() {
                                title = Some(format!(
                                    "type {}\n{}",
                                    class.lock().await.name,
                                    lock.parsed(),
                                ));
                                break;
                            }
                        }

                        title = Some(lock.parsed().to_string());
                        break;
                    }
                }
            }
            linter::Node::VariableDeclaration(var) => title = Some(var.to_string()),
            linter::Node::FunctionDeclaration(func) => title = Some(func.to_string()),
            linter::Node::EventDeclaration(event) => title = Some(event.to_string()),
            linter::Node::LValue(lvalue) => match &lvalue.lvalue_type {
                parser::LValueType::This => {
                    let mut contents = "this (Reserved Keyword)".to_owned();
                    if let Some(class) = top_level_type.get_class() {
                        contents += ": ";
                        contents += &class.lock().await.name;
                    }
                    title = Some(contents);
                }
                parser::LValueType::Super => {
                    let mut contents = "super (Reserved Keyword)".to_owned();
                    if let Some(class) = linter.class {
                        contents += ": ";
                        contents += &class.lock().await.base.to_string();
                    }
                    title = Some(contents);
                }
                parser::LValueType::Parent => {
                    let mut contents = "parent (Reserved Keyword)".to_owned();
                    if let Some(class) = linter.class {
                        if let Some(within) = &class.lock().await.within {
                            contents += ": ";
                            contents += &within.to_string();
                        }
                    }
                    title = Some(contents);
                }
                parser::LValueType::Variable(_) => unreachable!(),
                parser::LValueType::Member(lvalue, access) => {
                    let data_type = linter.lint_lvalue(&*lvalue).await;
                    match data_type {
                        parser::DataTypeType::Complex(grouped_name) => {
                            if let Some(linter::Complex::Class(class_mut)) =
                                linter.find_class(&grouped_name).await
                            {
                                for (name, var) in linter
                                    .get_variables_in_class(
                                        class_mut,
                                        &tokenizer::AccessType::PRIVATE,
                                        access.is_write,
                                    )
                                    .await
                                {
                                    if name == (&access.name.content).into() {
                                        title = Some(format!(
                                            "type {}\n{}",
                                            grouped_name,
                                            var.lock().await.unwrap_instance().1
                                        ));
                                        break;
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
                parser::LValueType::Function(call) => {
                    let arg_types = linter.lint_expressions(&call.arguments).await;

                    for (name, func) in linter.get_functions().await {
                        let lock = func.lock().await;
                        if name == (&call.name.content).into()
                            && linter
                                .is_function_callable(
                                    &lock,
                                    &arg_types,
                                    &tokenizer::AccessType::PRIVATE,
                                )
                                .await
                        {
                            title = Some(lock.to_string());
                            description = lock.help.clone();
                            break;
                        }
                    }
                }
                parser::LValueType::Method(lvalue, call) => {
                    let arg_types = linter.lint_expressions(&call.arguments).await;
                    let lvalue_type = linter.lint_lvalue(&lvalue).await;

                    let parser::DataTypeType::Complex(grouped_name) = lvalue_type else {
                        return Ok(None);
                    };

                    let Some(linter::Complex::Class(class_mut)) =
                        linter.find_class(&grouped_name).await
                    else {
                        return Ok(None);
                    };

                    for (name, func) in linter
                        .get_functions_in_class(class_mut, &tokenizer::AccessType::PRIVATE)
                        .await
                    {
                        let lock = func.lock().await;
                        if name == (&call.name.content).into()
                            && linter
                                .is_function_callable(
                                    &lock,
                                    &arg_types,
                                    &tokenizer::AccessType::PRIVATE,
                                )
                                .await
                        {
                            title = Some(format!("type {}\n{}", grouped_name, lock));
                            description = lock.help.clone();
                            break;
                        }
                    }
                }
                index @ parser::LValueType::Index(..) => title = Some(index.to_string()),
                parser::LValueType::SQLAccess(_, access) => {
                    title = Some(format!(":{}", access.lvalue_type.to_string()))
                }
            },
            linter::Node::Expression(expr) => title = Some(expr.expression_type.to_string()),
            linter::Node::Statement(_) => {}
            linter::Node::DataType(dt) => {
                let dt_name = dt.data_type_type.to_string();
                title = Some(match &dt.data_type_type {
                    parser::DataTypeType::Complex(grouped_name) => {
                        match linter.find_class(&grouped_name).await {
                            Some(linter::Complex::Class(class_mut)) => {
                                let lock = class_mut.lock().await;
                                description = lock.help.clone();
                                lock.to_string()
                            }
                            Some(linter::Complex::Enum(en)) => {
                                description = en.lock().await.help.clone();
                                format!("type {} from enumerated", dt_name)
                            }
                            None => format!("type {}", dt_name),
                        }
                    }
                    _ => format!("type {} from primitives", dt_name),
                })
            }
        }

        let Some(title) = title else { return Ok(None) };

        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: {
                    let mut content = format!("```powerbuilder\n{}\n```", title);
                    if let Some(desc) = description {
                        content += "\n---\n";
                        content += &desc;
                    }
                    content
                }
            }),
            range: Some(range.clone().into()),
        }))
    }
}
