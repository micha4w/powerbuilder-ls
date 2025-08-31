use std::collections::HashMap;

use tower_lsp::{jsonrpc, lsp_types::*};

use crate::{linter, parser, tokenizer};

use super::ls::PowerBuilderLS;

impl PowerBuilderLS {
    pub(super) fn completion_capabilities(&self, caps: &mut ServerCapabilities) {
        caps.completion_provider = Some(CompletionOptions {
            completion_item: Some(CompletionOptionsCompletionItem {
                label_details_support: Some(true),
            }),
            ..Default::default()
        });
    }

    pub(super) async fn completion_impl(
        &self,
        params: CompletionParams,
    ) -> jsonrpc::Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        self.get_file(&uri).await?;
        let proj = self.m.proj.read().await;
        let file_lock = proj.files.get(&uri).unwrap();
        let file = file_lock.read().await;

        self.client
            .log_message(MessageType::INFO, "Running completion")
            .await;

        let pos = params.text_document_position.position.into();

        let Some((top_level_type, nodes)) = proj.get_node_at(&file, &pos) else {
            return Ok(None);
        };

        let Some(node) = nodes.last() else {
            self.client
                .log_message(MessageType::INFO, "Node not found")
                .await;
            return Ok(None);
        };

        self.client
            .log_message(MessageType::INFO, format!("Found Node {}", node))
            .await;

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

        let mut items = Vec::new();
        async fn add_variable(
            items: &mut Vec<CompletionItem>,
            var: &linter::Variable,
            err: Option<String>,
        ) {
            let name = var.parsed().access.name.content.clone();
            let data_type = var.data_type.to_string();
            let variable_type = match &var.variable_type {
                linter::VariableType::Local(_) => "Local Variable".into(),
                linter::VariableType::Scoped(scoped_variable) => match scoped_variable.scope {
                    tokenizer::ScopeModif::GLOBAL => "Global Variable".into(),
                    tokenizer::ScopeModif::SHARED => "Shared Variable".into(),
                },
                linter::VariableType::Argument(_) => "Argument".into(),
                linter::VariableType::Instance((class_weak, _)) => match class_weak.upgrade() {
                    Some(class) => {
                        format!("Instance variable of {}", class.lock().await.name)
                    }
                    None => "Instance Variable".into(),
                },
            };

            let unavailable = err.as_ref().map(|_| true);

            let detail = format!("{} {} // {}", name, data_type, variable_type);

            items.push(CompletionItem {
                label: name,
                label_details: Some(CompletionItemLabelDetails {
                    detail: None,
                    description: Some(data_type),
                }),
                detail: Some(detail),
                deprecated: unavailable,
                documentation: err.map(Documentation::String),
                kind: Some(CompletionItemKind::VARIABLE),
                ..Default::default()
            });
        }

        async fn add_function(
            items: &mut Vec<CompletionItem>,
            func: &linter::Function,
            err: Option<String>,
        ) {
            let mut name = func.parsed().name.content.clone() + "(";
            let mut insert = name.clone();
            let has_args = func.parsed().arguments.len() > 0 || func.parsed().vararg.is_some();
            if has_args {
                name += "…";
            } else {
                insert += ")";
            }
            name += ")";
            let unavailable = err.as_ref().map(|_| true);

            let mut documentation = format!("```powerbuilder\n{}\n```", func);
            if let Some(help) = &func.help {
                if !help.trim().is_empty() {
                    documentation += "\n---\n";
                    documentation += help;
                }
            }

            items.push(CompletionItem {
                label: name,
                label_details: None,
                detail: None,
                deprecated: unavailable,
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: documentation,
                })),
                kind: Some(CompletionItemKind::VARIABLE),
                insert_text: Some(insert),
                ..Default::default()
            });
        }

        async fn add_data_type(
            items: &mut Vec<CompletionItem>,
            data_type: &parser::DataType,
            err: Option<String>,
        ) {
            let name = data_type.data_type_type.to_string();

            let unavailable = err.as_ref().map(|_| true);

            items.push(CompletionItem {
                label: name,
                // label_details: Some(CompletionItemLabelDetails {
                // detail: None,
                // description: Some(data_type),
                // }),
                // detail: Some(detail),
                deprecated: unavailable,
                documentation: err.map(Documentation::String),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            });
        }

        match node {
            linter::Node::InstanceVariableDeclaration(var) => {}
            linter::Node::ScopedVariableDeclaration(var) => {}
            linter::Node::VariableAccess(access) => {
                // let mut vars: Pin<Box<dyn futures::Stream<Item = _> + Send>> = Box::pin(
                //     linter
                //         .get_variables(&access.name.range.end, access.is_write)
                //         .await,
                // );

                // while let Some((name, var)) = vars.next().await {
                //     items.push
                // }
            }
            linter::Node::VariableDeclaration(var) => {}
            linter::Node::FunctionDeclaration(func) => {}
            linter::Node::EventDeclaration(event) => {}
            linter::Node::LValue(lvalue) => match &lvalue.lvalue_type {
                parser::LValueType::This => {
                    // TODO
                }
                parser::LValueType::Super => {
                    // TODO
                }
                parser::LValueType::Parent => {
                    // TODO
                }
                parser::LValueType::Variable(_) => unreachable!(),
                parser::LValueType::Member(lvalue, access) => {
                    let data_type = linter.lint_lvalue(&*lvalue).await;
                    match data_type {
                        parser::DataTypeType::Complex(grouped_name) => {
                            if let Some(linter::Complex::Class(class_mut)) =
                                linter.find_class(&grouped_name).await
                            {
                                for (_, var) in linter
                                    .get_variables_in_class(
                                        class_mut.clone(),
                                        &tokenizer::AccessType::PRIVATE,
                                        access.is_write,
                                    )
                                    .await
                                {
                                    let lock = var.lock().await;
                                    add_variable(&mut items, &lock, None).await;
                                }

                                for (_, func) in linter
                                    .get_functions_in_class(
                                        class_mut,
                                        &tokenizer::AccessType::PRIVATE,
                                    )
                                    .await
                                {
                                    let lock = func.lock().await;
                                    add_function(&mut items, &lock, None).await;
                                }
                            }
                        }
                        _ => {}
                    }
                }
                parser::LValueType::Function(call) => {
                    let arg_types = linter.lint_expressions(&call.arguments).await;
                    let mut funcs = Box::pin(linter.get_functions().await);

                    // while let Some((name, func)) = funcs.next().await {
                    //     let lock = func.lock().await;
                    //     if name == (&call.name.content).into()
                    //         && linter
                    //             .is_function_callable(
                    //                 &lock,
                    //                 &arg_types,
                    //                 &tokenizer::AccessType::PRIVATE,
                    //             )
                    //             .await
                    //     {
                    //         // title = Some(lock.to_string());
                    //         break;
                    //     }
                    // }
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

                    let mut funcs = Box::pin(
                        linter
                            .get_functions_in_class(class_mut, &tokenizer::AccessType::PRIVATE)
                            .await,
                    );

                    // while let Some((name, func)) = funcs.next().await {
                    //     let lock = func.lock().await;
                    //     if name == (&call.name.content).into()
                    //         && linter
                    //             .is_function_callable(
                    //                 &lock,
                    //                 &arg_types,
                    //                 &tokenizer::AccessType::PRIVATE,
                    //             )
                    //             .await
                    //     {
                    //         title = Some(format!("type {}\n{}", grouped_name, lock));
                    //         break;
                    //     }
                    // }
                }
                index @ parser::LValueType::Index(..) => {}
                parser::LValueType::SQLAccess(_, access) => {}
            },
            linter::Node::Expression(expr) => {}
            linter::Node::Statement(_) => {}
            linter::Node::DataType(dt) => {}
        }

        Ok(Some(CompletionResponse::Array(items)))
    }

    // let mut items = Vec::new();

    // let path = match std::path::absolute(params.text_document_position.text_document.uri.path())
    // {
    //     Ok(path) => path,
    //     Err(err) => {
    //         return Err(jsonrpc::Error::invalid_params(format!(
    //             "Invalid path: {}",
    //             err
    //         )))
    //     }
    // };

    // if let Err(err) = self
    //     .m
    //     .proj
    //     .write()
    //     .await
    //     .add_file(&path, LintProgress::Complete)
    //     .await
    // {
    //     self.client
    //         .log_message(
    //             MessageType::ERROR,
    //             format!("Failed to Lint File: {:?}", err),
    //         )
    //         .await;
    //     return Err(jsonrpc::Error::internal_error());
    // }

    // let proj = self.m.proj.read().await;
    // let Some(file_lock) = proj.files.get(&path) else {
    //     return Ok(None);
    // };
    // let mut file = file_lock.write().await;
    // let top_levels = match &file.top_levels {
    //     ls_types::ProgressedTopLevels::Complete(top_levels) => top_levels,
    //     _ => return Ok(None),
    // };

    // let pos = &params.text_document_position.position.into();
    // if let Some((_, top_level_type)) = top_levels.iter().find(|(range, _)| range.contains(pos))
    // {
    //     match top_level_type {
    //         ls_types::TopLevelType::ForwardDecl(_) => todo!(),
    //         ls_types::TopLevelType::ScopedVariableDecl(_) => todo!(),
    //         ls_types::TopLevelType::ScopedVariablesDecl(_) => todo!(),
    //         ls_types::TopLevelType::DatatypeDecl(_) => todo!(),
    //         ls_types::TopLevelType::TypeVariablesDecl(_) => todo!(),
    //         ls_types::TopLevelType::FunctionsForwardDecl(_) => todo!(),
    //         ls_types::TopLevelType::ExternalFunctions(_) => todo!(),
    //         ls_types::TopLevelType::FunctionBody((
    //             (_parsed, _statements),
    //             (class_mut, function),
    //         )) => {

    //             let mut linter = LintState {
    //                 proj: self.m.proj.clone(),
    //                 class: class_mut.clone(),
    //                 variables: function
    //                     .lock()
    //                     .await
    //                     .definition
    //                     .as_ref()
    //                     .map_or(HashMap::new(), |def| def.variables.clone()),
    //                 return_type: ls_types::DataType::Void,
    //                 file: MaybeMut::No(&file),
    //             };

    //             let (variables, err_variables) =
    //                 linter.get_accessible_variables(pos, false).await;

    //             for (_, var) in variables {
    //                 add_variable(&mut items, &&var.lock().await, None).await;
    //             }
    //             for (_, var, err) in err_variables {
    //                 add_variable(&mut items, &&var.lock().await, Some(err.into())).await;
    //             }

    //             let (data_types, err_data_types) = linter.get_accessible_data_types().await;
    //             for data_type in data_types {
    //                 add_data_type(&mut items, &data_type, None).await;
    //             }
    //             for (data_type, err) in err_data_types {
    //                 add_data_type(&mut items, &data_type, Some(err.into())).await;
    //             }

    //             let proj = self.m.proj.read().await;
    //             for class_mut in proj.builtin_classes.values() {
    //                 let class = class_mut.lock().await;
    //                 items.push(CompletionItem {
    //                     label: class.name.clone(),
    //                     label_details: Some(CompletionItemLabelDetails {
    //                         detail: None,
    //                         description: Some("builtin class".into()),
    //                     }),
    //                     detail: Some(class.name.clone() + " from " + class.base.name.as_str()),
    //                     documentation: class.help.clone().map(|help| {
    //                         Documentation::MarkupContent(MarkupContent {
    //                             kind: MarkupKind::PlainText,
    //                             value: help,
    //                         })
    //                     }),
    //                     kind: Some(CompletionItemKind::CLASS),
    //                     ..Default::default()
    //                 });
    //             }

    //             for enumerated_mut in proj.builtin_enums.values() {
    //                 let enumerated = enumerated_mut.lock().await;
    //                 items.push(CompletionItem {
    //                     label: enumerated.name.clone(),
    //                     label_details: Some(CompletionItemLabelDetails {
    //                         detail: None,
    //                         description: Some("enum".into()),
    //                     }),
    //                     documentation: enumerated.help.clone().map(Documentation::String),
    //                     kind: Some(CompletionItemKind::ENUM),
    //                     ..Default::default()
    //                 });

    //                 for member in &enumerated.values {
    //                     items.push(CompletionItem {
    //                         label: member.clone() + "!",
    //                         label_details: Some(CompletionItemLabelDetails {
    //                             detail: None,
    //                             description: Some(enumerated.name.clone()),
    //                         }),
    //                         documentation: enumerated.help.clone().map(Documentation::String),
    //                         kind: Some(CompletionItemKind::ENUM_MEMBER),
    //                         ..Default::default()
    //                     });
    //                 }
    //             }
    //         }
    //         ls_types::TopLevelType::EventBody(_) => todo!(),
    //         ls_types::TopLevelType::OnBody(_) => todo!(),
    //     };
    // };

    //     Ok(Some(CompletionResponse::Array(items)))
    // }
}
