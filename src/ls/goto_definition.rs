use std::collections::HashMap;

use futures::StreamExt;
use tower_lsp::{jsonrpc, lsp_types::*};

use crate::{linter, parser, tokenizer, types::MaybeMut};

use super::ls::PowerBuilderLS;

impl PowerBuilderLS {
    pub(crate) fn goto_definition_capabilities(&self, caps: &mut ServerCapabilities) {
        caps.definition_provider = Some(OneOf::Left(true));
    }

    pub(crate) async fn goto_definition_impl(
        &self,
        params: GotoDefinitionParams,
    ) -> jsonrpc::Result<Option<GotoDefinitionResponse>> {
        let path = self
            .get_file(&params.text_document_position_params.text_document.uri)
            .await?;
        let proj = self.m.proj.read().await;
        let file_lock = proj.files.get(&path).unwrap();
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
            file: MaybeMut::No(&file),
        };

        let mut range = None;
        match node {
            linter::Node::VariableDeclaration(var) => {
                range = Some(var.access.name.range.clone());
            }
            linter::Node::InstanceVariableDeclaration(var) => {
                range = Some(var.variable.access.name.range.clone());
            }
            linter::Node::ScopedVariableDeclaration(var) => {
                range = Some(var.variable.access.name.range.clone());
            }
            linter::Node::VariableAccess(access) => {
                let var = linter.find_variable(access, access.is_write).await;
                if let Some(var) = var {
                    let lock = var.lock().await;
                    range = Some(lock.parsed().access.name.range.clone());
                }
            }
            linter::Node::FunctionDeclaration(func) => range = Some(func.name.range.clone()),
            linter::Node::EventDeclaration(event) => range = Some(event.name.range.clone()),
            linter::Node::LValue(lvalue) => match &lvalue.lvalue_type {
                parser::LValueType::This => {
                    if let Some(class) = linter.class {
                        range = Some(class.lock().await.parsed.name.range.clone())
                    }
                }
                parser::LValueType::Super => {
                    if let Some(class) = &linter.class {
                        if let Some(linter::Complex::Class(base)) =
                            linter.find_class(&class.lock().await.base).await
                        {
                            range = Some(base.lock().await.parsed.name.range.clone())
                        }
                    }
                }
                parser::LValueType::Parent => {
                    if let Some(class) = &linter.class {
                        if let Some(within_name) = &class.lock().await.within {
                            if let Some(linter::Complex::Class(within)) =
                                linter.find_class(within_name).await
                            {
                                range = Some(within.lock().await.parsed.name.range.clone())
                            }
                        }
                    }
                }
                parser::LValueType::Variable(_) => unreachable!(),
                parser::LValueType::Member(lvalue, access) => {
                    let data_type = linter.lint_lvalue(&*lvalue).await;
                    match data_type {
                        parser::DataTypeType::Complex(grouped_name) => {
                            if let Some(linter::Complex::Class(class_mut)) =
                                linter.find_class(&grouped_name).await
                            {
                                let mut vars = Box::pin(
                                    linter
                                        .get_variables_in_class(
                                            class_mut,
                                            &tokenizer::AccessType::PRIVATE,
                                            access.is_write,
                                        )
                                        .await,
                                );

                                while let Some((name, var)) = vars.next().await {
                                    if name == (&access.name.content).into() {
                                        range = Some(
                                            var.lock().await.parsed().access.name.range.clone(),
                                        );
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
                    let mut funcs = Box::pin(linter.get_functions().await);

                    while let Some((name, func)) = funcs.next().await {
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
                            range = Some(lock.parsed().name.range.clone());
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

                    let mut funcs = Box::pin(
                        linter
                            .get_functions_in_class(class_mut, &tokenizer::AccessType::PRIVATE)
                            .await,
                    );

                    while let Some((name, func)) = funcs.next().await {
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
                            range = Some(lock.parsed().name.range.clone());
                            break;
                        }
                    }
                }
                index @ parser::LValueType::Index(..) => {}
                parser::LValueType::SQLAccess(_, access) => {}
            },
            linter::Node::Expression(expr) => {}
            linter::Node::Statement(_) => {}
            linter::Node::DataType(dt) => {
                if let parser::DataTypeType::Complex(grouped_name) = &dt.data_type_type {
                    if let Some(linter::Complex::Class(class_mut)) =
                        linter.find_class(&grouped_name).await
                    {
                        range = Some(class_mut.lock().await.parsed.name.range.clone());
                    }
                }
            }
        }

        let Some(range) = range else { return Ok(None) };
        let uri = match Url::from_file_path(range.uri.as_path()) {
            Ok(uri) => uri,
            Err(_) => {
                return Err(jsonrpc::Error::invalid_params(format!(
                    "Invalid path: {:?}",
                    range.uri.to_str()
                )))
            }
        };

        Ok(Some(GotoDefinitionResponse::Scalar(Location {
            uri,
            range: range.into(),
        })))
    }
}
