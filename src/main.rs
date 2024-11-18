mod ls;
mod parser;

use std::collections::HashMap;
use std::panic::PanicHookInfo;
use std::path::PathBuf;
use std::pin::Pin;
use std::sync::Arc;

use futures::StreamExt;
use ls::ls_types::{self, LintState, MaybeMut};
use ls::ls_types::{LintProgress, Project};
use tokio::sync::RwLock;

use parser::parser_types::{self, EitherOr};

use tower_lsp::jsonrpc;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct PowerBuilderLSCreator {
    proj: Arc<RwLock<Project>>,
    // watcher: Arc<Mutex<notify::INotifyWatcher>>,
    // watcher_rx: mpsc::Receiver<notify::Event>,
}

impl PowerBuilderLSCreator {
    async fn new(system_files_root: PathBuf) -> anyhow::Result<Self> {
        // let (tx, rx) = mpsc::channel::<notify::Event>(1);
        // let watcher =
        //     notify::recommended_watcher(move |res: Result<notify::Event, notify::Error>| {
        //         let err = match res {
        //             Ok(event) => tx.blocking_send(event).map_err(anyhow::Error::msg),
        //             Err(err) => Err(anyhow::Error::msg(err)),
        //         };

        //         eprintln!("File watcher failed to watch: {:?}", err);
        //     })?;

        let proj = Arc::new(RwLock::new(
            Project::new(
                system_files_root.join("enums.pb"),
                system_files_root.join("classes.pb"),
                system_files_root.join("system_functions.pb"),
            )
            .await?,
        ));

        Ok(Self {
            proj,
            // watcher: Arc::new(Mutex::new(watcher)),
            // watcher_rx: rx,
        })
    }
}

#[derive(Debug)]
struct PowerBuilderLS {
    client: Client,
    m: PowerBuilderLSCreator,
    ver: i32,
}

impl PowerBuilderLS {
    fn new(client: Client, creator: PowerBuilderLSCreator) -> Self {
        Self {
            client,
            m: creator,
            ver: 0,
        }
    }

    async fn get_file_diagnostics(&self, path: PathBuf) -> jsonrpc::Result<Vec<Diagnostic>> {
        self.m.proj.write().await.files.remove(&path);
        if let Err(err) = ls::add_file(self.m.proj.clone(), &path, LintProgress::Complete).await {
            self.client
                .log_message(
                    MessageType::ERROR,
                    format!("Failed to Lint File: {:?}", err),
                )
                .await;
            return Err(jsonrpc::Error::internal_error());
        }

        let items;
        if let Some(file) = self.m.proj.read().await.files.get(&path) {
            items = file
                .read()
                .await
                .diagnostics
                .iter()
                .map(|d| Diagnostic {
                    range: Range::new(
                        Position::new(d.range.start.line, d.range.start.column),
                        Position::new(d.range.end.line, d.range.end.column),
                    ),
                    severity: Some(match d.severity {
                        parser_types::Severity::Error => DiagnosticSeverity::ERROR,
                        parser_types::Severity::Warning => DiagnosticSeverity::WARNING,
                        parser_types::Severity::Info => DiagnosticSeverity::INFORMATION,
                        parser_types::Severity::Hint => DiagnosticSeverity::HINT,
                    }),
                    message: d.message.clone(),
                    ..Default::default()
                })
                .collect();
        } else {
            let mut json_err = jsonrpc::Error::internal_error();
            json_err.message = "File not found".into();
            return Err(json_err);
        }

        Ok(items)
    }

    // async fn file_change_watcher(&mut self) {
    //     while let Some(i) = self.m.watcher_rx.recv().await {
    //         for path in i.paths {}
    //     }
    // }
}

#[tower_lsp::async_trait]
impl LanguageServer for PowerBuilderLS {
    async fn initialize(&self, _: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                // diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                //     DiagnosticOptions {
                //         identifier: None,
                //         inter_file_dependencies: true,
                //         workspace_diagnostics: false,
                //         work_done_progress_options: WorkDoneProgressOptions {
                //             work_done_progress: None,
                //         },
                //     },
                // )),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                        ..Default::default()
                    },
                )),
                completion_provider: Some(CompletionOptions {
                    completion_item: Some(CompletionOptionsCompletionItem {
                        label_details_support: Some(true),
                    }),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    // async fn diagnostic(
    //     &self,
    //     params: DocumentDiagnosticParams,
    // ) -> jsonrpc::Result<DocumentDiagnosticReportResult> {
    //     self.client
    //         .log_message(
    //             MessageType::INFO,
    //             format!("file diagnostics! {:?}", params.text_document),
    //         )
    //         .await;

    //     let path: PathBuf = params.text_document.uri.path().into();
    //     let items = self.get_file_diagnostics(path).await?;
    //     Ok(DocumentDiagnosticReportResult::Report(
    //         DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
    //             related_documents: None,
    //             full_document_diagnostic_report: FullDocumentDiagnosticReport {
    //                 result_id: None,
    //                 items,
    //             },
    //         }),
    //     ))
    // }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(
                MessageType::INFO,
                format!("file opened! {:?}", params.text_document),
            )
            .await;

        let path = match std::path::absolute(params.text_document.uri.path()) {
            Ok(path) => path,
            Err(err) => {
                return self
                    .client
                    .log_message(
                        MessageType::ERROR,
                        format!("Failed to canonicalize Path: {}", err),
                    )
                    .await
            }
        };
        match self.get_file_diagnostics(path).await {
            Ok(items) => {
                self.client
                    .publish_diagnostics(params.text_document.uri, items, None)
                    .await
            }
            Err(err) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("Failed to get File diagnostics: {:?}", err),
                    )
                    .await
            }
        }
    }

    // async fn did_change(&self, params: DidChangeTextDocumentParams) {
    //     self.m.proj.write().await.ver =  params.text_document.version;
    // }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.client
            .log_message(
                MessageType::INFO,
                format!("file saved! {:?}", params.text_document),
            )
            .await;

        let path = match std::path::absolute(params.text_document.uri.path()) {
            Ok(path) => path,
            Err(err) => {
                return self
                    .client
                    .log_message(
                        MessageType::ERROR,
                        format!("Failed to canonicalize Path: {}", err),
                    )
                    .await
            }
        };
        match self.get_file_diagnostics(path).await {
            Ok(items) => {
                self.client
                    .publish_diagnostics(params.text_document.uri, items, None)
                    .await
            }
            Err(err) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("Failed to get File diagnostics: {:?}", err),
                    )
                    .await
            }
        }
    }

    async fn hover(&self, params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        let path = match std::path::absolute(
            params
                .text_document_position_params
                .text_document
                .uri
                .path(),
        ) {
            Ok(path) => path,
            Err(err) => {
                return Err(jsonrpc::Error::invalid_params(format!(
                    "Invalid path: {}",
                    err
                )))
            }
        };

        if let Err(err) = ls::add_file(self.m.proj.clone(), &path, LintProgress::Complete).await {
            self.client
                .log_message(
                    MessageType::ERROR,
                    format!("Failed to Lint File: {:?}", err),
                )
                .await;
            return Err(jsonrpc::Error::internal_error());
        }

        let proj = self.m.proj.read().await;
        let Some(file_lock) = proj.files.get(&path) else {
            return Ok(None);
        };
        let file = file_lock.read().await;
        let top_levels = match &file.top_levels {
            ls_types::ProgressedTopLevels::Complete(top_levels) => top_levels,
            _ => return Ok(None),
        };

        let pos = &params.text_document_position_params.position.into();
        let Some((_, top_level_type)) = top_levels.iter().find(|(range, _)| range.contains(pos))
        else {
            return Ok(None);
        };

        let mut state = LintState {
            proj: self.m.proj.clone(),
            class: top_level_type.get_class().cloned(),
            variables: top_level_type
                .get_variables()
                .await
                .unwrap_or_else(HashMap::new),
            return_type: ls_types::DataType::Void,
            file: MaybeMut::No(&file),
        };

        match top_level_type.get_expression_at(pos) {
            Some(EitherOr::Left(expression)) => match &expression.expression_type {
                parser_types::ExpressionType::Literal(literal) => {
                    return Ok(Some(Hover {
                        contents: HoverContents::Scalar(MarkedString::String(format!(
                            "{} (literal): {}",
                            literal.content, literal.literal_type
                        ))),
                        range: Some(literal.range.into()),
                    }));
                }
                parser_types::ExpressionType::ArrayLiteral(vec) => {}
                parser_types::ExpressionType::Operation(expression, operator, expression1) => {}
                parser_types::ExpressionType::UnaryOperation(operator, expression) => {}
                parser_types::ExpressionType::IncrementDecrement(expression, symbol) => {}
                parser_types::ExpressionType::BooleanNot(expression) => {}
                parser_types::ExpressionType::Parenthesized(expression) => {}
                parser_types::ExpressionType::Create(data_type) => {}
                parser_types::ExpressionType::CreateUsing(expression) => {}
                parser_types::ExpressionType::LValue(lvalue) => unreachable!(),
                parser_types::ExpressionType::Error => {}
            },
            Some(EitherOr::Right(lvalue)) => match &lvalue.lvalue_type {
                parser_types::LValueType::This => {
                    let mut contents = "this (Reserved Keyword)".to_owned();
                    if let Some(class) = top_level_type.get_class() {
                        contents += ": ";
                        contents += &class.lock().await.name;
                    }
                    return Ok(Some(Hover {
                        contents: HoverContents::Scalar(MarkedString::String(contents)),
                        range: Some(lvalue.range.into()),
                    }));
                }
                parser_types::LValueType::Super => {
                    let mut contents = "super (Reserved Keyword)".to_owned();
                    if let Some(class) = top_level_type.get_class() {
                        contents += ": ";
                        contents += &class.lock().await.base.combine();
                    }
                    return Ok(Some(Hover {
                        contents: HoverContents::Scalar(MarkedString::String(contents)),
                        range: Some(lvalue.range.into()),
                    }));
                }
                parser_types::LValueType::Parent => {
                    let mut contents = "parent (Reserved Keyword)".to_owned();
                    if let Some(class) = top_level_type.get_class() {
                        if let Some(within) = &class.lock().await.within {
                            contents += ": ";
                            contents += &within.combine();
                        }
                    }
                    return Ok(Some(Hover {
                        contents: HoverContents::Scalar(MarkedString::String(contents)),
                        range: Some(lvalue.range.into()),
                    }));
                }
                parser_types::LValueType::Variable(access) => {
                    let mut vars: Pin<Box<dyn futures::Stream<Item = _> + Send>> = Box::pin(
                        state
                            .get_variables(&lvalue.range.end, access.is_write)
                            .await,
                    );

                    while let Some((name, var)) = vars.next().await {
                        if name == (&access.name.content).into() {
                            let contents = name.to_string()
                                + " (variable):"
                                + &var.lock().await.data_type.to_string();

                            return Ok(Some(Hover {
                                contents: HoverContents::Scalar(MarkedString::String(contents)),
                                range: Some(lvalue.range.into()),
                            }));
                        }
                    }
                }
                parser_types::LValueType::Member(lvalue, access) => {
                    let data_type = state.lint_lvalue(&*lvalue).await;
                    match data_type {
                        ls_types::DataType::Complex(grouped_name) => {
                            if let Some(ls_types::Complex::Class(class_mut)) =
                                state.find_class(&grouped_name).await
                            {
                                let mut vars = Box::pin(
                                    state
                                        .get_variables_in_class(
                                            class_mut,
                                            &parser::tokenizer_types::AccessType::PRIVATE,
                                            access.is_write,
                                        )
                                        .await,
                                );

                                while let Some((name, var)) = vars.next().await {
                                    if name == (&access.name.content).into() {
                                        let contents = name.to_string()
                                            + " (variable):"
                                            + &var.lock().await.data_type.to_string();

                                        return Ok(Some(Hover {
                                            contents: HoverContents::Scalar(MarkedString::String(
                                                contents,
                                            )),
                                            range: Some(access.name.range.into()),
                                        }));
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
                parser_types::LValueType::Function(call) => {
                    let arg_types = state.lint_expressions(&call.arguments).await;
                    let mut funcs = Box::pin(state.get_functions().await);

                    while let Some((name, func)) = funcs.next().await {
                        let lock = func.lock().await;
                        if name == (&call.name.content).into()
                            && state
                                .is_function_callable(
                                    &lock,
                                    &arg_types,
                                    &parser::tokenizer_types::AccessType::PRIVATE,
                                )
                                .await
                        {
                            let contents = lock.to_string();

                            return Ok(Some(Hover {
                                contents: HoverContents::Scalar(MarkedString::String(contents)),
                                range: Some(call.name.range.into()),
                            }));
                        }
                    }
                }
                parser_types::LValueType::Method(lvalue, call) => {
                    let arg_types = state.lint_expressions(&call.arguments).await;
                    let lvalue_type = state.lint_lvalue(&lvalue).await;

                    let ls_types::DataType::Complex(grouped_name) = lvalue_type else {
                        return Ok(None);
                    };

                    let Some(ls_types::Complex::Class(class_mut)) =
                        state.find_class(&grouped_name).await
                    else {
                        return Ok(None);
                    };

                    let mut funcs = Box::pin(
                        state
                            .get_functions_in_class(
                                class_mut,
                                &parser::tokenizer_types::AccessType::PRIVATE,
                            )
                            .await,
                    );

                    while let Some((name, func)) = funcs.next().await {
                        let lock = func.lock().await;
                        if name == (&call.name.content).into()
                            && state
                                .is_function_callable(
                                    &lock,
                                    &arg_types,
                                    &parser::tokenizer_types::AccessType::PRIVATE,
                                )
                                .await
                        {
                            let contents = lock.to_string();

                            return Ok(Some(Hover {
                                contents: HoverContents::Scalar(MarkedString::String(contents)),
                                range: Some(call.name.range.into()),
                            }));
                        }
                    }
                }
                parser_types::LValueType::Index(lvalue, expression) => return Ok(None),
            },
            None => {}
        }

        if let Some(statement) = top_level_type.get_statement_at(pos) {
            match statement.get_variable_at(pos) {
                Some(EitherOr::Left(var)) => {
                    if let Some(var_mut) = state.variables.get(&(&var.variable.name.content).into())
                    {
                        let contents = var.variable.name.content.clone()
                            + " (variable):"
                            + &var_mut.lock().await.data_type.to_string();

                        return Ok(Some(Hover {
                            contents: HoverContents::Scalar(MarkedString::String(contents)),
                            range: Some(var.variable.name.range.into()),
                        }));
                    }
                }
                Some(EitherOr::Right(var)) => {
                    if let Some(var_mut) = state.variables.get(&(&var.name.content).into()) {
                        let contents = var.name.content.clone()
                            + " (variable):"
                            + &var_mut.lock().await.data_type.to_string();

                        return Ok(Some(Hover {
                            contents: HoverContents::Scalar(MarkedString::String(contents)),
                            range: Some(var.name.range.into()),
                        }));
                    }
                }
                None => {}
            };
        };

        Ok(None)
        // Ok(Some(CompletionResponse::Array(items)))
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> jsonrpc::Result<Option<CompletionResponse>> {
        let mut items = Vec::new();

        let path = match std::path::absolute(params.text_document_position.text_document.uri.path())
        {
            Ok(path) => path,
            Err(err) => {
                return Err(jsonrpc::Error::invalid_params(format!(
                    "Invalid path: {}",
                    err
                )))
            }
        };

        if let Err(err) = ls::add_file(self.m.proj.clone(), &path, LintProgress::Complete).await {
            self.client
                .log_message(
                    MessageType::ERROR,
                    format!("Failed to Lint File: {:?}", err),
                )
                .await;
            return Err(jsonrpc::Error::internal_error());
        }

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
        //             async fn add_variable(
        //                 items: &mut Vec<CompletionItem>,
        //                 var: &ls_types::Variable,
        //                 err: Option<String>,
        //             ) {
        //                 let name = var.parsed().name.clone();
        //                 let data_type = var.data_type.to_string();
        //                 let variable_type = match &var.variable_type {
        //                     ls_types::VariableType::Local(_) => "Local Variable".into(),
        //                     ls_types::VariableType::Scoped(scoped_variable) => {
        //                         match scoped_variable.scope {
        //                             parser::tokenizer_types::ScopeModif::GLOBAL => {
        //                                 "Global Variable".into()
        //                             }
        //                             parser::tokenizer_types::ScopeModif::SHARED => {
        //                                 "Shared Variable".into()
        //                             }
        //                         }
        //                     }
        //                     ls_types::VariableType::Argument(_) => "Argument".into(),
        //                     ls_types::VariableType::Instance((class_weak, _)) => {
        //                         match class_weak.upgrade() {
        //                             Some(class) => {
        //                                 format!("Instance variable of {}", class.lock().await.name)
        //                             }
        //                             None => "Instance Variable".into(),
        //                         }
        //                     }
        //                 };

        //                 let unavailable = err.as_ref().map(|_| true);

        //                 let detail = format!("{} {} : {}", name.content, data_type, variable_type);

        //                 items.push(CompletionItem {
        //                     label: name.content,
        //                     label_details: Some(CompletionItemLabelDetails {
        //                         detail: None,
        //                         description: Some(data_type),
        //                     }),
        //                     detail: Some(detail),
        //                     deprecated: unavailable,
        //                     documentation: err.map(Documentation::String),
        //                     kind: Some(CompletionItemKind::VARIABLE),
        //                     ..Default::default()
        //                 });
        //             }

        //             async fn add_data_type(
        //                 items: &mut Vec<CompletionItem>,
        //                 data_type: &ls_types::DataType,
        //                 err: Option<String>,
        //             ) {
        //                 let name = data_type.to_string();

        //                 let unavailable = err.as_ref().map(|_| true);

        //                 items.push(CompletionItem {
        //                     label: name,
        //                     // label_details: Some(CompletionItemLabelDetails {
        //                     // detail: None,
        //                     // description: Some(data_type),
        //                     // }),
        //                     // detail: Some(detail),
        //                     deprecated: unavailable,
        //                     documentation: err.map(Documentation::String),
        //                     kind: Some(CompletionItemKind::CLASS),
        //                     ..Default::default()
        //                 });
        //             }

        //             let mut state = LintState {
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
        //                 state.get_accessible_variables(pos, false).await;

        //             for (_, var) in variables {
        //                 add_variable(&mut items, &&var.lock().await, None).await;
        //             }
        //             for (_, var, err) in err_variables {
        //                 add_variable(&mut items, &&var.lock().await, Some(err.into())).await;
        //             }

        //             let (data_types, err_data_types) = state.get_accessible_data_types().await;
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

        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }
}

fn panic_hook(info: &PanicHookInfo) {
    if let Err(err) = std::fs::write(
        "/home/micha4w/Code/Rust/powerbuilder-ls/res/log.txt",
        info.to_string(),
    ) {
        eprintln!("{}", err);
    };
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    std::panic::set_hook(Box::new(panic_hook));

    let creator =
        PowerBuilderLSCreator::new("/home/micha4w/Code/Rust/powerbuilder-ls/system".into()).await?;

    // ls::add_file(
    //     creator.proj.clone(),
    //     &"/home/micha4w/Code/Rust/powerbuilder-ls/res/pfc_w_find.srw".into(),
    //     ls_types::LintProgress::Complete,
    // )
    // .await?;

    // for (x,y) in &creator.proj.read().await.files {
    //     for diagnostic in &y.read().await.diagnostics {
    //         println!("{:?}", diagnostic)
    //     }
    // }

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(move |client| PowerBuilderLS::new(client, creator));
    Server::new(stdin, stdout, socket).serve(service).await;

    // lsp::add_file(
    //     proj_arc.clone(),
    //     "res/test.sru".into(),
    //     lsp_types::LintProgress::Complete,
    // )
    // .await?;

    // let path = "res/test.sru";
    // let mut tokens = FileTokenizer::open(path)?;
    // tokens.skip_headers();
    // // TODO stream the tokens

    // let mut parser = Parser::new(&mut tokens);
    // parser.parse_tokens();

    // let buf = Bytes::from_iter(std::fs::read("system/classes.pb")?.iter().cloned());
    // let classes = powerbuilder::Classes::decode(buf)?;

    // let file = File::open("system/system_functions.json")?;
    // let reader = BufReader::new(file);
    // let u : powerbuilder::Functions = serde_json::from_reader(reader)?;

    // let v = u.encode_to_vec();
    // std::fs::write("system/system_functions.pb", v)?;

    // let buf = Bytes::from_iter(std::fs::read("system/enums2.pb")?.iter().cloned());
    // let classes = powerbuilder::Enums::decode(buf)?;

    // let buf = std::fs::read("system/enums2.pb")?;
    // let mut cursor: Cursor<Vec<u8>> = Cursor::new(buf);
    // let enums = output::Enums::decode_length_delimited(&mut cursor)?;
    // lsp.add_file("res/u_filehandler.sru".into(), LintState::Complete)?;
    // lsp.add_file("res/pfc_w_find.srw".into(), LintState::Complete)?;

    // for token in tokens {
    //     println!("{:?} {:?} {}:{}-{}:{}", token.token_type, token.content, token.range.start.line, token.range.start.column, token.range.end.line, token.range.end.column);
    // }

    Ok(())
}
