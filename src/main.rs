mod ls;
mod parser;

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use ls::ls_types::{self, LintState};
use ls::ls_types::{LintProgress, Project};
use tokio::sync::RwLock;

use parser::parser_types;

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

        let proj = self.m.proj.read().await;
        let Some(file_lock) = proj.files.get(&path) else {
            return Ok(Some(CompletionResponse::Array(items)));
        };
        let mut file = file_lock.write().await;
        let top_levels = match &file.top_levels {
            ls_types::ProgressedTopLevels::Complete(top_levels) => top_levels,
            _ => return Ok(Some(CompletionResponse::Array(items))),
        };

        let pos = &params.text_document_position.position.into();
        if let Some((_, top_level_type)) = top_levels.iter().find(|(range, _)| range.contains(pos))
        {
            match top_level_type {
                ls_types::TopLevelType::ForwardDecl(_) => todo!(),
                ls_types::TopLevelType::ScopedVariableDecl(_) => todo!(),
                ls_types::TopLevelType::ScopedVariablesDecl(_) => todo!(),
                ls_types::TopLevelType::DatatypeDecl(_) => todo!(),
                ls_types::TopLevelType::TypeVariablesDecl(_) => todo!(),
                ls_types::TopLevelType::FunctionsForwardDecl(_) => todo!(),
                ls_types::TopLevelType::ExternalFunctions(_) => todo!(),
                ls_types::TopLevelType::FunctionBody((
                    (_parsed, _statements),
                    (class_mut, function),
                )) => {
                    async fn add_variable(
                        items: &mut Vec<CompletionItem>,
                        var: &ls_types::Variable,
                        err: Option<String>,
                    ) {
                        let name = var.parsed().name.clone();
                        let data_type = var.data_type.to_string();
                        let variable_type = match &var.variable_type {
                            ls_types::VariableType::Local(_) => "Local Variable".into(),
                            ls_types::VariableType::Scoped(scoped_variable) => {
                                match scoped_variable.scope {
                                    parser::tokenizer_types::ScopeModif::GLOBAL => {
                                        "Global Variable".into()
                                    }
                                    parser::tokenizer_types::ScopeModif::SHARED => {
                                        "Shared Variable".into()
                                    }
                                }
                            }
                            ls_types::VariableType::Argument(_) => "Argument".into(),
                            ls_types::VariableType::Instance((class_weak, _)) => {
                                match class_weak.upgrade() {
                                    Some(class) => {
                                        format!("Instance variable of {}", class.lock().await.name)
                                    }
                                    None => "Instance Variable".into(),
                                }
                            }
                        };

                        let unavailable = err.as_ref().map(|_| true);

                        let detail = format!("{} {} : {}", name, data_type, variable_type);

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

                    let mut state = LintState {
                        proj: self.m.proj.clone(),
                        class: class_mut.clone(),
                        variables: function
                            .lock()
                            .await
                            .definition
                            .as_ref()
                            .map_or(HashMap::new(), |def| def.variables.clone()),
                        return_type: ls_types::DataType::Void,
                        file: None,
                    };
                    state.file = Some(&mut file);

                    let (variables, err_variables) =
                        state.get_accessible_variables(pos, false).await;

                    for var in variables {
                        add_variable(&mut items, &&var.lock().await, None).await;
                    }
                    for (var, err) in err_variables {
                        add_variable(&mut items, &&var.lock().await, Some(err.into())).await;
                    }
                }
                ls_types::TopLevelType::EventBody(_) => todo!(),
                ls_types::TopLevelType::OnBody(_) => todo!(),
            };
        };

        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
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
