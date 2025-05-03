use std::{collections::HashMap, path::PathBuf, sync::Arc};

use crate::linter::{self, File};
use ropey::Rope;
use tokio::sync::{oneshot, Mutex, RwLock};

use tower_lsp::{jsonrpc, lsp_types::*, Client, LanguageServer};

#[derive(Debug)]
pub struct PowerBuilderLS {
    pub client: Client,
    pub(super) m: PowerBuilderLSCreator,
}

#[tower_lsp::async_trait]
impl LanguageServer for PowerBuilderLS {
    async fn initialize(&self, _: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        let mut capabilities = ServerCapabilities {
            workspace: Some(WorkspaceServerCapabilities {
                workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                    supported: Some(true),
                    change_notifications: Some(OneOf::Left(true)),
                }),
                file_operations: None,
            }),
            text_document_sync: Some(TextDocumentSyncCapability::Options(
                TextDocumentSyncOptions {
                    open_close: Some(true),
                    save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                        include_text: Some(false),
                    })),
                    change: Some(TextDocumentSyncKind::INCREMENTAL),
                    ..Default::default()
                },
            )),
            ..Default::default()
        };
        self.hover_capabilities(&mut capabilities);
        self.completion_capabilities(&mut capabilities);
        self.goto_definition_capabilities(&mut capabilities);
        self.diagnostics_capabilities(&mut capabilities);

        Ok(InitializeResult {
            server_info: None,
            capabilities,
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn hover(&self, params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        self.hover_impl(params).await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> jsonrpc::Result<Option<GotoDefinitionResponse>> {
        self.goto_definition_impl(params).await
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> jsonrpc::Result<Option<CompletionResponse>> {
        self.completion_impl(params).await
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.file_changed_on_disk(params.text_document.uri).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.file_changed_on_disk(params.text_document.uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        {
            let mut proj = self.m.proj.write().await;
            if !proj.files.contains_key(&uri) {
                match File::new(uri.clone()) {
                    Err(err) => {
                        self.client
                            .log_message(
                                MessageType::ERROR,
                                format!("Failed to Lint File: {:?}", err),
                            )
                            .await;
                        return;
                    }

                    Ok(file) => {
                        proj.files.insert(uri.clone(), RwLock::new(file));
                    }
                }
            }
        }
        {
            let proj = self.m.proj.read().await;
            let file_lock = proj.files.get(&uri).unwrap();
            let mut file = file_lock.write().await;

            for change in params.content_changes {
                match change.range {
                    None => {
                        file.content = Rope::from_str(&change.text);
                    }
                    Some(range) => {
                        let start = file.content.line_to_char(range.start.line as usize)
                            + range.start.character as usize;
                        let end = file.content.line_to_char(range.end.line as usize)
                            + range.end.character as usize;

                        file.content.remove(start..end);
                        file.content.insert(start, &change.text);
                    }
                }
            }
            file.content_changed = true;
        }

        self.file_changed(uri).await;
    }
}

impl PowerBuilderLS {
    pub(super) async fn get_file(&self, uri: &Url) -> jsonrpc::Result<()> {
        if let Err(err) = self
            .m
            .proj
            .write()
            .await
            .add_file(&uri, linter::LintProgress::Complete)
            .await
        {
            self.client
                .log_message(
                    MessageType::ERROR,
                    format!("Failed to Lint File: {:?}", err),
                )
                .await;
            return Err(jsonrpc::Error::internal_error());
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct PowerBuilderLSCreator {
    pub(super) proj: Arc<RwLock<linter::Project>>,
    pub(super) change_timeouts: Mutex<HashMap<Url, oneshot::Sender<()>>>,
}

impl PowerBuilderLSCreator {
    pub async fn new() -> anyhow::Result<Self> {
        let proj = Arc::new(RwLock::new(linter::Project::new().await?));

        Ok(Self {
            proj,
            change_timeouts: Mutex::new(HashMap::new()),
        })
    }

    pub fn create(self, client: Client) -> PowerBuilderLS {
        PowerBuilderLS { client, m: self }
    }
}
