use std::{path::PathBuf, sync::Arc};

use crate::linter;
use tokio::sync::RwLock;

use tower_lsp::{jsonrpc, lsp_types::*, Client, LanguageServer};

#[derive(Debug)]
pub struct PowerBuilderLS {
    pub client: Client,
    pub(crate) m: PowerBuilderLSCreator,
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
        self.send_diagnostics(params.text_document.uri).await;
    }

    // async fn did_change(&self, params: DidChangeTextDocumentParams) {
    //     self.m.proj.write().await.ver =  params.text_document.version;
    // }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.send_diagnostics(params.text_document.uri).await;
    }
}

impl PowerBuilderLS {
    pub(crate) async fn get_file(&self, url: &Url) -> jsonrpc::Result<PathBuf> {
        let path = match std::path::absolute(url.path()) {
            Ok(path) => path,
            Err(err) => {
                return Err(jsonrpc::Error::invalid_params(format!(
                    "Invalid path: {}",
                    err
                )))
            }
        };

        if let Err(err) = self
            .m
            .proj
            .write()
            .await
            .add_file(&path, linter::LintProgress::Complete)
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

        Ok(path)
    }

    // async fn file_change_watcher(&mut self) {
    //     while let Some(i) = self.m.watcher_rx.recv().await {
    //         for path in i.paths {}
    //     }
    // }
}

#[derive(Debug)]
pub struct PowerBuilderLSCreator {
    pub(crate) proj: Arc<RwLock<linter::Project>>,
    // watcher: Arc<Mutex<notify::INotifyWatcher>>,
    // watcher_rx: mpsc::Receiver<notify::Event>,
}

impl PowerBuilderLSCreator {
    pub async fn new() -> anyhow::Result<Self> {
        // let (tx, rx) = mpsc::channel::<notify::Event>(1);
        // let watcher =
        //     notify::recommended_watcher(move |res: Result<notify::Event, notify::Error>| {
        //         let err = match res {
        //             Ok(event) => tx.blocking_send(event).map_err(anyhow::Error::msg),
        //             Err(err) => Err(anyhow::Error::msg(err)),
        //         };

        //         eprintln!("File watcher failed to watch: {:?}", err);
        //     })?;

        let proj = Arc::new(RwLock::new(linter::Project::new().await?));

        Ok(Self {
            proj,
            // watcher: Arc::new(Mutex::new(watcher)),
            // watcher_rx: rx,
        })
    }

    pub fn create(self, client: Client) -> PowerBuilderLS {
        PowerBuilderLS { client, m: self }
    }
}
