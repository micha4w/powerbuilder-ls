use std::{collections::HashMap, sync::Arc, time::Duration};

use super::ls::{PowerBuilderLS, PowerBuilderLSInner, SolutionCell};

use crate::{
    ls::ls::{lock_inner_if_initialized, OpenedFile},
    solution::{self},
};
use tokio::{sync::oneshot, time::timeout};

use tower_lsp::{jsonrpc, lsp_types::*, LanguageServer};

#[tower_lsp::async_trait]
impl LanguageServer for PowerBuilderLS {
    async fn initialize(&self, params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        #[allow(unused_mut)]
        let mut capabilities = ServerCapabilities {
            workspace: Some(WorkspaceServerCapabilities {
                workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                    supported: Some(true),
                    // change_notifications: Some(OneOf::Left(true)),
                    ..Default::default()
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

        #[cfg(feature = "hover")]
        self.hover_capabilities(&mut capabilities);
        #[cfg(feature = "completion")]
        self.completion_capabilities(&mut capabilities);
        #[cfg(feature = "goto-definition")]
        self.goto_definition_capabilities(&mut capabilities);
        #[cfg(feature = "diagnostics")]
        self.diagnostics_capabilities(&mut capabilities);

        let Some(root) = params.root_uri else {
            return Err(jsonrpc::Error::invalid_params("Missing root URI"));
        };
        let sol = solution::Solution::new(&root).map_err(|err| {
            jsonrpc::Error::invalid_params(format!(
                "Failed to load solution from root URI ({}) {:?}",
                root, err
            ))
        })?;

        let mut lock = self.m.write().await;
        *lock = Some(PowerBuilderLSInner {
            sol: SolutionCell::new(sol, |_| Default::default()),
            opened_files: HashMap::new(),
            change_timeout: None,
            pending_changes: HashMap::new(),
        });
        let mut inner = lock.as_mut().unwrap();

        self.rebuild_solution(&mut inner, |_| {}).await;

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

    #[cfg(feature = "hover")]
    async fn hover(&self, params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        let Some((help, range)) = self
            .with_context(params.text_document_position_params, |ctx| {
                self.hover_impl(ctx)
            })
            .await?
        else {
            return Ok(None);
        };

        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: help,
            }),
            range: Some(range.into()),
        }))
    }

    #[cfg(feature = "goto-definition")]
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> jsonrpc::Result<Option<GotoDefinitionResponse>> {
        let range = self
            .with_context(params.text_document_position_params, |ctx| {
                self.goto_definition_impl(&ctx)
            })
            .await?;

        if let Some(r) = range {
            if !Arc::ptr_eq(&r.uri, &solution::BUILTIN_URL) {
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: (*r.uri).clone(),
                    range: r.into(),
                })));
            }
        }
        Ok(None)
    }

    #[cfg(feature = "completion")]
    async fn completion(
        &self,
        params: CompletionParams,
    ) -> jsonrpc::Result<Option<CompletionResponse>> {
        let items = self
            .with_context(params.text_document_position, |ctx| {
                self.completion_impl(ctx).map(Some)
            })
            .await?;

        Ok(items.map(CompletionResponse::Array))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = Arc::new(params.text_document.uri);

        lock_inner_if_initialized!(self, inner);

        if !inner.opened_files.contains_key(&uri) {
            inner.opened_files.insert(uri, OpenedFile {});
        }

        self.rebuild_solution(&mut inner, |_| {}).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        lock_inner_if_initialized!(self, inner);

        inner.opened_files.remove(&uri);
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        lock_inner_if_initialized!(self, inner);

        self.rebuild_solution(&mut inner, |sol| {
            sol.files.remove(&uri);
        })
        .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = Arc::new(params.text_document.uri);

        let (change_tx, change_rx) = oneshot::channel::<()>();
        {
            lock_inner_if_initialized!(self, inner);
            if !inner.opened_files.contains_key(&uri) {
                return;
            }

            inner.change_timeout = Some(change_tx);
            inner
                .pending_changes
                .entry(uri.clone())
                .or_default()
                .push(params.content_changes);
        }

        if let Ok(_tx_drop_err) = timeout(Duration::from_millis(500), change_rx).await {
            return;
        }

        lock_inner_if_initialized!(self, inner);
        self.rebuild_solution(&mut inner, |_| {}).await;
    }
}
