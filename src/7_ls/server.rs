use std::{collections::HashMap, time::Duration};

use super::ls::{PowerBuilderLS, PowerBuilderLSInner, SolutionCell};

use crate::{
    ls::ls::{lock_inner_if_initialized, OpenedFile},
    solution::{self},
    types::*,
};
use tokio::{sync::oneshot, time::timeout};

#[cfg(feature = "goto-type")]
use tower_lsp::lsp_types::request::{GotoTypeDefinitionParams, GotoTypeDefinitionResponse};
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
        #[cfg(feature = "goto-type")]
        self.goto_type_capabilities(&mut capabilities);
        #[cfg(feature = "diagnostics")]
        self.diagnostics_capabilities(&mut capabilities);

        let sol = solution::Solution::new(params.root_uri.and_then(FilePath::new));

        let mut lock = self.m.write().await;
        *lock = Some(PowerBuilderLSInner {
            sol: SolutionCell::new(sol, |_| Default::default()),
            opened_files: HashMap::new(),
            change_timeout: None,
            pending_changes: HashMap::new(),
            solution_changed: false,
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
            if !FileID::ptr_eq(&r.file_id, &solution::BUILTIN_FILE_ID) {
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: r.file_id.uri().clone(),
                    range: r.into(),
                })));
            }
        }
        Ok(None)
    }

    #[cfg(feature = "goto-type")]
    async fn goto_type_definition(
        &self,
        params: GotoTypeDefinitionParams,
    ) -> jsonrpc::Result<Option<GotoTypeDefinitionResponse>> {
        let range = self
            .with_context(params.text_document_position_params, |ctx| {
                self.goto_type_impl(&ctx)
            })
            .await?;

        if let Some(r) = range {
            if !FileID::ptr_eq(&r.file_id, &solution::BUILTIN_FILE_ID) {
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: r.file_id.uri().clone(),
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

    // #[cfg(feature = "completion")]
    // async fn completion_resolve(&self, params: CompletionItem) -> jsonrpc::Result<CompletionItem> {
    //     let items = self
    //         .with_context(params.text_document_position, |ctx| {
    //             self.completion_impl(ctx).map(Some)
    //         })
    //         .await?;

    //     Ok(items.map(CompletionResponse::Array))
    // }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let Some(file_id) = FileID::new(params.text_document.uri) else {
            return;
        };
        lock_inner_if_initialized!(self, inner);

        if !inner.opened_files.contains_key(&file_id) {
            inner.opened_files.insert(file_id, OpenedFile {});
        }

        self.rebuild_solution(&mut inner, |_| {}).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let Some(file_id) = FileID::new(params.text_document.uri) else {
            return;
        };
        lock_inner_if_initialized!(self, inner);

        inner.opened_files.remove(&file_id);
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let Some(file_id) = FileID::new(params.text_document.uri) else {
            return;
        };
        lock_inner_if_initialized!(self, inner);

        if let Some((_, "pbproj" | "pbsln")) = file_id.uri().path().rsplit_once(".") {
            inner.solution_changed = true;
        }

        self.rebuild_solution(&mut inner, |sol| {
            sol.files.remove(&file_id);
        })
        .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let Some(file_id) = FileID::new(params.text_document.uri) else {
            return;
        };

        let (change_tx, change_rx) = oneshot::channel::<()>();
        {
            lock_inner_if_initialized!(self, inner);
            if !inner.opened_files.contains_key(&file_id) {
                return;
            }

            inner.change_timeout = Some(change_tx);
            inner
                .pending_changes
                .entry(file_id)
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
