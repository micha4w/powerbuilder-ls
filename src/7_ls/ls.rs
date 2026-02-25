use std::{
    collections::{hash_map, HashMap, HashSet},
    mem::replace,
    sync::Arc,
    time::Duration,
};

use super::ls_context::LSContext;

use crate::{
    builder::{self, File, FileMeta},
    project::{self},
    resolver::{self, FileAnnotations, ResolvedType, Resolver},
    types::replace_with,
};
use async_lock::{RwLock, RwLockUpgradableReadGuard, RwLockWriteGuard};
use parking_lot::lock_api::RawRwLockDowngrade;
use ropey::Rope;
use self_cell::self_cell;
use tokio::{sync::oneshot, time::timeout};

use tower_lsp::{jsonrpc, lsp_types::*, Client, LanguageServer};

// macro_rules! create_context {
//     ($ctx:ident, $ls:expr, $uri:expr, $pos:expr) => {
//         let $ctx = match LSContext::new($ls, $uri, $pos) {
//             Ok(ctx) => ctx,
//             Err(err) => {
//                 $ls.client
//                     .log_message(
//                         MessageType::ERROR,
//                         format!("Failed to create context: {}", err),
//                     )
//                     .await;
//                 return Ok(None);
//             }
//         };
//         let Some($ctx) = $ctx.alloc() else {
//             return Ok(None);
//         };
//     };
// }

impl project::Project {
    pub fn apply_pending_changes(
        &mut self,
        pending_changes: HashMap<Url, Vec<Vec<TextDocumentContentChangeEvent>>>,
    ) {
        for (url, all_changes) in pending_changes {
            let Some(file) = self.files.get_mut(&url) else {
                continue;
            };
            let meta = file.meta_mut();

            for changes in all_changes {
                for change in changes {
                    match change.range {
                        None => {
                            meta.content = Rope::from_str(&change.text);
                        }
                        Some(range) => {
                            let start = meta.content.line_to_char(range.start.line as usize)
                                + range.start.character as usize;
                            let end = meta.content.line_to_char(range.end.line as usize)
                                + range.end.character as usize;

                            meta.content.remove(start..end);
                            meta.content.insert(start, &change.text);
                        }
                    }
                }
            }

            file.reparse();
        }
    }
}

pub struct OpenedFile {}

#[derive(Default)]
pub struct ProjectDependent<'proj> {
    pub annotations: HashMap<Url, FileAnnotations<'proj>>,
}

self_cell!(
    pub struct ProjectCell {
        owner: project::Project,

        #[covariant]
        dependent: ProjectDependent,
    }
);

pub struct PowerBuilderLSInner {
    pub proj: ProjectCell,
    pub opened_files: HashMap<Url, OpenedFile>,
    pub change_timeout: Option<oneshot::Sender<()>>,
    pub pending_changes: HashMap<Url, Vec<Vec<TextDocumentContentChangeEvent>>>, // vec of vecs so we dont need to clone
}

impl PowerBuilderLSInner {}

pub struct PowerBuilderLS {
    pub client: Client,
    pub(super) m: Arc<RwLock<PowerBuilderLSInner>>,
}

impl PowerBuilderLS {
    pub fn new(client: Client) -> PowerBuilderLS {
        let proj = ProjectCell::new(project::Project::new(), |_| Default::default());

        PowerBuilderLS {
            client,
            m: Arc::new(RwLock::new(PowerBuilderLSInner {
                proj,
                opened_files: HashMap::new(),
                change_timeout: None,
                pending_changes: HashMap::new(),
            })),
        }
    }

    pub async fn with_context<T>(
        &self,
        document_position: TextDocumentPositionParams,
        f: impl FnOnce(&LSContext<'_>) -> jsonrpc::Result<Option<T>>,
    ) -> jsonrpc::Result<Option<T>> {
        let uri = document_position.text_document.uri;
        let pos = document_position.position.into();

        let inner_rd;
        let inner_upgd = self.m.upgradable_read().await;
        if inner_upgd.change_timeout.is_some() {
            let mut inner_wr = RwLockUpgradableReadGuard::upgrade(inner_upgd).await;
            self.rebuild_project(&mut inner_wr, |_| {}).await;
            inner_rd = RwLockWriteGuard::downgrade(inner_wr);
        } else {
            inner_rd = RwLockUpgradableReadGuard::downgrade(inner_upgd);
        }

        inner_rd.proj.with_dependent(|proj, dep| {
            let Some(ctx) = LSContext::new(proj, dep, &uri, &pos) else {
                return Ok(None);
            };
            f(&ctx)
        })
    }

    pub async fn rebuild_project(
        &self,
        inner: &mut PowerBuilderLSInner,
        pre_load_files: impl FnOnce(&mut project::Project),
    ) {
        inner.change_timeout = None;
        let pending_changes = replace(&mut inner.pending_changes, HashMap::new());

        let mut errs = Vec::new();
        replace_with(&mut inner.proj, |cell| {
            let mut proj = cell.into_owner();
            proj.apply_pending_changes(pending_changes);

            pre_load_files(&mut proj);

            for (url, _) in &inner.opened_files {
                // TODO(errors): show to user
                if let Err(err) = proj.load_file_for_editing(url) {
                    errs.push((url.clone(), err));
                }
            }

            ProjectCell::new(proj, |proj| {
                let mut dep = ProjectDependent::default();

                for (url, _) in &inner.opened_files {
                    let Some(File::Built(file)) = proj.files.get(url) else {
                        continue;
                    };

                    dep.annotations
                        .insert(url.clone(), Resolver::resolve_file(proj, file));
                }

                dep
            })
        });

        for (url, err) in errs {
            self.client
                .log_message(
                    MessageType::ERROR,
                    format!("Failed to load file: {}: {}", url, err),
                )
                .await;
        }

        self.diagnostics_post_rebuild(inner).await;
    }
}

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

        if let Some(root) = params.root_uri {
            let mut res = None;
            let mut inner = self.m.write().await;
            self.rebuild_project(&mut inner, |proj| {
                res = proj.add_default_project(&root).err()
            })
            .await;
            if let Some(err) = res {
                // TODO(errors): abort?
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!(
                            "Failed to open Default Project of Root URI ({}) {:?}",
                            root, err
                        ),
                    )
                    .await;
            }
        }

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
            if !Arc::ptr_eq(&r.uri, &project::BUILTIN_URL) {
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
        let uri = params.text_document.uri;

        let mut inner = self.m.write().await;
        if !inner.opened_files.contains_key(&uri) {
            inner.opened_files.insert(uri.clone(), OpenedFile {});
        }

        self.rebuild_project(&mut inner, |proj| {
            proj.files.insert(
                uri.clone(),
                File::new_from_content(uri.clone(), &params.text_document.text),
            );
        })
        .await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.m.write().await.opened_files.remove(&uri);
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        let mut inner = self.m.write().await;

        inner.pending_changes.remove(&uri);
        self.rebuild_project(&mut inner, |proj| {
            proj.files.remove(&uri);

            for (_, file) in &mut proj.files {
                file.rebuild();
            }
        })
        .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;

        let (change_tx, change_rx) = oneshot::channel::<()>();
        {
            let mut m = self.m.write().await;
            if !m.opened_files.contains_key(&uri) {
                return;
            }

            m.change_timeout = Some(change_tx);
            m.pending_changes
                .entry(uri.clone())
                .or_default()
                .push(params.content_changes);
        }

        if let Ok(_tx_drop_err) = timeout(Duration::from_millis(500), change_rx).await {
            return;
        }

        let mut inner = self.m.write().await;
        self.rebuild_project(&mut inner, |proj| {
            for (_, file) in &mut proj.files {
                file.rebuild();
            }
        })
        .await;
    }
}
