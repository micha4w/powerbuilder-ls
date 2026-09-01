use std::{
    collections::{hash_map, HashMap, HashSet},
    mem::replace,
    sync::Arc,
    time::Duration,
};

use super::ls_context::LSContext;

use crate::{
    builder::FileMeta,
    resolver::{FileAnnotations, Resolver},
    solution::{self, Solution},
    types::replace_with,
};
use async_lock::{RwLock, RwLockUpgradableReadGuard, RwLockWriteGuard};
use ropey::Rope;
use self_cell::self_cell;
use tokio::sync::oneshot;

use tower_lsp::{jsonrpc, lsp_types::*, Client};

macro_rules! lock_inner_if_initialized {
    ($self:ident, $inner:ident) => {
        let mut lock = $self.m.write().await;
        #[allow(unused_mut)]
        let Some(mut $inner) = lock.as_mut() else {
            $self
                .client
                .log_message(MessageType::ERROR, "Server not initialized")
                .await;
            return;
        };
    };
}
pub(super) use lock_inner_if_initialized;
use tracing::{debug, info_span, warn};

impl FileMeta {
    pub fn apply_pending_changes(&mut self, all_changes: Vec<Vec<TextDocumentContentChangeEvent>>) {
        debug!(uri=%self.uri, changes=all_changes.len(), "applying pending changes");
        for changes in all_changes {
            for change in changes {
                match change.range {
                    None => {
                        self.content = Rope::from_str(&change.text);
                    }
                    Some(range) => {
                        let start = self.content.line_to_char(range.start.line as usize)
                            + range.start.character as usize;
                        let end = self.content.line_to_char(range.end.line as usize)
                            + range.end.character as usize;

                        self.content.remove(start..end);
                        self.content.insert(start, &change.text);
                    }
                }
            }
        }
    }
}

pub struct OpenedFile {}

#[derive(Default)]
pub struct SolutionDependent<'sol> {
    pub annotations: HashMap<Arc<Url>, FileAnnotations<'sol>>,
}

self_cell!(
    pub struct SolutionCell {
        owner: solution::Solution,

        #[covariant]
        dependent: SolutionDependent,
    }
);

pub struct PowerBuilderLSInner {
    pub sol: SolutionCell,
    pub opened_files: HashMap<Arc<Url>, OpenedFile>,
    pub change_timeout: Option<oneshot::Sender<()>>,
    pub pending_changes: HashMap<Arc<Url>, Vec<Vec<TextDocumentContentChangeEvent>>>, // vec of vecs so we dont need to clone
}

impl PowerBuilderLSInner {}

pub struct PowerBuilderLS {
    pub client: Client,
    pub(super) m: Arc<RwLock<Option<PowerBuilderLSInner>>>,
}

impl PowerBuilderLS {
    pub fn new(client: Client) -> PowerBuilderLS {
        PowerBuilderLS {
            client,
            m: Arc::new(RwLock::new(None)),
        }
    }

    pub async fn rebuild_solution(
        &self,
        inner: &mut PowerBuilderLSInner,
        pre_load_files: impl FnOnce(&mut solution::Solution),
    ) {
        let mut errs = Vec::new();
        info_span!("ls.rebuild").in_scope(|| {
            inner.change_timeout = None;
            let pending_changes = replace(&mut inner.pending_changes, HashMap::new());

            replace_with(&mut inner.sol, |cell| {
                let mut sol = cell.into_owner();

                pre_load_files(&mut sol);

                (sol, errs) = Solution::rebuilt(
                    sol,
                    pending_changes
                        .into_iter()
                        .map(|(url, changes)| {
                            (url, |meta: &mut FileMeta| {
                                meta.apply_pending_changes(changes);
                            })
                        })
                        .collect(),
                );

                for (url, _) in &inner.opened_files {
                    debug!(%url, "opening previously opened file");
                    sol.open_file(url);
                }

                SolutionCell::new(sol, |sol| {
                    let mut dep = SolutionDependent::default();

                    for (url, _) in &inner.opened_files {
                        let Some(file) = sol.files.get(url) else {
                            warn!(%url, "opened file not found in solution");
                            continue;
                        };

                        debug!(%url, "resolving previously opened file");
                        dep.annotations
                            .insert(url.clone(), Resolver::resolve_file(sol, file));
                    }

                    dep
                })
            });
        });

        for err in errs {
            self.client.log_message(MessageType::ERROR, err).await;
        }

        #[cfg(feature = "diagnostics")]
        self.diagnostics_post_rebuild(inner).await;
    }

    pub async fn with_solution<T>(
        &self,
        f: impl FnOnce(&SolutionCell) -> jsonrpc::Result<T>,
    ) -> jsonrpc::Result<T> {
        let inner_rd;
        let inner_upgd = self.m.upgradable_read().await;
        let Some(inner) = inner_upgd.as_ref() else {
            self.client
                .log_message(MessageType::ERROR, "Server not initialized")
                .await;
            return Err(jsonrpc::Error::invalid_request());
        };

        if inner.change_timeout.is_some() {
            let mut inner_wr = RwLockUpgradableReadGuard::upgrade(inner_upgd).await;
            self.rebuild_solution(inner_wr.as_mut().unwrap(), |_| {})
                .await;
            inner_rd = RwLockWriteGuard::downgrade(inner_wr);
        } else {
            inner_rd = RwLockUpgradableReadGuard::downgrade(inner_upgd);
        }

        f(&inner_rd.as_ref().unwrap().sol)
    }

    pub async fn with_context<T>(
        &self,
        document_position: TextDocumentPositionParams,
        f: impl FnOnce(&LSContext<'_>) -> jsonrpc::Result<Option<T>>,
    ) -> jsonrpc::Result<Option<T>> {
        let uri = document_position.text_document.uri;
        let pos = document_position.position.into();

        self.with_solution(|cell| {
            cell.with_dependent(|sol, dep| {
                let Some(ctx) = LSContext::new(sol, dep, &uri, &pos) else {
                    return Ok(None);
                };
                f(&ctx)
            })
        })
        .await
    }
}
