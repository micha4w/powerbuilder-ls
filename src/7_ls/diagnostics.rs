use std::collections::HashMap;

use tower_lsp::lsp_types;
use tracing::{info, warn};

use super::ls::{PowerBuilderLS, PowerBuilderLSInner};
use crate::{linter::Linter, solution::SolutionType, types::*};

impl PowerBuilderLS {
    pub(super) fn diagnostics_capabilities(&self, _: &mut lsp_types::ServerCapabilities) {}

    pub(super) async fn diagnostics_post_rebuild(&self, inner: &PowerBuilderLSInner) {
        let diagnostics = inner.sol.with_dependent(|sol, _| {
            let SolutionType::FailedPbSln(errors) = &sol.solution_type else {
                return None;
            };

            let mut diagnostics = errors
                .iter()
                .map(|(file_id, err)| {
                    (
                        file_id.uri().clone(),
                        err.iter().cloned().map(Into::into).collect(),
                    )
                })
                .collect::<HashMap<_, _>>();

            for (file_id, _) in &inner.opened_files {
                if !sol.files.contains_key(file_id) {
                    continue;
                }

                let mut file_diags = Vec::new();
                file_diags.push(
                    Diagnostic {
                        range: Range::all(file_id.clone()),
                        severity: Severity::Error,
                        message: "Failed to parse solution, check your *.pbsln".to_string(),
                    }
                    .into(),
                );

                diagnostics.insert(file_id.uri().clone(), file_diags);
            }

            Some(diagnostics)
        });
        if let Some(diagnostics) = diagnostics {
            info!(
                "sending failed solution diagnostics for {} files",
                diagnostics.len()
            );
            for (uri, items) in diagnostics {
                self.client.publish_diagnostics(uri, items, None).await;
            }
            return;
        }

        for (file_id, _) in &inner.opened_files {
            match inner.build_file_diagnostics(&file_id) {
                Ok(Some(items)) => {
                    info!("sending diagnostics");
                    self.client
                        .publish_diagnostics(file_id.uri().clone(), items, None)
                        .await;
                }
                Ok(None) => {}
                Err(err) => {
                    self.client
                        .log_message(
                            lsp_types::MessageType::ERROR,
                            format!("Failed to get File diagnostics: {:?}", err),
                        )
                        .await
                }
            }
        }
    }
}

impl PowerBuilderLSInner {
    fn build_file_diagnostics(
        &self,
        file_id: &FileID,
    ) -> anyhow::Result<Option<Vec<lsp_types::Diagnostic>>> {
        self.sol.with_dependent(|sol, dep| {
            let Some(file) = sol.files.get(file_id) else {
                warn!(%file_id, "file not found when sending diagnostics");

                if let SolutionType::PbSln {
                    solution, projects, ..
                } = &sol.solution_type
                {
                    if solution == file_id || projects.contains_key(file_id) {
                        return Ok(None);
                    }
                }

                return Ok(Some(vec![Diagnostic {
                    range: Range::all(file_id.clone()),
                    severity: Severity::Info,
                    message: "File is not part of the current project".to_string(),
                }
                .into()]));
            };
            let Some(annotations) = dep.annotations.get(file_id) else {
                warn!(%file_id, "annotations not found when sending diagnostics");
                return Ok(None);
            };

            let mut linter = Linter::new(sol, file, annotations);
            linter.lint_file();

            let items = Iterator::chain(
                file.borrow_owner().meta.parse_diagnostics.iter(),
                linter.diagnostics.borrow().iter(),
            )
            .cloned()
            .map(Into::into)
            .collect();

            Ok(Some(items))
        })
    }
}
