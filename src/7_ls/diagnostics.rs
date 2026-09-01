use tower_lsp::lsp_types;
use tracing::{info, warn};

use super::ls::{PowerBuilderLS, PowerBuilderLSInner};
use crate::{linter::Linter, types::*};

impl PowerBuilderLS {
    pub(super) fn diagnostics_capabilities(&self, _: &mut lsp_types::ServerCapabilities) {}

    pub(super) async fn diagnostics_post_rebuild(&self, inner: &PowerBuilderLSInner) {
        for (uri, _) in &inner.opened_files {
            match inner.build_file_diagnostics(&uri) {
                Ok(Some(items)) => {
                    info!("sending diagnostics");
                    self.client
                        .publish_diagnostics((**uri).clone(), items, None)
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
        uri: &Url,
    ) -> anyhow::Result<Option<Vec<lsp_types::Diagnostic>>> {
        self.sol.with_dependent(|sol, dep| {
            let Some(file) = sol.files.get(uri) else {
                warn!(%uri, "file not found when sending diagnostics");
                return Ok(None);
            };
            let Some(annotations) = dep.annotations.get(uri) else {
                warn!(%uri, "annotations not found when sending diagnostics");
                return Ok(None);
            };

            let mut linter = Linter::new(sol, file, annotations);
            linter.lint_file();

            let items = Iterator::chain(
                file.borrow_owner().meta.parse_diagnostics.iter(),
                linter.diagnostics.borrow().iter(),
            )
            .map(|d| lsp_types::Diagnostic {
                range: lsp_types::Range::new(
                    lsp_types::Position::new(d.range.start.line, d.range.start.column),
                    lsp_types::Position::new(d.range.end.line, d.range.end.column),
                ),
                severity: Some(match d.severity {
                    Severity::Error => lsp_types::DiagnosticSeverity::ERROR,
                    Severity::Warning => lsp_types::DiagnosticSeverity::WARNING,
                    Severity::Info => lsp_types::DiagnosticSeverity::INFORMATION,
                    Severity::Hint => lsp_types::DiagnosticSeverity::HINT,
                }),
                message: d.message.clone(),
                ..Default::default()
            })
            .collect();

            Ok(Some(items))
        })
    }
}
