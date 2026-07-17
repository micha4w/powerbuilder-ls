use tower_lsp::lsp_types;

use crate::{builder::File, linter::Linter, types::*};
use super::ls::{PowerBuilderLS, PowerBuilderLSInner};

impl PowerBuilderLS {
    pub(super) fn diagnostics_capabilities(&self, _: &mut lsp_types::ServerCapabilities) {}

    pub(super) async fn diagnostics_post_rebuild(&self, inner: &PowerBuilderLSInner) {
        for (uri, _) in &inner.opened_files {
            match inner.get_file_diagnostics(&uri) {
                Ok(Some(items)) => {
                    eprintln!("Sending diagnostics");
                    self.client
                        .publish_diagnostics(uri.clone(), items, None)
                        .await;
                }
                Ok(None) => {
                    eprintln!("Skipping send diagnostics because they didn't change");
                }
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
    fn get_file_diagnostics(
        &self,
        uri: &Url,
    ) -> anyhow::Result<Option<Vec<lsp_types::Diagnostic>>> {
        self.proj.with_dependent(|proj, dep| {
            let Some(File::Built(file)) = proj.files.get(uri) else {
                eprintln!("[WARN] File not found or not built");
                return Ok(None);
            };
            let Some(annotations) = dep.annotations.get(uri) else {
                eprintln!("[WARN] Annotations not found");
                return Ok(None);
            };

            let mut linter = Linter::new(proj, file, annotations);
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
