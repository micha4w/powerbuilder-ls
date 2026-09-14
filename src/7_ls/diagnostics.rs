use std::u32;

use tower_lsp::lsp_types;
use tracing::{info, warn};

use super::ls::{PowerBuilderLS, PowerBuilderLSInner};
use crate::{linter::Linter, solution::SolutionType, types::*};

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
    fn to_lsp_diagnostic(d: &Diagnostic) -> lsp_types::Diagnostic {
        lsp_types::Diagnostic {
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
        }
    }

    fn build_file_diagnostics(
        &self,
        uri: &Url,
    ) -> anyhow::Result<Option<Vec<lsp_types::Diagnostic>>> {
        self.sol.with_dependent(|sol, dep| {
            if let SolutionType::FailedPbSln(errors) = &sol.solution_type {
                let mut diagnostics = Vec::new();
                for err in errors {
                    if *err.range.uri == *uri {
                        diagnostics.push(Self::to_lsp_diagnostic(err));
                    }
                }

                if diagnostics.is_empty() {
                    diagnostics.push(lsp_types::Diagnostic {
                        range: lsp_types::Range::new(
                            lsp_types::Position::default(),
                            lsp_types::Position {
                                line: u32::MAX,
                                character: u32::MAX,
                            },
                        ),
                        severity: Some(lsp_types::DiagnosticSeverity::ERROR),
                        message: "Failed to parse solution, check your *.pbsln".to_string(),
                        ..Default::default()
                    });
                }

                return Ok(Some(diagnostics));
            }

            let Some(file) = sol.files.get(uri) else {
                warn!(%uri, "file not found when sending diagnostics");
                return Ok(Some(vec![lsp_types::Diagnostic {
                    range: lsp_types::Range::new(
                        lsp_types::Position::default(),
                        lsp_types::Position {
                            line: u32::MAX,
                            character: u32::MAX,
                        },
                    ),
                    severity: Some(lsp_types::DiagnosticSeverity::INFORMATION),
                    message: "File is not part of the current project".to_string(),
                    ..Default::default()
                }]));
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
            .map(Self::to_lsp_diagnostic)
            .collect();

            Ok(Some(items))
        })
    }
}
