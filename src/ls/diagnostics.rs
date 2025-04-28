use std::path::PathBuf;

use tower_lsp::{jsonrpc, lsp_types::*};

use crate::{linter, parser};

use super::ls::PowerBuilderLS;

impl PowerBuilderLS {
    pub(crate) fn diagnostics_capabilities(&self, _caps: &mut ServerCapabilities) {
        // caps.diagnostic_provider = Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
        //     identifier: None,
        //     inter_file_dependencies: true,
        //     workspace_diagnostics: false,
        //     work_done_progress_options: WorkDoneProgressOptions {
        //         work_done_progress: None,
        //     },
        // }));
    }

    pub(crate) async fn send_diagnostics(&self, uri: Url) {
        match self.get_file_diagnostics(uri.path().into()).await {
            Ok(items) => self.client.publish_diagnostics(uri, items, None).await,
            Err(err) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("Failed to get File diagnostics: {:?}", err),
                    )
                    .await
            }
        }
    }

    async fn get_file_diagnostics(&self, path: PathBuf) -> jsonrpc::Result<Vec<Diagnostic>> {
        self.m.proj.write().await.files.remove(&path);
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

        let items;
        if let Some(file) = self.m.proj.read().await.files.get(&path) {
            items = file
                .read()
                .await
                .diagnostics
                .iter()
                .map(|d| Diagnostic {
                    range: Range::new(
                        Position::new(d.range.start.line, d.range.start.column),
                        Position::new(d.range.end.line, d.range.end.column),
                    ),
                    severity: Some(match d.severity {
                        parser::Severity::Error => DiagnosticSeverity::ERROR,
                        parser::Severity::Warning => DiagnosticSeverity::WARNING,
                        parser::Severity::Info => DiagnosticSeverity::INFORMATION,
                        parser::Severity::Hint => DiagnosticSeverity::HINT,
                    }),
                    message: d.message.clone(),
                    ..Default::default()
                })
                .collect();
        } else {
            let mut json_err = jsonrpc::Error::internal_error();
            json_err.message = "File not found".into();
            return Err(json_err);
        }

        Ok(items)
    }

    // async fn diagnostic_impl(
    //     &self,
    //     params: DocumentDiagnosticParams,
    // ) -> jsonrpc::Result<DocumentDiagnosticReportResult> {
    //     self.client
    //         .log_message(
    //             MessageType::INFO,
    //             format!("file diagnostics! {:?}", params.text_document),
    //         )
    //         .await;

    //     let path: PathBuf = params.text_document.uri.path().into();
    //     let items = self.get_file_diagnostics(path).await?;
    //     Ok(DocumentDiagnosticReportResult::Report(
    //         DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
    //             related_documents: None,
    //             full_document_diagnostic_report: FullDocumentDiagnosticReport {
    //                 result_id: None,
    //                 items,
    //             },
    //         }),
    //     ))
    // }
}
