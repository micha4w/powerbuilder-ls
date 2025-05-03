use std::{path::PathBuf, time::Duration};

use tokio::{sync::oneshot, time::timeout};
use tower_lsp::{jsonrpc, lsp_types::*};

use crate::{
    linter::{self, LintProgress},
    parser,
};

use super::ls::PowerBuilderLS;

impl PowerBuilderLS {
    pub(super) fn diagnostics_capabilities(&self, _caps: &mut ServerCapabilities) {
        // caps.diagnostic_provider = Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
        //     identifier: None,
        //     inter_file_dependencies: true,
        //     workspace_diagnostics: false,
        //     work_done_progress_options: WorkDoneProgressOptions {
        //         work_done_progress: None,
        //     },
        // }));
    }

    pub(super) async fn file_changed_on_disk(&self, uri: Url) {
        self.m.change_timeouts.lock().await.remove(&uri);
        self.m.proj.write().await.files.remove(&uri);
        self.send_diagnostics(uri).await;
    }

    pub(super) async fn file_changed(&self, uri: Url) {
        let (tx, rx) = oneshot::channel::<()>();

        eprint!(".");
        self.m.change_timeouts.lock().await.insert(uri.clone(), tx);
        if timeout(Duration::from_millis(500), rx).await.is_err() {
            eprintln!("Stopped typing");
            self.send_diagnostics(uri).await;
        }
    }

    pub(super) async fn send_diagnostics(&self, uri: Url) {
        match self.get_file_diagnostics(&uri).await {
            Ok(Some(items)) => self.client.publish_diagnostics(uri, items, None).await,
            Ok(None) => {}
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

    async fn get_file_diagnostics(&self, uri: &Url) -> jsonrpc::Result<Option<Vec<Diagnostic>>> {
        if let Some(file_lock) = self.m.proj.read().await.files.get(&uri) {
            let file = file_lock.read().await;
            if !file.content_changed && !file.diagnostics_changed {
                eprintln!("Not sending diagnostics because they did not changed");
                return Ok(None);
            }
        }

        if let Err(err) = self
            .m
            .proj
            .write()
            .await
            .add_file(&uri, linter::LintProgress::Complete)
            .await
        {
            self.client
                .log_message(
                    MessageType::ERROR,
                    format!("Failed to Lint File: {:?}", err),
                )
                .await;
            return Err(jsonrpc::Error::internal_error());
        };

        let proj = self.m.proj.read().await;
        let file_lock = proj.files.get(&uri).unwrap();
        file_lock.write().await.diagnostics_changed = false;

        let items = file_lock
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

        Ok(Some(items))
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
