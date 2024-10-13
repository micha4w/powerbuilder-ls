mod ls;
mod parser;

use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use ls::ls_types::{self, LintProgress, Project};
use notify::Watcher;
use tokio::sync::mpsc;
use tokio::sync::Mutex;
use tokio::sync::RwLock;

use parser::parser_types;

use tower_lsp::jsonrpc;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct PowerBuilderLSCreator {
    proj: Arc<RwLock<Project>>,
    watcher: Arc<Mutex<notify::INotifyWatcher>>,
    watcher_rx: mpsc::Receiver<notify::Event>,
}

impl PowerBuilderLSCreator {
    fn new(system_files_root: PathBuf) -> anyhow::Result<Self> {
        let (tx, rx) = mpsc::channel::<notify::Event>(1);
        let watcher =
            notify::recommended_watcher(move |res: Result<notify::Event, notify::Error>| {
                let err = match res {
                    Ok(event) => tx.blocking_send(event).map_err(anyhow::Error::msg),
                    Err(err) => Err(anyhow::Error::msg(err)),
                };

                eprintln!("File watcher failed to watch: {:?}", err);
            })?;

        let proj = Arc::new(RwLock::new(Project::new(
            system_files_root.join("enums.pb"),
            system_files_root.join("classes.pb"),
            system_files_root.join("system_functions.pb"),
        )?));

        Ok(Self {
            proj,
            watcher: Arc::new(Mutex::new(watcher)),
            watcher_rx: rx,
        })
    }
}

#[derive(Debug)]
struct PowerBuilderLS {
    client: Client,
    m: PowerBuilderLSCreator,
}

impl PowerBuilderLS {
    fn new(client: Client, creator: PowerBuilderLSCreator) -> Self {
        Self { client, m: creator }
    }

    async fn file_change_watcher(&mut self) {
        while let Some(i) = self.m.watcher_rx.recv().await {
            for path in i.paths {}
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for PowerBuilderLS {
    async fn initialize(&self, _: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: None,
                        inter_file_dependencies: true,
                        workspace_diagnostics: false,
                        work_done_progress_options: WorkDoneProgressOptions {
                            work_done_progress: None,
                        },
                    },
                )),
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> jsonrpc::Result<DocumentDiagnosticReportResult> {
        self.client
            .log_message(
                MessageType::INFO,
                format!("file diagnostics! {:?}", params.text_document),
            )
            .await;

        let path: PathBuf = params.text_document.uri.path().into();

        // self.m
        //     .watcher
        //     .watch(&path, notify::RecursiveMode::NonRecursive)
        //     .map_err(|err| {
        //         let mut json_err = jsonrpc::Error::internal_error();
        //         json_err.message = err.to_string().into();
        //         json_err
        //     })?;
        self.m.proj.write().await.files.remove(&path);
        if let Err(err) = ls::add_file(self.m.proj.clone(), &path, LintProgress::Complete).await {
            let mut json_err = jsonrpc::Error::internal_error();
            json_err.message = err.to_string().into();
            return Err(json_err);
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
                        parser_types::Severity::Error => DiagnosticSeverity::ERROR,
                        parser_types::Severity::Warning => DiagnosticSeverity::WARNING,
                        parser_types::Severity::Info => DiagnosticSeverity::INFORMATION,
                        parser_types::Severity::Hint => DiagnosticSeverity::HINT,
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

        Ok(DocumentDiagnosticReportResult::Report(
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: None,
                    items,
                },
            }),
        ))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(
                MessageType::INFO,
                format!("file opened! {:?}", params.text_document),
            )
            .await;

        // self.on_change(TextDocumentItem {
        //     uri: params.text_document.uri,
        //     text: params.text_document.text,
        //     version: params.text_document.version,
        // })
        // .await
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let creator =
        PowerBuilderLSCreator::new("/home/micha4w/Code/Rust/powerbuilder-ls/system".into())?;

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(move |client| PowerBuilderLS::new(client, creator));
    Server::new(stdin, stdout, socket).serve(service).await;

    // lsp::add_file(
    //     proj_arc.clone(),
    //     "res/test.sru".into(),
    //     lsp_types::LintProgress::Complete,
    // )
    // .await?;

    // ls::add_file(
    //     proj_arc.clone(),
    //     &"res/pfc_w_find.srw".into(),
    //     ls_types::LintProgress::Complete,
    // )
    // .await?;

    // let path = "res/test.sru";
    // let mut tokens = FileTokenizer::open(path)?;
    // tokens.skip_headers();
    // // TODO stream the tokens

    // let mut parser = Parser::new(&mut tokens);
    // parser.parse_tokens();

    // let buf = Bytes::from_iter(std::fs::read("system/classes.pb")?.iter().cloned());
    // let classes = powerbuilder::Classes::decode(buf)?;

    // let file = File::open("system/system_functions.json")?;
    // let reader = BufReader::new(file);
    // let u : powerbuilder::Functions = serde_json::from_reader(reader)?;

    // let v = u.encode_to_vec();
    // std::fs::write("system/system_functions.pb", v)?;

    // let buf = Bytes::from_iter(std::fs::read("system/enums2.pb")?.iter().cloned());
    // let classes = powerbuilder::Enums::decode(buf)?;

    // let buf = std::fs::read("system/enums2.pb")?;
    // let mut cursor: Cursor<Vec<u8>> = Cursor::new(buf);
    // let enums = output::Enums::decode_length_delimited(&mut cursor)?;
    // lsp.add_file("res/u_filehandler.sru".into(), LintState::Complete)?;
    // lsp.add_file("res/pfc_w_find.srw".into(), LintState::Complete)?;

    // for token in tokens {
    //     println!("{:?} {:?} {}:{}-{}:{}", token.token_type, token.content, token.range.start.line, token.range.start.column, token.range.end.line, token.range.end.column);
    // }

    Ok(())
}
