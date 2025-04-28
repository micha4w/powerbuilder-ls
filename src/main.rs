mod linter;
mod ls;
mod parser;
mod tokenizer;
mod types;

// use std::panic::PanicHookInfo;

// fn panic_hook(info: &PanicHookInfo) {
//     if let Err(err) = std::fs::write(
//         "/home/micha4w/Code/Rust/powerbuilder-ls/res/log.txt",
//         info.to_string(),
//     ) {
//         eprintln!("{}", err);
//     };
// }

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // std::panic::set_hook(Box::new(panic_hook));

    let creator = ls::PowerBuilderLSCreator::new().await?;

    // creator
    //     .proj
    //     .write()
    //     .await
    //     .add_file(&"res/test.sru".into(), linter::LintProgress::Complete)
    //     .await?;

    // for (x,y) in &creator.proj.read().await.files {
    //     for diagnostic in &y.read().await.diagnostics {
    //         println!("{:?}", diagnostic)
    //     }
    // }

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = tower_lsp::LspService::new(move |client| creator.create(client));
    tower_lsp::Server::new(stdin, stdout, socket)
        .serve(service)
        .await;

    // lsp::add_file(
    //     proj_arc.clone(),
    //     "res/test.sru".into(),
    //     lsp_types::LintProgress::Complete,
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
