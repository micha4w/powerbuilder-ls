mod lsp;
mod parser;

use std::sync::Arc;

use lsp::lsp_types::{self, Project};
use tokio::sync::RwLock;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
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

    let proj_arc = Arc::new(RwLock::new(Project::new()));
    {
        let mut proj = proj_arc.write().await;
        proj.load_enums("system/enums.pb".into())?;
        proj.load_builtin_classes("system/classes.pb".into())?;
        proj.load_builtin_functions("system/system_functions.pb".into())?;
    }
    lsp::add_file(
        proj_arc.clone(),
        "res/test.sru".into(),
        lsp_types::LintProgress::Complete,
    )
    .await?;
    // lsp.add_file("res/u_filehandler.sru".into(), LintState::Complete)?;
    // lsp.add_file("res/pfc_w_find.srw".into(), LintState::Complete)?;

    // for token in tokens {
    //     println!("{:?} {:?} {}:{}-{}:{}", token.token_type, token.content, token.range.start.line, token.range.start.column, token.range.end.line, token.range.end.column);
    // }

    Ok(())
}
