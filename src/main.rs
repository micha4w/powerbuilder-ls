mod parser;
mod lsp;

use parser::{tokenizer::FileTokenizer, parser::Parser};
use lsp::lsp::LSP;

fn main() -> anyhow::Result<()> {
    // let path = "res/test.sru";
    // let mut tokens = FileTokenizer::open(path)?;
    // tokens.skip_headers();
    // // TODO stream the tokens

    // let mut parser = Parser::new(&mut tokens);
    // parser.parse_tokens();
    
    let mut lsp = LSP::new()?;
    // lsp.add_file("res/test.sru".into(), false)?;
    lsp.add_file("res/u_filehandler.sru".into(), false)?;

    // for token in tokens {
    //     println!("{:?} {:?} {}:{}-{}:{}", token.token_type, token.content, token.range.start.line, token.range.start.column, token.range.end.line, token.range.end.column);
    // }

    Ok(())
}
