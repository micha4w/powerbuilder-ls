use std::path::Path;

use self::{
    parser::Parser, parser_types::TopLevel, tokenizer::FileTokenizer
};

pub mod parser;
pub mod parser_types;
pub mod tokenizer;
pub mod tokenizer_types;

pub fn parse_file(file: &Path) -> anyhow::Result<Vec<TopLevel>> {
    let mut tokens = FileTokenizer::open(file)?;
    tokens.skip_headers();
    // TODO stream the tokens????

    let mut parser = Parser::new(&mut tokens);
    Ok(parser.parse_tokens())
}
