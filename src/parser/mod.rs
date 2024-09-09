use std::path::Path;

use self::{
    parser::Parser, parser_types::TopLevel, tokenizer::FileTokenizer
};

pub mod parser;
pub mod parser_types;
pub mod tokenizer;
pub mod tokenizer_types;

pub fn tokenize_file(file: &Path) -> anyhow::Result<Parser> {
    let mut tokens = FileTokenizer::open_file(file)?;
    tokens.skip_headers();
    // TODO stream the tokens????

    Ok(Parser::new(tokens))
}


pub fn tokenize(buf: &String) -> anyhow::Result<Parser> {
    let tokens = FileTokenizer::new(buf.clone());

    Ok(Parser::new(tokens))
}