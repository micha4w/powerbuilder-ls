mod tokenizer;
mod types;

pub use tokenizer::*;
pub use types::*;

use std::path::Path;

pub fn tokenize_file(file: &Path) -> anyhow::Result<FileTokenizer> {
    let mut tokens = FileTokenizer::open_file(file)?;
    tokens.skip_headers();
    // TODO stream the tokens????

    Ok(tokens)
}

pub fn tokenize(buf: &String) -> anyhow::Result<FileTokenizer> {
    let tokens = FileTokenizer::new(buf.clone());

    Ok(tokens)
}
