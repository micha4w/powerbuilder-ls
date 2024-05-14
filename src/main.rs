mod parser;
use parser::{tokenizer::FileTokenizer, tree_builder::TreeBuilder};

fn main() -> anyhow::Result<()> {
    let path = "res/test.sru";
    let mut tokens = FileTokenizer::open(path)?;
    // TODO stream the tokens

    TreeBuilder::new(&mut tokens);
    // for token in tokens {
    //     println!("{:?} {:?} {}:{}-{}:{}", token.token_type, token.content, token.range.start.line, token.range.start.column, token.range.end.line, token.range.end.column);
    // }

    Ok(())
}
