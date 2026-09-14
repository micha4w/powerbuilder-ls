#![feature(try_trait_v2, try_trait_v2_residual)]
#![feature(coroutines, stmt_expr_attributes, iter_from_coroutine)]

mod types;

#[path = "3_builder/mod.rs"]
mod builder;
#[path = "6_linter/mod.rs"]
mod linter;
#[path = "7_ls/mod.rs"]
mod ls;
#[path = "2_parser/mod.rs"]
mod parser;
#[path = "5_resolver/mod.rs"]
mod resolver;
#[path = "4_solution/mod.rs"]
mod solution;
#[path = "1_tokenizer/mod.rs"]
mod tokenizer;

use std::{
    panic::{self, PanicHookInfo},
    process,
};

use ls::PowerBuilderLS;
use tracing_subscriber::{fmt::format::FmtSpan, EnvFilter};

fn panic_hook(info: &PanicHookInfo) {
    eprintln!("{}", info);
    process::exit(1)
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    panic::set_hook(Box::new(panic_hook));

    let mut args = std::env::args();
    if args.len() > 1 {
        if args.len() == 2 && args.nth(1) == Some("--version".into()) {
            let version =
                option_env!("POWERBUILDER_LS_VERSION").unwrap_or(env!("CARGO_PKG_VERSION"));
            let build_info = option_env!("POWERBUILDER_LS_BUILD");

            println!("PowerBuilder-LS {version}");
            if let Some(info) = build_info {
                println!(" ({info})");
            }

            return Ok(());
        }

        eprintln!("PowerBuilder-LS does not take any arguments. Use --version to get the version.");
        process::exit(1);
    }

    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_span_events(FmtSpan::CLOSE)
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = tower_lsp::LspService::new(move |client| PowerBuilderLS::new(client));

    tower_lsp::Server::new(stdin, stdout, socket)
        .serve(service)
        .await;

    Ok(())
}
