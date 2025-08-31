mod lint_bodies;
mod lint_top_levels;
mod linter;
mod load_builtins;
mod project;
mod types;

pub use linter::*;
pub use project::*;
pub use types::*;

pub mod powerbuilder_proto {
    include!(concat!(env!("OUT_DIR"), "/protobuf/powerbuilder.rs"));
    include!(concat!(env!("OUT_DIR"), "/protobuf/powerbuilder.serde.rs"));
}
