mod lint_bodies;
mod lint_top_levels;
mod linter;

pub use linter::*;

pub mod powerbuilder_proto {
    include!(concat!(env!("OUT_DIR"), "/protobuf/powerbuilder.rs"));
    include!(concat!(env!("OUT_DIR"), "/protobuf/powerbuilder.serde.rs"));
}
