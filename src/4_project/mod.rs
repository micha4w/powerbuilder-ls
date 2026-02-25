mod builtins;
mod project;
mod types;

pub use project::Project;
pub use builtins::BUILTIN_URL;
pub use types::*;

pub mod powerbuilder_proto {
    include!(concat!(env!("OUT_DIR"), "/protobuf/powerbuilder.rs"));
    include!(concat!(env!("OUT_DIR"), "/protobuf/powerbuilder.serde.rs"));
}
