mod builtins;
mod solution;
mod types;

pub use builtins::BUILTIN_URL;
pub use solution::{Solution, SolutionType};
pub use types::*;

pub mod powerbuilder_proto {
    include!(concat!(env!("OUT_DIR"), "/protobuf/powerbuilder.rs"));
}
