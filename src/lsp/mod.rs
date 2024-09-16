pub mod lsp;
pub mod lsp_types;

pub mod powerbuilder_proto {
    include!(concat!(env!("OUT_DIR"), "/protobuf/powerbuilder.rs"));
    include!(concat!(env!("OUT_DIR"), "/protobuf/powerbuilder.serde.rs"));
}