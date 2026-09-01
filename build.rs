use std::path::PathBuf;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let proto_file =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("builtins/proto/builtins.proto");

    println!("cargo:rerun-if-changed={}", proto_file.display());

    let fds = protox::compile(&[proto_file.clone()], &[proto_file.parent().unwrap()])?;

    let protobuf_out = PathBuf::from(std::env::var("OUT_DIR").unwrap()).join("protobuf");

    std::fs::create_dir_all(&protobuf_out)?;

    prost_build::Config::new()
        .out_dir(&protobuf_out)
        .compile_fds(fds)?;

    Ok(())
}
