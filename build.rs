use std::{io::Result, path::PathBuf};

fn main() -> Result<()> {
    let proto_file = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("proto/output.proto");

    println!("cargo:rerun-if-changed={}", proto_file.display());

    let protobuf_out = PathBuf::from(std::env::var("OUT_DIR").unwrap()).join("protobuf");
    let descriptor_path = protobuf_out.join("proto_descriptor.bin");

    std::fs::create_dir_all(&protobuf_out)?;

    prost_build::Config::new()
        .out_dir(&protobuf_out)
        .file_descriptor_set_path(&descriptor_path)
        .protoc_executable(protobuf_src::protoc())
        .compile_protos(&[proto_file.clone()], &[proto_file.parent().unwrap()])?;


    let descriptor_set = std::fs::read(descriptor_path)?;
    pbjson_build::Builder::new()
        .out_dir(&protobuf_out)
        .register_descriptors(&descriptor_set)?
        .build(&["."])?;

    Ok(())
}
