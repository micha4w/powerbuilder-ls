{ pkgs ? import <nixpkgs> {}, ... }:
  (pkgs.buildFHSEnv {
    name = "protoc";
    targetPkgs = pkgs: with pkgs; [
      python3
      gccStdenv
      p7zip
    ];
  }).env
