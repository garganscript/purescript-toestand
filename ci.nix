{ pkgs ? import ./nix/pinned.nix { } }:
let
  easy-ps = import ./nix/easy-ps.nix { inherit pkgs; };

  build = pkgs.writeShellScriptBin "build" ''
    #!/usr/bin/env bash
    purs compile "src/**/*.purs" "test/**/*.purs"
  '';
in
pkgs.mkShell {
  name = "purescript-reactix-d3";

  buildInputs = [
    easy-ps.purs-0_15_0
    easy-ps.psc-package
    easy-ps.spago
    build
    pkgs.dhall-json
  ];
}
