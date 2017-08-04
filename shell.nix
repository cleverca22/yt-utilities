{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "scripts";
  LANG = "en_US.UTF-8";
  buildInputs = [ icu zlib ];
}
