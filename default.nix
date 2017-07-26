{ }:
let 
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
  fetchgit = pkgs.fetchgit;
  op = pkgs.ocamlPackages_latest;
  ocaml = op.ocaml; 
  findlib = op.findlib;
in 
stdenv.mkDerivation {

  name = "tjr_lib";
  
  src=./.;
  
  buildInputs = [ ocaml findlib ];
  
  configurePhase = "true"; 	# Skip configure
  
  createFindlibDestdir = true;
  
  postInstall=''cp -R src $out'';
  
}
