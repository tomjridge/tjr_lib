{ }:
let 
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
  fetchgit = pkgs.fetchgit;
  git = pkgs.git;
  opam = pkgs.opam;
  op = pkgs.ocamlPackages_latest;
  ocaml = op.ocaml; 
  findlib = op.findlib;
in 
stdenv.mkDerivation {

  name = "tjr_lib";
  
  src=./.;
  
  configurePhase = "true"; 	# Skip configure
  
  buildInputs = [ ocaml findlib git opam pkgs.unzip pkgs.wget pkgs.curl pkgs.m4 pkgs.which ];

  preBuild=''
    export OPAMROOT=$out
    opam init
    eval `opam config env`
    opam install cppo
    opam install bos omd fileutils yojson core_kernel ppx_deriving_yojson ppx_sexp_conv
  '';
  
  createFindlibDestdir = true;
  
  postInstall=''cp -R src $out'';
  
}
