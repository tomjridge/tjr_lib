SHELL:=bash

build:
	dune build 

# NOTE install and uninstall do not involve opam; to build and install with opam, first pin
install: 
	dune install

uninstall: 
	dune uninstall

clean: 
	dune clean
