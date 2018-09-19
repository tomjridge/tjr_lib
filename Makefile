SHELL:=bash

build:
	dune build 

install: 
	dune install

uninstall: 
	dune uninstall

clean: 
	dune clean
