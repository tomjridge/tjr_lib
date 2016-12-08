SHELL:=bash

all:
	$(MAKE) -C src

install: all
	ocamlfind install tjr_lib src/META `find src -name "*.cmi" -o -name "*.cma" -o -name "*.cmxa" -o -name "*.a"`

uninstall: 
	ocamlfind remove tjr_lib

clean: 
	$(MAKE) -C src clean 
