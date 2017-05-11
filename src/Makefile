SHELL:=/bin/bash
BASH_ENV:=bash_env.sh
export BASH_ENV

all: FORCE
	$$ocamlc -c $$mls
	$$ocamlopt -c $$mls
	$$mk_cma -g -a -o tjr_lib.cma $$cmos
	$$mk_cmxa -g -a -o tjr_lib.cmxa $$cmxs
	$(MAKE) install

install:
	-ocamlfind remove tjr_lib
	ocamlfind install tjr_lib META *.cmi *.cmo *.cmx *.o *.cma *.cmxa

uninstall:
	ocamlfind remove tjr_lib

clean: 
	rm -f *.cmi *.cmo *.cmx *.o *.x *.a *.cma *.cmxa

FORCE:

