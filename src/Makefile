SHELL:=/bin/bash
BASH_ENV:=bash_env.sh
export BASH_ENV

all: FORCE
	$$ocamlc -c $$mls
	$$ocamlopt -c $$mls
	@echo "NOTE cma contains: $$cmos" # simple check
	$$mk_cma -g -a -o tjr_lib.cma $$cmos
	$$mk_cmxa -g -a -o tjr_lib.cmxa $$cmxs
#	$(MAKE) install

install:
	-ocamlfind remove tjr_lib
	ocamlfind install tjr_lib META *.cmi *.o *.a *.cma *.cmxa  #*.cmo *.cmx 

gb:=$(shell git symbolic-ref --short HEAD) # branch
dd1:=/tmp/l/ocamlfind_local/$(gb)
dd:=-destdir $(dd1)
local_install:
	test -d $(dd1) || mkdir $(dd1)
	-ocamlfind remove $(dd) tjr_lib
	ocamlfind install $(dd) tjr_lib META *.cmi *.o *.cma *.cmxa  #*.cmo *.cmx 

uninstall:
	ocamlfind remove tjr_lib

clean: 
	rm -f *.cmi *.cmo *.cmx *.o *.x *.a *.cma *.cmxa

FORCE:

