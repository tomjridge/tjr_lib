SHELL:=/bin/bash
BASH_ENV:=bash_env.sh
export BASH_ENV

# include ----------------------------------------

.DEFAULT_GOAL:=all
include depend.mk

# targets ----------------------------------------

all: FORCE
	$(MAKE) $(tjr_prelude.x) tjr_lib.cma tjr_lib.cmxa


%.x: %.mli %.ml 
	$$ocamlc -c $*.mli $*.ml
	$$ocamlopt -c $*.mli $*.ml
	touch $@

%.x: %.ml
	$$ocamlc -c $*.ml
	$$ocamlopt -c $*.ml
	touch $@

tjr_lib.cma: $(xs)
	$$mk_cma -g -a -o $@ $(xs:.x=.cmo)

tjr_lib.cmxa: $(xs)
	$$mk_cmxa -g -a -o $@ $(xs:.x=.cmx)

clean: 
	rm -f *.cmi *.cmo *.cmx *.o *.x *.a *.cma *.cmxa

FORCE:

