#
# Generic makefile, srcs in current directory, not packed; config in
# bash_env.sh
#

SHELL:=/bin/bash
BASH_ENV:=bash_env.sh
export BASH_ENV

all: FORCE
	$$ocamlc -c $$mls
	$$ocamlopt -c $$mls
	@echo "NOTE cma contains: $$cmos" # simple check
	$$mk_cma -g -a -o $$libname.cma $$cmos
	$$mk_cmxa -g -a -o $$libname.cmxa $$cmxs
#	$(MAKE) install

install:
	-ocamlfind remove $$package_name
	mk_meta
	ocamlfind install $$package_name META *.cmi *.o *.a *.cma *.cmxa  #*.cmo *.cmx 

uninstall:
	ocamlfind remove $$package_name

clean: 
	rm -f *.cmi *.cmo *.cmx *.o *.x *.a *.cma *.cmxa

FORCE:




# gb:=$(shell git symbolic-ref --short HEAD) # branch
# dd1:=/tmp/l/ocamlfind_local/$(gb)
# dd:=-destdir $(dd1)
# local_install:
# 	test -d $(dd1) || mkdir $(dd1)
# 	-ocamlfind remove $(dd) $libname
# 	ocamlfind install $(dd) $libname META *.cmi *.o *.cma *.cmxa  #*.cmo *.cmx 

