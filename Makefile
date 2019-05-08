SHELL:=bash
DUNE:=dune

build:
	$(DUNE) build 

# NOTE install and uninstall do not involve opam; to build and install with opam, first pin
install: 
	$(DUNE) install

uninstall: 
	$(DUNE) uninstall

clean: 
	$(DUNE) clean

all:
	$(MAKE) clean
	$(MAKE) build
	$(MAKE) install
	$(MAKE) docs


SRC:=_build/default/_doc/_html
DST:=docs
DST2:=/tmp/tjr_lib
docs: FORCE
	$(DUNE) build @doc
	@if [ ! -z "$$PROMOTE_DOCS" ]; then rm -rf $(DST)/* ; cp -R $(SRC)/* $(DST); echo "docs built and promoted to docs/"; else \
	  rsync -vaz $(SRC)/* $(DST2); echo "docs built in $(DST2) but not promoted to docs/"; fi

promote_docs: FORCE
	PROMOTE_DOCS=true $(MAKE) docs

FORCE:
