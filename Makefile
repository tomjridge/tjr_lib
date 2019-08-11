TMP_DOC_DIR:=/tmp/tjr_lib
scratch:=/tmp/l/github/scratch

default: all

all::
	dune build src-test/test_main.exe

run_test:
	dune exec src-test/test_main.exe

-include Makefile.ocaml

# for auto-completion of Makefile target
clean::

