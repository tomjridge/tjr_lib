default: all

-include Makefile.ocaml

all::
	dune build --only-packages=tjr_lib_core,tjr_lib
	dune build src-test/test_main.exe

run_test:
	OCAMLRUNPARAM=b dune exec src-test/test_main.exe


# for auto-completion of Makefile target
clean::

