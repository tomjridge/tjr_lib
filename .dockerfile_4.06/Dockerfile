FROM ubuntu_16.04_with_ocaml_4.06

RUN eval `opam config env` && opam install ppx_sexp_conv ppx_deriving_yojson bos fileutils omd

# NOTE this builds the version on github FIXME perhaps just copy src?
RUN git clone https://github.com/tomjridge/tjr_lib.git

RUN echo edit this to force rebuild...

# -n means don't build etc
RUN eval `opam config env` && opam pin add -n tjr_lib ./tjr_lib
RUN eval `opam config env` && opam install tjr_lib
RUN eval `opam config env` && opam list
