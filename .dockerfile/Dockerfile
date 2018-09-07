FROM ubuntu_16.04_with_ocaml_4.06

RUN eval `opam config env` && opam install ppx_deriving_yojson bos fileutils str omd

RUN git clone https://github.com/tomjridge/tjr_lib.git

# -n means don't build etc
RUN eval `opam config env` && opam pin add -n tjr_lib ./tjr_lib
RUN eval `opam config env` && opam install tjr_lib
RUN eval `opam config env` && opam list
