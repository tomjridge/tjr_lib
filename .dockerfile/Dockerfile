# NOTE maybe add a "RUN echo ..." to force a rebuild, or --no-cache

# FROM ocaml/opam:ubuntu - latest doesn't work due to problems with ppx_deriving deps
# 17.04 doesn't work due to missing archives at ubuntu? or stale packages info?
FROM tjr_ubuntu_16.04_with_ocaml
# was ocaml/opam:ubuntu-16.04_ocaml-4.04.2

RUN eval `opam config env` && opam install core_kernel ppx_sexp_conv ppx_deriving_yojson rresult bos fileutils omd

RUN git clone https://github.com/tomjridge/tjr_lib.git

# -n means don't build etc
RUN eval `opam config env` && opam pin add -n tjr_lib ./tjr_lib
RUN eval `opam config env` && opam install tjr_lib
RUN eval `opam config env` && opam list
