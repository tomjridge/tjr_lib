set -a # export all vars
#set -x # debug

libname="tjr_lib"
required_packages="str,bos.setup,omd,fileutils,yojson,core_kernel,ppx_deriving_yojson,ppx_sexp_conv,core,lwt" #,astring ,ppx_string_interpolate
description="Various useful OCaml functions"

source bash_env.common

function mk_links() {
true
#    for f in msg2/*.ml; do ln -sf $f .; done
}


function clean() {
	rm -f *.cmi *.cmo *.cmx *.o *.x *.a *.cma *.cmxa
	find . -xtype l -exec rm -f \{\} \;
}

function real_clean() {
	find . -maxdepth 1 -type l -exec rm -f \{\} \;
}
