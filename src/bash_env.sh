set -a # export all vars
#set -x # debug

libname="tjr_lib"
required_packages="str,bos.setup,omd,fileutils,yojson,core_kernel,ppx_deriving_yojson,ppx_sexp_conv,core,lwt" #,astring ,ppx_string_interpolate
description="Various useful OCaml functions"

source bash_env.common

function mk_links() {
    for f in msg/*.ml; do ln -sf $f .; done
}


function clean() {
	find . -xtype l -exec rm -f \{\} \;
}
