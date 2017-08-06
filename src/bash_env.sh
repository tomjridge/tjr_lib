set -a # export all vars
#set -x # debug

libname="tjr_lib"
required_packages="str,bos.setup,omd,fileutils,yojson,core_kernel,ppx_deriving_yojson,ppx_sexp_conv,core" #,astring ,ppx_string_interpolate
description="Various useful OCaml functions"

source bash_env.common
