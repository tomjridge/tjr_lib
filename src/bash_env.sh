set -a # export all vars
#set -x # debug

libname="tjr_lib"
required_packages="str,bos.setup,omd,fileutils,yojson,ppx_string_interpolate" #,astring
description="Various useful OCaml functions"

source bash_env.common
