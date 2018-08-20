set -a # export all vars
#set -x # debug


# NOTE in toplevel you need to #require "bos.top" and bos.setup;;

libname="tjr_lib"
required_packages="str,tjr_lib_core,bos,bos.setup,fileutils,omd"
description="Various useful OCaml functions"
source ../../VERSION


# function mk_links() {
#     for f in core/*.ml non_core/*.ml; do ln -sf $f .; done
# }


function clean() {
	rm -f *.cmi *.cmo *.cmx *.o *.x *.a *.cma *.cmxa
	find . -maxdepth 1 -type l -exec rm -f \{\} \;
  rm -f *.html *.css
  rm -f META
}

mls=`ocamldep -sort -one-line *.ml`


# doc ----------------------------------------------------

function mk_doc() {
    ocamlfind ocamldoc $PKGS $WARN -html $mls
    # FIXME assume package built and installed
}



# generic from here ----------------------------------------------------

PKGS="-package $required_packages"
SYNTAX=""

# 8~"pattern-matching is not exhaustive"; 
# 11~"this match case is unused";
# 26~"unused variable s2"
WARN="-w @f@p@u@s@40-8-11-26"

# -thread needed for core
  ocamlc="$DISABLE_BYTE ocamlfind ocamlc   -thread -g $WARN $PKGS $SYNTAX"
ocamlopt="$DISABLE_NTVE ocamlfind ocamlopt -thread -g $WARN $PKGS $SYNTAX"
ocamldep="ocamlfind ocamldep $PKGS"

mk_cma="$DISABLE_BYTE ocamlfind ocamlc"
mk_cmxa="$DISABLE_NTVE ocamlfind ocamlopt"

cmos="${mls//.ml/.cmo}"
cmxs="${mls//.ml/.cmx}"

natives="
"

package_name="${libname}"

function mk_meta() {
cat >META <<EOF
name="$package_name"
description="$description"
version="$v"
requires="$required_packages"
archive(byte)="$libname.cma"
archive(native)="$libname.cmxa"
EOF
}
