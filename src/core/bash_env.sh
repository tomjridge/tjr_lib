set -a # export all vars
#set -x # debug


libname="tjr_lib_core"
package_name="${libname}"
required_packages="ppx_deriving_yojson"  # str?
description="Various useful OCaml functions"

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


function mk_meta() {
v=`cat ../../VERSION`
cat >META <<EOF
name="$package_name"
description="$description"
version="$v"
requires="$required_packages"
archive(byte)="$libname.cma"
archive(native)="$libname.cmxa"
EOF
}
