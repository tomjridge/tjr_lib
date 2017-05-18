set -a # export all vars
# set -x # debug

root=$(realpath $(dirname $BASH_SOURCE))/..

PKGS="-package str,bos.setup,omd,fileutils" #,astring
SYNTAX=""

# 8~"pattern-matching is not exhaustive"; 
# 11~"this match case is unused";
# 26~"unused variable s2"
WARN="-w @f@p@u@s@40-8-11-26"

  ocamlc="$DISABLE_BYTE ocamlfind ocamlc   $WARN $PKGS $SYNTAX"
ocamlopt="$DISABLE_NTVE ocamlfind ocamlopt $WARN $PKGS $SYNTAX"
ocamldep="ocamlfind ocamldep $PKGS"

mk_cma="$DISABLE_BYTE ocamlfind ocamlc"
mk_cmxa="$DISABLE_NTVE ocamlfind ocamlopt"

mls=`ocamldep -sort -one-line *.ml`
cmos="${mls//.ml/.cmo}"
cmxs="${mls//.ml/.cmx}"

natives="
"

libn1="tjr_lib"
branch=`git symbolic-ref --short HEAD` 
v=`date +'%F'`
if [ "$branch" = "master" ]; then
    libn="${libn1}"
else 
    libn="${libn1}_${branch}"
fi
function mk_meta() {
cat >META <<EOF
name="$libn"
description="Tom Ridge, various OCaml functions"
version="$v"
requires="str bos.setup omd fileutils"
archive(byte)="tjr_lib.cma"
archive(native)="tjr_lib.cmxa"
EOF

}
