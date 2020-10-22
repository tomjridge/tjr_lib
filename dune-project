(lang dune 2.0)
(name tjr_lib)

;; for opam
(authors              "Tom Ridge <tom.j.ridge@googlemail.com>")
(maintainers          "Tom Ridge <tom.j.ridge@googlemail.com>")
(source               (github tomjridge/tjr_lib))
(homepage             https://github.com/tomjridge/tjr_lib)
(documentation        https://tomjridge.github.io/ocamldocs/)
(generate_opam_files  true)


(package 
 (name tjr_lib_core)
 (synopsis "Small OCaml lib (tjr_lib_core)")
 (depends 
   yojson
   ppx_deriving_yojson
   base
   core_kernel
   lru
))


(package 
 (name tjr_lib)
 (synopsis "Small OCaml lib (tjr_lib)")
 (depends 
   bos
   extlib extunix
   fileutils
   omd
   tjr_lib_core
))

