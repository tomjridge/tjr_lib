# tjr_lib_core and tjr_lib: simple OCaml utility libraries

Very simple modules for debugging, files, lists, strings and substrings, trees etc. NOTE there are two libraries: `tjr_lib_core` (for non-Unix stuff) and `tjr_lib` (for stuff that depends on OS facilities).

## Quicklinks

* OCamldoc is [here](https://tomjridge.github.io/tjr_lib/)



## Building and installing 

NOTE Building requires OCaml 4.07 for List.to_seq.


| Build type                     | Instructions                                                 |
| ------------------------------ | ------------------------------------------------------------ |
| build from source              | In this directory type: make.  NOTE this requires the dependencies to be manually installed first using opam. |
| install from source using opam | In this directory, type: opam pin add tjr_lib . (NOTE the "." is part of the command.) |
| install from github using opam | Type: opam pin add tjr_lib https://github.com/tomjridge/tjr_lib.git |



## Dependencies

tjr_lib_core:

| Dependency                  | Comment                                                      |
| --------------------------- | ------------------------------------------------------------ |
| yojson                      | FIXME should move code which uses this to non-core           |
| ppx_deriving_yojson         | tjr_tree pretty-printing; yojson_diff; FIXME consider moving to non-core |
| ppx_deriving_yojson.runtime | ocamlfind lib installed with ppx_deriving_yojson             |
| XX ppx_sexp_conv            | tjr_tree pretty-printing (currently commented out)           |
| base, core_kernel           | no reason not to reuse functionality from these libs if possible |

tjr_lib:

| Dependency   | Comment                                                      |
| ------------ | ------------------------------------------------------------ |
| tjr_lib_core |                                                              |
| bos          | read and write files; execute external command FIXME replace? |
| fileutils    | tjr_file, permissions handling                               |
| omd          | markdown processing                                          |
| XX str       | (not needed anymore?) tjr_file (filename sanitization); doesn't need to be installed via opam? |



