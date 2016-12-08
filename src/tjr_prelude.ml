
(* to develop interactively, use: (merlin-use "bos.setup") in emacs interactively *)

(* Bos.Cmd and Bos.OS.Cmd are different! *)

(* prelude ---------------------------------------- *)

let get_ok = Rresult.R.get_ok

module X_string = Tjr_string

module X_list = Tjr_list

module Misc = struct

  open Bos_setup

  open Bos

  open Bos.OS.Cmd
  open Bos.Cmd

  let pln = print_endline

  (* let _ = pln ("# on host "^(hn_to_string hn)) *)

  let read_file s = OS.File.read (Fpath.v s) |> get_ok

  let write_string_to_file s f = OS.File.write (Fpath.v f) s |> get_ok


  let dest_Some = (function (Some x) -> x | _ -> failwith "dest_Some")

  let mk_int = 
    let c = ref 0 in
    fun () -> (
        c:=!c+1;
        !c)

  let mk_sym () = mk_int () |> string_of_int

end

include Misc


(* asciidoc ---------------------------------------- *)

module Adoc = struct

  open Misc

  open Bos_setup
  open Bos.Cmd

  (* FIXME not concurrent safe; also awful *)
  let adoc_to_html: string -> string = (fun s ->
    write_string_to_file s "/tmp/tmp.adoc";
    Bos.OS.Cmd.run (Cmd.v "asciidoctor" % "-s" % "-o" % "/tmp/tmp.html" % "/tmp/tmp.adoc") |> get_ok;
    read_file "/tmp/tmp.html"
  )

end


module Md = struct

  let md_to_html md = Omd.(md|>of_string|>to_html)

end


(* files etc ---------------------------------------- *)


module File = struct

  open X_list
  open X_string
  open Bos.OS


  type fds_t = F | D | S | O (* f-or-d-or-symlink-or-other *)

  (* type follow_t = Follow|Dont_follow *)

  type fn = string
  
  let sep = "/"

  let ( / ) = fun dir base -> dir^sep^base

  (* drop to last sep, if any *)
  let basename fn = 
    let is = X_string.indexes sep fn in
    if is=[] then fn else
      X_string.drop (1+X_list.last is) fn
  

let sanitize s = 
  Str.(
    s
    |> (global_replace (regexp {|[^A-Za-z0-9._-]|}) "_"))


  (* return contents; if rel, then no leading path in result *)
  let readdir ~rel fn = (
    Dir.contents ~dotfiles:true ~rel:rel Fpath.(v fn) |> get_ok |>
    List.map Fpath.to_string |> List.sort Pervasives.compare
  )

  let rename f g = Unix.rename f g

  let readlink fn = Unix.readlink fn

  let stat : fn -> fds_t option = Unix.(
      fun fn -> 
        try (
          match (stat fn).st_kind with
          |S_REG -> Some F
          |S_DIR -> Some D
          |S_LNK -> Some S
          |_ -> Some O)
        with Unix_error _ -> None)

  let lstat : fn -> fds_t option = Unix.(
      fun fn -> 
        try (
          match (lstat fn).st_kind with
          |S_REG -> Some F
          |S_DIR -> Some D
          |S_LNK -> Some S
          |_ -> Some O)
        with Unix_error _ -> None)

  let can_follow : fn -> bool = Unix.(
      fun fn -> 
        try (let _ = stat fn in true) with Unix_error _ -> false)

  let exists fn = stat fn <> None

  (* return all basenames? or paths which extend the given path FIXME
     really a completion thing *)
  let ls : fn -> fn list = X_list.(
    fun fn -> 
      match (contains ~sub:sep fn) with
      | false -> (readdir true "." |> inc (fun x -> starts fn x))
      | true -> (
          let (path,base) = split_on_last sep fn in
          readdir false (path^sep) |> inc (fun x -> starts base x))
    )

  let dirname s = (split_on_last sep s)|>snd

  let complete fn = (
    (* if fn has a single extension then try to extend that *)
    let fn = (
      match ls fn with
      | [x] -> (dirname fn ^ sep ^ x)
      | _ -> fn)
    in
    (* if fn is a dir, add a sep *)
    let fn = (if (stat fn = Some(D)) then (fn^sep) else fn) in
    (* then find extensions *)
    ls fn)



end
