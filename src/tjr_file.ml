(* very tiny file utilities *)

open Tjr_result
open Tjr_list
open Tjr_string
open Bos.OS

let read_file s = Bos.OS.File.read (Fpath.v s) |> get_ok

let write_string_to_file s f = Bos.OS.File.write (Fpath.v f) s |> get_ok



type fds_t = F | D | S | O (* f-or-d-or-symlink-or-other *)

(* type follow_t = Follow|Dont_follow *)

type fn = string

let sep = "/"

let ( / ) = fun dir base -> dir^sep^base

(* drop to last sep, if any *)
let basename fn = 
  let is = Tjr_string.indexes sep fn in
  if is=[] then fn else
    Tjr_string.drop (1+list_last is) fn


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
let ls : fn -> fn list = Tjr_list.(
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


(* pread from ExtUnixAll *)



(* permissions ------------------------------------------------------ *)

(* http://ocaml-fileutils.forge.ocamlcore.org/api-fileutils/FileUtil.html *)
module Perm = FileUtil

open Perm

let no_perm = {sticky=false; exec=false; write=false; read=false }  
let rw = {no_perm with write=true; read=true }

let no_perm = {user=no_perm; group=no_perm; other=no_perm }

let default_create_perm = {no_perm with user=rw} |> int_of_permission
                            
(* let default_perm = 0o640 *)
