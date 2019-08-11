(** Very tiny file utilities *)

open String_

(* NOTE Std is the Std of ExtLib; for some reason they have not bound it to ExtLib.Std *)

(** {2 Types} *)

(** f-or-d-or-symlink-or-other *)
type fds_t = F | D | S | O 

module Slash_operator = struct
  let slash = "/"

  (** Combines p1 and p2; if p1 doesn't end with a slash, and p2 doesn't start with a slash, it is added *)
  let ( / ) p1 p2 = 
    match String_.ends_with ~suffix:slash p1 || String_.starts_with ~prefix:slash p2 with
    | true -> p1^p2
    | false -> p1^slash^p2
end
open Slash_operator


(** {2 File input and output} *)

module File_read_write = struct

  let read_file s = 
    Std.input_file s

  let write_string_to_file ~fn s = 
    Std.output_file ~filename:fn ~text:s

  (* https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)
  (* 
let read_file_as_line_list filename =
  let chan = open_in filename in
  Std.input_list chan |> fun xs ->
  close_in chan; xs
*)
  let read_file_as_line_list filename = 
    let lines = ref [] in
    let chan = open_in filename in
    try
      while true; do
        lines := input_line chan :: !lines
      done; !lines
    with End_of_file ->
      close_in chan;
      List.rev !lines ;;
end
include File_read_write


(** {2 Filenames} *)

module Filenames = struct

  (* type follow_t = Follow|Dont_follow *)

  type fn = string

  let sep = "/"

  let ( / ) = fun dir base -> dir^sep^base

  module Export = struct
    (* drop to last sep, if any *)
    let basename fn = 
      let is = String_.indexes sep fn in
      if is=[] then fn else
        String_.drop (1+list_last is) fn

    let sanitize s = 
      Str.(
        s
        |> (global_replace (regexp {|[^A-Za-z0-9._-]|}) "_"))

    let dirname s = (split_on_last sep s)|>snd
  end
end
include Filenames.Export
open Filenames


(** {2 Commands} *)

module Commands = struct
  let readdir ?dir:(dir=".") () = 
    dir |> Unix.opendir |> fun dh ->
    let es = ref [] in
    (try
       while (true) do (es:=(Unix.readdir dh)::!es) done
     with End_of_file -> ());
    Unix.closedir dh;
    List.rev !es


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

  let cwd () = Unix.getcwd

  let cd = Unix.chdir

  let filename_exists fn = stat fn <> None

  (* pread from ExtUnixAll *)

  (** Get a file descriptor corresponding to a file. Possibly create the
      file, and init it (truncate it to 0). FIXME move elsewhere? *)
  let fd_from_file ~fn ~create ~init = Unix.(
      let flgs = [O_RDWR] @ (if create then [O_CREAT] else []) in
      openfile fn flgs 0o640 |> fun fd -> 
      (if init then ftruncate fd 0 else ()) |> fun _ -> 
      fd)

end
include Commands

(** {2 Permissions} *)

(* http://ocaml-fileutils.forge.ocamlcore.org/api-fileutils/FileUtil.html *)
module Perm = FileUtil

module Internal = struct

  open Perm

  let no_perm = {sticky=false; exec=false; write=false; read=false }  

  let rw = {no_perm with write=true; read=true }

  let no_perm = {user=no_perm; group=no_perm; other=no_perm }

  let default_create_perm = {no_perm with user=rw} |> int_of_permission

  (* let default_perm = 0o640 *)
end

include Internal

(** {2 Extra/derived operations} *)

module Extra = struct

  let equal_file path1 path2 =
    let s1 = Unix.stat path1 in
    let s2 = Unix.stat path2 in
    let uniq_id s = Unix.(s.st_dev,s.st_ino) in (* POSIX: unique per dev / filesystem *)
    (uniq_id s1 = uniq_id s2, (s1, s2)) (* return the stats as well, just in case useful *)
    
  let is_root path = equal_file path (path / "..")
  
end


module Export = struct
  include File_read_write
  include Filenames.Export
  include Commands
end
