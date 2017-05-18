
(* to develop interactively, use: (merlin-use "bos.setup") in emacs interactively *)

(* Bos.Cmd and Bos.OS.Cmd are different! *)

(* prelude ---------------------------------------- *)

include Tjr_either
include Tjr_gensym
include Tjr_result

module Misc = struct

  open Bos_setup

  open Bos

  open Bos.OS.Cmd
  open Bos.Cmd

  let pln = print_endline

  (* let _ = pln ("# on host "^(hn_to_string hn)) *)


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

  open Tjr_file

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



(* short names for open ---------------------------------------- *)

module F = Tjr_file

module S = Tjr_string

module L = Tjr_list

