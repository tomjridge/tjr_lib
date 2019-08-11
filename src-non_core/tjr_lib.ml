(** {2 Core} *)

include Tjr_lib_core

(* to develop interactively, use: (merlin-use "bos.setup") in emacs
   interactively *)

(** {2 Non-core} *)

module Tjr_buffer = Tjr_buffer

module Tjr_config = Tjr_config

module Tjr_file = Tjr_file
(* include Tjr_file.Export *)

module Internal_cmd = struct
  open Bos.OS.Cmd
  include Bos.Cmd
  let run = run    

  (** Run a command, and expect an Ok *)
  let shell_command_to_unit s = 
    of_string s |> dest_Ok
    |> run |> dest_Ok;
end
let shell_command_to_unit = Internal_cmd.shell_command_to_unit

module Adoc = struct  
  open Tjr_file      
  open Internal_cmd

  (** FIXME not concurrent safe; also awful *)
  let adoc_to_html: string -> string = fun s ->
      write_string_to_file ~fn:"/tmp/tmp.adoc" s;
      shell_command_to_unit {|asciidoctor -s-o /tmp/tmp.html /tmp/tmp.adoc|};
      read_file "/tmp/tmp.html"
end
include Adoc

module Md = struct
  let md_to_html md = Omd.(md|>of_string|>to_html)
end
include Md


