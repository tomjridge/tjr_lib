(** Simple command line processing *)


open Iter

module Internal_types = struct

  (* NOTE after recognizing something, we drop the something; in
     particular, after dispatching on the flag or subcmd, the flag or
     subcmd just parses the rest... *)

  (** A "named" parser, where the name is somehow related to the parsing *)
  type 'a t = {
    name:string;
    parse:string list -> string list;
    extra: 'a
    (* guaranteed to be a proper suffix of the input list *)
  }
  
  type flag_extra = unit (* string -> bool *)

  type flag = flag_extra t  (* include the flag action as the extra field *)

  (* mutable set of t *)
  type 'a t_set = {
    names: (string,'a t) Hashtbl.t;  (* mutable! *)
    parse: string list -> string list;
    add: 'a t -> unit; (* modifies options *)
  }

  type flag_set = flag_extra t_set

  type subcmd_extra = unit -> unit

  type subcmd = subcmd_extra t

  type subcmd_set = subcmd_extra t_set

  type cmd = {
    usage:string;
    flags:flag_set;
    subcmds:subcmd_set
  }
end
include Internal_types

(*
(** We include these in the public interface *)
type 'a t = 'a Internal_types.t
type flag = Internal_types.flag
type subcmd = Internal_types.subcmd
type cmd = Internal_types.cmd
*)

module Internal_string = struct

  let starts_with_double_dash s = String_.starts_with ~prefix:"--" s

  let starts_with_single_dash s = String_.starts_with ~prefix:"-" s

(*
  let drop_double_dash s = 
    assert(starts_with_double_dash s);
    String_.drop 2 s
*)

  let is_flag s = starts_with_single_dash s
end
open Internal_string

module Common_flags = struct
  let int_flag ~name (f:int -> unit) : flag = {
    name;
    parse=(function
        | [] -> 
          Printf.sprintf "%s option failed to parse (empty list)\n" name
          |> failwith
        | x::xs -> (
            int_of_string_opt x |> function
            | None -> 
              Printf.sprintf "%s option failed to parse number: %s\n" name x
              |> failwith
            | Some i -> f i; xs));
(*    extra=(fun s -> int_of_string_opt s |> function
      | None -> false
      | Some _ -> true)*)
    extra=()
  }

  let _ = int_flag

  let string_flag ~name (f:string -> unit) : flag = {
    name;
    parse=(function
        | [] -> 
          Printf.sprintf "%s option failed to parse (empty list)\n" name
          |> failwith __LOC__
        | x::xs -> (f x; xs));
    extra=()
  }

  let boolean_flag ~name f : flag = {
    name;
    parse=(function
        | [] -> 
          Printf.sprintf "%s option failed to parse (empty list)\n" name
          |> failwith __LOC__
        | "true"::xs -> (f true; xs)
        | "false"::xs -> (f false; xs)
        | x::_ -> 
          Printf.sprintf "boolean_flag: failed to parse: %s\n" x
          |> failwith);
    extra=()
  }

(*
  (** if flag parsing fails, use the default *)
  let flag_with_default default f = {
    name=f.name;
    parse=(fun xs -> try f.parse xs with _ -> f.extra default);
    extra=()
  }
*)
end
include Common_flags
  

module Internal_flags = struct

  let parse_flags ~(flags:(string,flag)Hashtbl.t) xs = 
    xs |> iter_k (fun ~k xs -> 
        match xs with
        | [] -> []
        | x::xs -> 
          match is_flag x with
          | true -> (
              Hashtbl.find_opt flags x |> function
              | None -> 
                Printf.sprintf 
                  "parse_flags: failed to find parser for: %s\n" x
                |> failwith
              | Some f -> 
                f.parse xs |> fun xs' -> 
                k xs')
          | false -> x::xs) (* stop if not a flag *)

  let make_flag_set flag_list =
    let tbl = Hashtbl.create 10 in
    let parse xs = parse_flags ~flags:tbl xs in  (* NOTE eta ? *)
    let add_flag f = Hashtbl.replace tbl f.name f in
    List.iter (fun f -> add_flag f) flag_list;
    { names=tbl; parse; add=add_flag }

  let _ = make_flag_set

end
open Internal_flags
let make_flag_set = make_flag_set

module Internal_subcmds = struct
  let make_subcmd ~name ~set ~(flags:flag_set) : subcmd = {
    name;
    parse=(fun xs -> flags.parse xs);(* NOTE eta, so that we can add to flags *)
    extra=set
  }

  let dispatch_on_subcmd ~(subcmds:(string,subcmd)Hashtbl.t) xs =
    match xs with
    | [] -> []
    | x::xs -> 
      match Hashtbl.find_opt subcmds x with
      | None -> x::xs
      (* return the unparsed remainder - there may be something
         following the subcommand eg filename etc *)
      | Some s -> 
        s.extra();
        s.parse xs
(*
  let make_subcmd_parsers ~(subcmds:(string,subcmd)Hashtbl.t) xs = 
    match xs with 
    | [] -> []
    | x::xs -> 
      match Hashtbl.find_opt subcmds x with
      | None -> 
        Printf.sprintf "make_subcmd_parsers: unrecognized subcmd: %s\n" x
        |> failwith
      | Some c -> 
        c.parse_following xs
*)

  let make_subcmd_set subcmd_list =
    let subcmds = Hashtbl.create 10 in
    let parse xs = dispatch_on_subcmd ~subcmds xs in
    let add (x:subcmd) = Hashtbl.replace subcmds x.name x in
    List.iter (fun s -> add s) subcmd_list;
    { names=subcmds; parse; add }
end
open Internal_subcmds

let make_subcmd = make_subcmd
let make_subcmd_set = make_subcmd_set

module Internal_cmd = struct

  let cmd_parser ~flags_parser ~subcmd_parsers xs = 
    xs 
    |> flags_parser
    |> subcmd_parsers

end
open Internal_cmd

let make_cmd ~usage ~flags ~subcmds = {
  usage;
  flags=make_flag_set flags;
  subcmds=make_subcmd_set subcmds
}

let cmd_parser (cmd:cmd) (xs:string list) = 
  let {flags; subcmds;_} = cmd in
  cmd_parser
    ~flags_parser:flags.parse 
    ~subcmd_parsers:subcmds.parse
    xs


module Test() = struct

  let _ = 
    let xx = ref "" in
    let yy = ref (-1) in
    let zz = ref (-1) in
    let subcmd = ref "" in
    let rest = ref ["FIXME"] in
    let cmd = 
      make_cmd
        ~usage:"test args parser" 
        ~flags:[
          string_flag ~name:"--xx" (fun s -> print_endline "xx"; xx:=s);
          int_flag ~name:"--yy" (fun s -> print_endline "yy"; yy:=s)
        ]
        ~subcmds:[
          make_subcmd 
            ~name:"build"
            ~set:(fun () -> subcmd:="build")
            ~flags:(
              let zz_ = (fun i -> print_endline "zz"; zz:=i) in
              make_flag_set [
                (* --zz and -z are aliases *)
                int_flag ~name:"--zz" zz_;
                int_flag ~name:"-z" zz_
              ]);
          make_subcmd
            ~name:"clean"
            ~set:(fun () -> subcmd:="clean")
            ~flags:(make_flag_set [])
        ]
    in
    let _ = 
      "--xx 2 --yy 10 build -z 2 filename" 
      |> String_.split_on_char ' '
      |> cmd_parser cmd 
      |> fun xs -> rest:=xs
    in
    Printf.printf 
      "xx:%s yy:%d subcmd:%s zz:%d rest:%s\n"
      (!xx) (!yy) (!subcmd) (!zz) (!rest |> String_.concat_strings ~sep:",")

end
