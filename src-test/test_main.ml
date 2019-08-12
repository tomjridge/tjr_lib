open Tjr_lib
(* open Tjr_file *)
open Tjr_file.Slash_operator
open Tjr_file.Extra

let _ = 
  assert(equal_file "/" "/tmp/..");
  let _ = 
    "." |> iter_break (fun p ->
        match is_root p with
        | true -> Break ()
        | false -> print_endline ExtUnix.Specific.(realpath p); Cont (p / ".."))
  in
  ()

let _ = 
  let fn = ".bashrc" in
  find_file_cwd_to_root ~fn |> function
  | None -> Printf.printf "%s not found\n" fn
  | Some p -> Printf.printf "%s found at %s\n" fn p


let _ = 
  1 |> iter_k (fun ~k x -> 
      match x > 10 with
      | true -> ()
      | false -> (Printf.printf "%d\n" x; k (x+1)))

let _ = 
  let open Args_ in
  let xx = ref "" in
  let yy = ref (-1) in
  let zz = ref (-1) in
  let subcmd = ref "" in
  let rest = ref ["FIXME"] in
  let cmd = 
    make_cmd
      ~usage:"test args parser" 
      ~flags:[
        (string_flag ~name:"--xx" (fun s -> print_endline "xx"; xx:=s));
        (int_flag ~name:"--yy" (fun s -> print_endline "yy"; yy:=s));]
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
  

