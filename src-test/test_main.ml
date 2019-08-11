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
