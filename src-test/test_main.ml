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
  
    
  
  
