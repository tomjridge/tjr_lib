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


(* module X = Bimap.Bimap_test() *)

(*
let _ = 
  let module X = Args_.Test() in
  ()

let _ = 
  let module X = Lru_two_gen.Test() in
  ()
*)

let _ = 
  let module X = Lru_two_gen_v3.Test() in
  ()

let _ = 
  let module _ = Tjr_show.Test() in ()
