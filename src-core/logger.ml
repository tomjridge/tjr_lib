(** A global logger. *)

let dest_Some x = match x with | Some x -> x | None -> failwith __LOC__

let logger : Log.log_ops option ref = 
  (ref None) |> Global.register ~name:(__MODULE__^".logger")


(* logging --------------------------------------------------------- *)

let log s = (dest_Some !logger).log s 
let jlog s = (dest_Some !logger).jlog s 


let log_lazy f = (dest_Some !logger).log_lazy f

(* minor abbrev *)
let logl f = log_lazy f 

let print_last_n () = (dest_Some !logger).print_last_n ()


(* warn ------------------------------------------------------------- *)

(** Warn with a string; warnings are always printed, and added to
    logs. The intention is that warning should never appear. *)
let warn s = 
  print_endline ("WARNING: "^s);
  flush_all();
  log s


(* exit_hook -------------------------------------------------------- *)


let print_at_exit = 
  (ref false)
  |> Global.register ~name:(__MODULE__^".print_at_exit")

let at_exit ~print =
  print_at_exit:=print

let _ = 
  Pervasives.at_exit (fun _ -> if !print_at_exit then print_last_n () else ())
