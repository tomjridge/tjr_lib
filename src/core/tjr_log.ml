(* simple debugging support ----------------------------------------- *)

(* NOTE do not use in production; perhaps place log ops in assert
   statements so they can be disabled easily *)

(* Store log messages, and print the last n at some point. The idea is
   that any broken invariant immediately calls exit, and via at_exit,
   the the last n messages are printed.  *)

type log_ops = {
  log: string -> unit;

  log_lazy: (unit -> string) -> unit;  
  (* in case the string takes a log time to construct *)

  log_now: string -> unit;  (* print immediately, but also log *)
  log_n: int ref;
  jlog:Yojson.Safe.json -> unit;
  print_last_n: unit -> unit;
  with_log: 'a 'b. ('a -> 'b) -> 'a -> 'b;
}

(* NOTE logs accumulate in xs, so memory leak if used in
   production. FIXME easy to fix *)

let mk_log_ops () = 
  let log_n = ref 10 in
  let xs = ref [] in
  let log s = xs:=(fun () -> s)::!xs in
  let log_lazy f = xs:=f::!xs in
  let jlog j = log (Yojson.Safe.to_string j) in
  let log_now s = log s; print_endline s in
  let print_last_n () = 
    ignore (!xs |> fun xs ->
            xs 
            |> Tjr_list.take (min (List.length xs) !log_n)
            |> List.iter (fun f -> print_endline (f())))
  in
  let with_log f x = try f x with e -> (
      e|>Printexc.to_string|>print_endline;
      print_last_n ();
      raise e)
  in      
  {log;log_lazy;log_now;log_n;jlog;print_last_n;with_log}

