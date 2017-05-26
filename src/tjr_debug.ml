(* simple debugging support *)

type debug = {
  log:string -> unit;
  jlog:Yojson.Safe.json -> unit;
  print: unit -> unit;
  with_debug: 'a 'b. ('a -> 'b) -> 'a -> 'b;
}

let mk_debug () = 
  let xs = ref [] in
  let log s = xs:=s::!xs in
  let jlog j = log (Yojson.Safe.to_string j) in
  let print () = 
    ignore (!xs |> Tjr_list.take 10 |> List.iter print_endline)
  in
  let with_debug f x = try f x with e -> (
      e|>Printexc.to_string|>print_endline;
      print ();
      raise e)
  in      
  {log;jlog;print;with_debug}

