let get_ok (r:('a,'b)result) = match r with
  | Ok x -> x
  | Error e -> failwith __LOC__
