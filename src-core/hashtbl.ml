include Stdlib.Hashtbl

(** merge m1 and m2, creating a new tbl (and leaving originals
    unaffected); prefer bindings from m1 *)
let merge ~m1 ~m2 = 
  (* copy m2 *)
  copy m2 |> fun tbl -> 
  (* update with newer entries from m1; FIXME could also use
       filter_map_inplace, which might be faster *)
  m1 |> iter (fun k v -> replace tbl k v);
  tbl

let to_list tbl = tbl |> to_seq |> List.of_seq
