(* basic support for maps *)


type ('k,'v,'t) map_ops = {
  map_empty:unit -> 't;
  map_is_empty:'t -> bool;
  map_add:'k -> 'v -> 't -> 't;
  map_remove: 'k -> 't -> 't;
  map_find: 'k -> 't -> 'v option;
}


(* reuse OCaml's maps *)
module Make = functor (Ord:Map.OrderedType) -> struct
  include Map.Make(Ord)
  let map_ops = {
    map_empty=(fun () -> empty);
    map_is_empty=is_empty;
    map_add=add;
    map_remove=remove;
    map_find=(fun k t -> try Some(find k t) with Not_found -> None);
  }
end
