(* basic support for maps *)


type ('k,'v,'t) map_ops = {
  map_empty:'t;
  map_is_empty:'t -> bool;
  map_add:'k -> 'v -> 't -> 't;
  map_remove: 'k -> 't -> 't;
  map_find: 'k -> 't -> 'v option;
  map_bindings: 't -> ('k*'v)list;
}


(* reuse OCaml's maps *)
module Make = functor (Ord:Map.OrderedType) -> struct
  module Map_ = Map.Make(Ord)
  let map_ops = Map_.{
    map_empty=empty;
    map_is_empty=is_empty;
    map_add=add;
    map_remove=remove;
    map_find=(fun k t -> try Some(find k t) with Not_found -> None);
    map_bindings=bindings
  }
end
