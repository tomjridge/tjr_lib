(* basic support for maps *)


type ('k,'v,'t) map_ops = {
  map_empty:'t;
  map_is_empty:'t -> bool;
  map_add:'k -> 'v -> 't -> 't;
  map_remove: 'k -> 't -> 't;
  map_find: 'k -> 't -> 'v option;
  map_bindings: 't -> ('k*'v)list;
}


(* prefer m2 bindings over m1 *)
let map_union ~map_ops ~m1 ~m2 = 
  let { map_add; map_bindings; _ } = map_ops in
  Tjr_list.with_each_elt'
    ~step:(fun ~state:m1' (k,op) -> map_add k op m1')
    ~init_state:m1
    (map_bindings m2)

let list_to_map ~map_ops kvs = 
  Tjr_list.with_each_elt
    ~list:kvs
    ~step:(fun ~state (k,v) -> map_ops.map_add k v state)
    ~init:map_ops.map_empty


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

module Int_ord = struct 
  type t = int 
  let compare (x:t) (y:t) = Stdlib.compare x y 
end

module Map_int = Map.Make(Int_ord)


module String_ord = struct 
  type t = string
  let compare (x:t) (y:t) = Stdlib.compare x y 
end

module Map_string = Map.Make(String_ord)


(* assoc list as map ------------------------------------------------ *)

let make_assoc_list_map () =
  let remove k kvs = List.filter (fun (k',v) -> k' <> k) kvs in
  { map_empty=[];
    map_is_empty=(fun x -> x=[]);
    map_add=(fun k v t -> (k,v)::(remove k t));
    map_remove=(fun k t -> remove k t);
    map_find=(fun k t -> List.assoc_opt k t);
    map_bindings=(fun t -> t); 
  }

let _ = make_assoc_list_map
