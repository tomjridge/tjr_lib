(* NOTE there is another version of polymap, in its own repo
   tjr_polymap; it has a dependence on extlib, whereas this is
   dependency free *)

(* FIXME add more of the stdlib operations as needed *)
type ('k,'v,'t) map_ops = {
  k_cmp: 'k -> 'k -> int;
  empty: 't;
  is_empty:'t -> bool;
  mem:'k -> 't -> bool;
  add:'k -> 'v -> 't -> 't;
  remove:'k -> 't -> 't;
  cardinal: 't -> int;
  bindings: 't -> ('k*'v) list;
  of_bindings: ('k*'v) list -> 't;
  max_binding_opt: 't -> ('k*'v)option;
  min_binding_opt: 't -> ('k*'v)option;
  split:'k -> 't -> 't * 'v option * 't;
  find: 'k -> 't -> 'v;
  find_opt: 'k -> 't -> 'v option;
  update: 'k -> ('v option -> 'v option) -> 't -> 't;
  disjoint_union: 't -> 't -> 't;
  find_first_opt: ('k -> bool) -> 't -> ('k * 'v) option;
  find_last_opt: ('k -> bool) -> 't -> ('k * 'v) option;
}

(** Functor to make map ops; generates a new impl type 't *)
module Make_map_ops(Ord: Map.OrderedType) = struct
  include Map.Make(Ord)
  let disjoint_union t1 t2 = 
    let f = fun k v1 v2 -> failwith "disjoint_union: duplicate key" in
    union f t1 t2
  let of_bindings kvs = 
    kvs |> List.to_seq |> of_seq
  let k_cmp = Ord.compare
  let map_ops = { 
    k_cmp; empty; is_empty; mem; add; remove; cardinal;
    bindings; max_binding_opt; min_binding_opt; split; find; find_opt;
    update; disjoint_union; of_bindings; find_first_opt; find_last_opt }
end

(** To avoid functors, we introduce a phantom type for a default
   implementation of map using OCaml's built in map. NOTE for every
   'k,'v,k_cmp... 't should be an abstract type about which nothing is
   known (eg not unit or anything like that... otherwise different
   k_cmp orderings might have impls with the same type). The user of
   this library has to provide the 't *)
type ('k,'v,'t) map  (* phant type iso to 'v Map.Make(K,k_cmp).t *)

let make_map_ops (type k v t) k_cmp : (k,v,(k,v,t) map)map_ops =
  let module M = Make_map_ops(struct type t = k let compare=k_cmp end) in
  let to_t (x:v M.t) : (k,v,t) map = Obj.magic x in
  let from_t (x:(k,v,t) map) : v M.t = Obj.magic x in
  let empty = M.empty |> to_t in
  let is_empty x = x |> from_t |> M.is_empty in
  let mem x s = M.mem x (from_t s) in
  let add k v t = M.add k v (from_t t) |> to_t in
  let remove k t = M.remove k (from_t t) |> to_t in
  let cardinal t = M.cardinal (from_t t) in
  let bindings t = M.bindings (from_t t) in
  let max_binding_opt t = M.max_binding_opt (from_t t) in
  let min_binding_opt t = M.min_binding_opt (from_t t) in
  let split k t = M.split k (from_t t) |> fun (t1,k,t2) -> (to_t t1,k,to_t t2) in
  let find k t = M.find k (from_t t) in
  let find_opt k t = M.find_opt k (from_t t) in
  let update k f t = M.update k f (from_t t) |> to_t in
  let disjoint_union t1 t2 = M.disjoint_union (from_t t1) (from_t t2) |> to_t in
  let of_bindings kvs = M.of_bindings kvs |> to_t in
  let find_first_opt p t = M.find_first_opt p (from_t t) in
  let find_last_opt p t = M.find_last_opt p (from_t t) in
  { k_cmp; empty; is_empty; mem; add; remove; cardinal; bindings;
    max_binding_opt; min_binding_opt; split; find; find_opt; update;
    disjoint_union; of_bindings; find_first_opt; find_last_opt }
  

let _ = make_map_ops


(** Make maps ops based on pervasives_compare; also provides ops bound
   at module level (rather than in a record). Bit of a hack. *)
module With_pervasives_compare = struct

  type ('k,'v) map_with_pervasives_compare

  (* NOTE the following uses polymorphic comparison!!! *)
  let poly_map_ops_2 (type k v) () : (k,v,(k,v) map_with_pervasives_compare)map_ops =
    let k_cmp = Pervasives.compare in
    let module M = Make_map_ops(struct type t = k let compare=k_cmp end) in
    let to_t (x:v M.t) : (k,v) map_with_pervasives_compare = Obj.magic x in
    let from_t (x:(k,v) map_with_pervasives_compare) : v M.t = Obj.magic x in
    let empty = M.empty |> to_t in
    let is_empty x = x |> from_t |> M.is_empty in
    let mem x s = M.mem x (from_t s) in
    let add k v t = M.add k v (from_t t) |> to_t in
    let remove k t = M.remove k (from_t t) |> to_t in
    let cardinal t = M.cardinal (from_t t) in
    let bindings t = M.bindings (from_t t) in
    let max_binding_opt t = M.max_binding_opt (from_t t) in
    let min_binding_opt t = M.min_binding_opt (from_t t) in
    let split k t = M.split k (from_t t) |> fun (t1,k,t2) -> (to_t t1,k,to_t t2) in
    let find k t = M.find k (from_t t) in
    let find_opt k t = M.find_opt k (from_t t) in
    let update k f t = M.update k f (from_t t) |> to_t in
    let disjoint_union t1 t2 = M.disjoint_union (from_t t1) (from_t t2) |> to_t in
    let of_bindings kvs = M.of_bindings kvs |> to_t in
    let find_first_opt p t = M.find_first_opt p (from_t t) in
    let find_last_opt p t = M.find_last_opt p (from_t t) in
    { k_cmp; empty; is_empty; mem; add; remove; cardinal; bindings;
      max_binding_opt; min_binding_opt; split; find; find_opt; update;
      disjoint_union; of_bindings; find_first_opt; find_last_opt }

  let empty () = (poly_map_ops_2()).empty
  let is_empty x = (poly_map_ops_2()).is_empty x
  let mem x s = (poly_map_ops_2()).mem x s
  let add k v t = (poly_map_ops_2()).add k v t
  let remove k t = (poly_map_ops_2()).remove k t
  let cardinal t = (poly_map_ops_2()).cardinal t 
  let bindings t = (poly_map_ops_2()).bindings t 
  let max_binding_opt t = (poly_map_ops_2()).max_binding_opt t 
  let min_binding_opt t = (poly_map_ops_2()).min_binding_opt t 
  let split k t = (poly_map_ops_2()).split k t
  let find k t = (poly_map_ops_2()).find k t
  let find_opt k t = (poly_map_ops_2()).find_opt k t
  let update k f t = (poly_map_ops_2()).update k f t
  let disjoint_union t1 t2 = (poly_map_ops_2()).disjoint_union t1 t2
  let of_bindings kvs = (poly_map_ops_2()).of_bindings kvs
  let find_first_opt t = (poly_map_ops_2()).find_first_opt t
  let find_last_opt t = (poly_map_ops_2()).find_last_opt t
end
