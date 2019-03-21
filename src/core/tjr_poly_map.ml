
(* NOTE there is another version of polymap, in its own repo
   tjr_polymap; it has a dependence on extlib, whereas this is
   dependency free *)

(* FIXME add more of these operations as needed *)
type ('k,'v,'t) map_ops = {
  empty: 't;
  is_empty:'t -> bool;
  mem:'k -> 't -> bool;
  add:'k -> 'v -> 't -> 't;
  remove:'k -> 't -> 't;
  cardinal: 't -> int;
  bindings: 't -> ('k*'v) list;
  max_binding_opt: 't -> ('k*'v)option;
  min_binding_opt: 't -> ('k*'v)option;
  split:'k -> 't -> 't * 'v option * 't;
  find: 'k -> 't -> 'v;
  find_opt: 'k -> 't -> 'v option;
}

module Make(Ord: Map.OrderedType) = struct
  include Map.Make(Ord)
  let map_ops = { empty; is_empty; mem; add; remove; cardinal; bindings; max_binding_opt; min_binding_opt; split; find; find_opt }
end

(* to expose polymorphic operations *)

(** NOTE for every 'k,'v,k_cmp... 't should be an abstract type about
   which nothing is known (eg not unit or anything like that) *)
type ('k,'v,'t) map  (* phant type iso to 'v Map.Make(K,k_cmp).t *)

let make_map_ops (type k v t) k_cmp : (k,v,(k,v,t) map)map_ops =
  let module M = Map.Make(struct type t = k let compare=k_cmp end) in
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
  { empty; is_empty; mem; add; remove; cardinal; bindings; max_binding_opt; min_binding_opt; split; find; find_opt }
  

let _ = make_map_ops


(* even more weakly typed *)

type ('k,'v) poly_map_2

let poly_map_ops_2 (type k v) () : (k,v,(k,v) poly_map_2)map_ops =
  let module M = Map.Make(struct type t = k let compare=Pervasives.compare end) in
  let to_t (x:v M.t) : (k,v) poly_map_2 = Obj.magic x in
  let from_t (x:(k,v) poly_map_2) : v M.t = Obj.magic x in
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
  { empty; is_empty; mem; add; remove; cardinal; bindings; max_binding_opt; min_binding_opt; split; find; find_opt }

let is_empty x = (poly_map_ops_2()).is_empty x
let mem x s = (poly_map_ops_2()).mem x s
let add k v t = (poly_map_ops_2()).add k v t
let remove k t = (poly_map_ops_2()).remove k t
let cardinal t = (poly_map_ops_2()).cardinal t 
let bindings t = (poly_map_ops_2()).bindings t 
let max_binding_opt t = (poly_map_ops_2()).max_binding_opt t 
let min_binding_opt t = (poly_map_ops_2()).min_binding_opt t 
let split k t = (poly_map_ops_2()).split k t
let find_opt k t = (poly_map_ops_2()).find_opt k t
