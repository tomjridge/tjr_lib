(* NOTE there is another version of polymap, in its own repo
   tjr_polymap; it has a dependence on extlib, whereas this is
   dependency free *)

(* FIXME add more of the stdlib operations as needed; don't add extra
   operations... prefer to write the extra operations in this file
   (?why?) *)
type ('k,'v,'t) map_ops = {
  k_cmp           : 'k -> 'k -> int;
  empty           : 't;
  is_empty        : 't -> bool;
  mem             : 'k -> 't -> bool;
  add             : 'k -> 'v -> 't -> 't;
  remove          : 'k -> 't -> 't;
  cardinal        : 't -> int;
  bindings        : 't -> ('k*'v) list;
  of_bindings     : ('k*'v) list -> 't;  (* FIXE allow duplicates? *)
  max_binding_opt : 't -> ('k*'v)option;
  min_binding_opt : 't -> ('k*'v)option;
  split           : 'k -> 't -> 't * 'v option * 't;
  find            : 'k -> 't -> 'v;
  find_opt        : 'k -> 't -> 'v option;
  update          : 'k -> ('v option -> 'v option) -> 't -> 't;
  disjoint_union  : 't -> 't -> 't;
  find_first_opt  : ('k -> bool) -> 't -> ('k * 'v) option;
  find_last_opt   : ('k -> bool) -> 't -> ('k * 'v) option;
  iter            : ('k -> 'v -> unit) -> 't -> unit;
  map             : ('v -> 'v) -> 't -> 't;  (* NOTE less general than stdlib *)
  (* merge        : older:'t -> newer:'t -> 't; FIXME why don't we include this? *)  
}

let map_merge ~map_ops ~old ~new_ =
  (* let open Tjr_map in *)
  map_ops.bindings new_ |> fun kvs ->
  (kvs,old) |> Util.iter_k (fun ~k:kont (kvs,merged) ->
      match kvs with
      | [] -> merged
      | (k,v)::kvs -> 
        kont (kvs,map_ops.add k v merged))

module type S = sig
  type k
  type v
  val k_cmp: k -> k -> int
end

module type T = sig
  type k
  type v
  type t
  val map_ops: (k,v,t)map_ops
end

(** Functor to make map ops; generates a new impl type 't *)
module Make_map_ops(S:S) : T with type k=S.k and type v=S.v = struct
  include S
  module Map = Map.Make(struct type t = k let compare = S.k_cmp end)
  open Map
  type t = v Map.t
  let disjoint_union t1 t2 = 
    let f = fun k v1 v2 -> failwith "disjoint_union: duplicate key" in
    union f t1 t2
  let of_bindings kvs = 
    kvs |> List.to_seq |> of_seq
  let map_ops = { 
    k_cmp; empty; is_empty; mem; add; remove; cardinal;
    bindings; max_binding_opt; min_binding_opt; split; find; find_opt;
    update; disjoint_union; of_bindings; find_first_opt; find_last_opt; iter; map }
end

(** To avoid functors, we introduce a phantom type for a default
   implementation of map using OCaml's built in map. NOTE for every
   'k,'v,k_cmp... 't should be an abstract type about which nothing is
   known (eg not unit or anything like that... otherwise different
   k_cmp orderings might have impls with the same type). The user of
   this library has to provide the 't *)
type ('k,'v,'t) map  (* phant type iso to 'v Map.Make(K,k_cmp).t *)

(** NOTE the following is unsafe, because the type t is unconstrained;
   it is up to you to make sure that this doesn't cause problems. If
   in doubt, use the functor version *)
let unsafe_make_map_ops (type k v t) k_cmp : (k,v,(k,v,t) map)map_ops =
  let module M = Make_map_ops(struct type nonrec k=k type nonrec v=v let k_cmp=k_cmp end) in
  let ops = M.map_ops in
  let to_t (x:M.t) : (k,v,t) map = Obj.magic x in
  let from_t (x:(k,v,t) map) : M.t = Obj.magic x in
  let empty = ops.empty |> to_t in
  let is_empty x = x |> from_t |> ops.is_empty in
  let mem x s = ops.mem x (from_t s) in
  let add k v t = ops.add k v (from_t t) |> to_t in
  let remove k t = ops.remove k (from_t t) |> to_t in
  let cardinal t = ops.cardinal (from_t t) in
  let bindings t = ops.bindings (from_t t) in
  let max_binding_opt t = ops.max_binding_opt (from_t t) in
  let min_binding_opt t = ops.min_binding_opt (from_t t) in
  let split k t = ops.split k (from_t t) |> fun (t1,k,t2) -> (to_t t1,k,to_t t2) in
  let find k t = ops.find k (from_t t) in
  let find_opt k t = ops.find_opt k (from_t t) in
  let update k f t = ops.update k f (from_t t) |> to_t in
  let disjoint_union t1 t2 = ops.disjoint_union (from_t t1) (from_t t2) |> to_t in
  let of_bindings kvs = ops.of_bindings kvs |> to_t in
  let find_first_opt p t = ops.find_first_opt p (from_t t) in
  let find_last_opt p t = ops.find_last_opt p (from_t t) in
  let iter f t = ops.iter f (from_t t) in
  let map f t = ops.map f (from_t t) |> to_t in
  { k_cmp; empty; is_empty; mem; add; remove; cardinal; bindings;
    max_binding_opt; min_binding_opt; split; find; find_opt; update;
    disjoint_union; of_bindings; find_first_opt; find_last_opt; iter; map }
  
let _ = unsafe_make_map_ops

(** Make maps ops based on pervasives_compare; also provides ops bound
   at module level (rather than in a record). Bit of a hack. *)
module With_stdcmp = struct
  type ('k,'v)stdmap

  (* NOTE the following uses polymorphic comparison!!! *)
  let poly_map_ops_2 (type k v) () : (k,v,(k,v)stdmap)map_ops =
    let k_cmp = Stdlib.compare in
  let module M = Make_map_ops(struct type nonrec k=k type nonrec v=v let k_cmp=k_cmp end) in
  let ops = M.map_ops in
  let to_t (x:M.t) : (k,v)stdmap = Obj.magic x in
  let from_t (x:(k,v)stdmap) : M.t = Obj.magic x in
  let empty = ops.empty |> to_t in
  let is_empty x = x |> from_t |> ops.is_empty in
  let mem x s = ops.mem x (from_t s) in
  let add k v t = ops.add k v (from_t t) |> to_t in
  let remove k t = ops.remove k (from_t t) |> to_t in
  let cardinal t = ops.cardinal (from_t t) in
  let bindings t = ops.bindings (from_t t) in
  let max_binding_opt t = ops.max_binding_opt (from_t t) in
  let min_binding_opt t = ops.min_binding_opt (from_t t) in
  let split k t = ops.split k (from_t t) |> fun (t1,k,t2) -> (to_t t1,k,to_t t2) in
  let find k t = ops.find k (from_t t) in
  let find_opt k t = ops.find_opt k (from_t t) in
  let update k f t = ops.update k f (from_t t) |> to_t in
  let disjoint_union t1 t2 = ops.disjoint_union (from_t t1) (from_t t2) |> to_t in
  let of_bindings kvs = ops.of_bindings kvs |> to_t in
  let find_first_opt p t = ops.find_first_opt p (from_t t) in
  let find_last_opt p t = ops.find_last_opt p (from_t t) in
  let iter f t = ops.iter f (from_t t) in
  let map f t = ops.map f (from_t t) |> to_t in
  { k_cmp; empty; is_empty; mem; add; remove; cardinal; bindings;
    max_binding_opt; min_binding_opt; split; find; find_opt; update;
    disjoint_union; of_bindings; find_first_opt; find_last_opt; iter; map }

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
  let iter f t = (poly_map_ops_2()).iter f t
  let map f t = (poly_map_ops_2()).map f t
end

(** With_base and With_base_as_record are used in
   Isa_btree.Isa_export_wrapper; at the time, Base provided some
   functionality that was missing in Stdlib. Probably this
   functionality is now there, and we can drop the dependence on Base
   *)

module With_base = struct

  (** A first-order approach *)

  (** Common *)
  module type Map1 = sig
    type k
    type v
    type t
    val k_cmp       : k -> k -> int
    val empty       : t
    val is_empty    : t -> bool
    val mem         : k -> t -> bool
    val add         : k -> v -> t -> t
    val remove      : k -> t -> t
    val cardinal    : t -> int
    val bindings    : t -> (k*v) list
    val of_bindings : (k*v) list -> t  (* FIXE allow duplicates? *)
    val find        : k -> t -> v
    val find_opt    : k -> t -> v option
  end

  (** Less common *)
  module type Map2 = sig
    include Map1
    val max_binding_opt: t -> (k*v)option
    val min_binding_opt: t -> (k*v)option
    val split:k -> t -> t * v option * t
    val update: k -> (v option -> v option) -> t -> t
    val disjoint_union: t -> t -> t
    (* val get_next_binding: k -> t -> (k*v)option *)
    (* val get_prev_binding: k -> t -> (k*v)option *)
    val closest_key: 
      [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to 
      | `Less_than ] -> 
      k -> t -> (k*v)option
  end

  open Base.Map

  (* this is the general type; the function below makes a particular
     implementation using Base.Map.t *)
  type ('k,'v,'t) map_ops = 
    (module Map2 with
      type k = 'k and type t = 't and type v = 'v)


  let make_map_ops (type k v cmp) (cmp:(k,cmp)comparator) : (k,v,(k,v,cmp)Base.Map.t)map_ops = 
    let module A = struct

      type nonrec k = k
      type nonrec v = v

      type t = (k,v,cmp) Base.Map.t
   
      let k_cmp = 
        let module S = (val cmp) in
        S.comparator.compare
          
      let _ = k_cmp
      
      let empty = empty cmp
          
      let is_empty = is_empty

      let mem x s = mem s x

      let add k v s = set s ~key:k ~data:v
          
      let remove k s = remove s k

      let cardinal s = length s

      let bindings s = to_alist s

      (** FIXME NOTE we throw an exception if there are duplicate elts *)
      let of_bindings kvs = of_alist_exn cmp kvs

      let find k t = find_exn t k

      let find_opt k t = Base.Map.find t k

      let max_binding_opt s = max_elt s

      let min_binding_opt s = min_elt s

      let split k t = 
        Base.Map.split t k |> fun (t1,kv,t2) -> 
        match kv with
        | None -> (t1,None,t2)
        | Some(k,v) -> (t1,Some v,t2)

      let update k f t =
        change t k ~f
          
      let disjoint_union t1 t2 =
        merge t1 t2 ~f:(fun ~key vs ->
            match vs with
            | `Left v1 -> Some v1
            | `Right v2 -> Some v2
            | `Both (v1,v2) -> failwith __LOC__)

(*
      let get_next_binding k t = failwith""
      let get_prev_binding k t = failwith""
*)
      let closest_key x k t = closest_key t x k

    end
    in
    (module A : Map2 with type k=k and type v=v and type t=(k,v,cmp) Base.Map.t)

  let _ = make_map_ops

end    

module With_base_as_record = struct

  module Map_ops_type = struct
    type ('k,'v,'t) map_ops = {
      k_cmp: 'k -> 'k -> int;
      empty: 't;
      is_empty:'t -> bool;
      mem:'k -> 't -> bool;
      add:'k -> 'v -> 't -> 't;
      remove:'k -> 't -> 't;
      cardinal: 't -> int;
      bindings: 't -> ('k*'v) list;
      of_bindings: ('k*'v) list -> 't;  (* FIXE allow duplicates? *)
      find: 'k -> 't -> 'v;
      find_opt: 'k -> 't -> 'v option;
      max_binding_opt: 't -> ('k*'v)option;
      min_binding_opt: 't -> ('k*'v)option;
      split:'k -> 't -> 't * 'v option * 't;
      update: 'k -> ('v option -> 'v option) -> 't -> 't;
      disjoint_union: 't -> 't -> 't;
      (* get_next_binding: 'k -> 't -> ('k*'v)option; *)
      (* get_prev_binding: 'k -> 't -> ('k*'v)option; *)
      (* find_first_opt: ('k -> bool) -> 't -> ('k * 'v) option; *)
      (* find_last_opt: ('k -> bool) -> 't -> ('k * 'v) option; *)
      closest_key: 
        [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to 
        | `Less_than ] -> 
        'k -> 't -> ('k*'v)option
    }
  end
  include Map_ops_type

  open With_base 

  let make_map_ops (type k v cmp) (cmp:(k,cmp)Base.Map.comparator) = 
    let m : (module Map2 with type k=k and type v=v and type t = (k, v, cmp) Base.Map.t) = With_base.make_map_ops cmp in
    let (module M) = m in
    let open M in
    { k_cmp; empty; is_empty; mem; add; remove; cardinal; bindings; of_bindings;
      find; find_opt; max_binding_opt; min_binding_opt; split; update; disjoint_union;
      closest_key }

  let _ = make_map_ops
    
  

end


(*

module With_singleton = struct
  open Singleton_type

  (* type ('k,'k_cmp,'v) map *)

  type 'k_cmp map_repr

  (** make_map_ops in Tjr_map leaves map repr type free; this just
     identifies it with 'k_cmp repr, where 'k_cmp is a singleton type
     representing the order *)
  let make_map_ops (type k v k_cmp) ~(k_cmp: (k_cmp,k->k->int) sng) 
    : (k,v,(k,v,k_cmp map_repr)map)map_ops 
    = 
    let k_cmp : k -> k -> int = dest_sng k_cmp in
    let map_ops (* : (k,v,(k,v,k_cmp)map) map_ops *) = make_map_ops k_cmp in
    map_ops
    

  let _ = make_map_ops
end
*)




(*
    let open A in
    { k_cmp; empty; is_empty; mem; add; remove; cardinal; bindings;
    max_binding_opt; min_binding_opt; split; find; find_opt; update;
    disjoint_union; of_bindings }
*)



(*
  type ('k,'v,'t) map_ops = {
    k_cmp: 'k -> 'k -> int;
    empty: 't;
    is_empty:'t -> bool;
    mem:'k -> 't -> bool;
    add:'k -> 'v -> 't -> 't;
    remove:'k -> 't -> 't;
    cardinal: 't -> int;
    bindings: 't -> ('k*'v) list;
    of_bindings: ('k*'v) list -> 't;  (* FIXE allow duplicates? *)
    max_binding_opt: 't -> ('k*'v)option;
    min_binding_opt: 't -> ('k*'v)option;
    split:'k -> 't -> 't * 'v option * 't;
    find: 'k -> 't -> 'v;
    find_opt: 'k -> 't -> 'v option;
    update: 'k -> ('v option -> 'v option) -> 't -> 't;
    disjoint_union: 't -> 't -> 't;
    get_next_binding: 'k -> 't -> ('k*'v)option;
    get_prev_binding: 'k -> 't -> ('k*'v)option;
    (* find_first_opt: ('k -> bool) -> 't -> ('k * 'v) option; *)
    (* find_last_opt: ('k -> bool) -> 't -> ('k * 'v) option; *)
  }
*)
