(** LRU based on pqwy *)

module Lru_ops = struct

  (** Functional LRU following pqwy, but with every entry having weight 1 *)
  type ('k,'v,'t) lru_ops = {
    empty    : int -> 't;
    is_empty : 't -> bool;
    capacity : 't -> int;
    size     : 't -> int;  (* may be over cap *)
    find     : 'k -> 't -> 'v option;
    insert   : 'k -> 'v -> 't -> 't;
    delete   : 'k -> 't -> 't;
    promote  : 'k -> 't -> 't;
    trim_1   : 't -> (('k*'v) * 't)option;
    trim     : 't -> ('k * 'v)list * 't; (* remove LRU entries till below cap *)
  }

end

type ('k,'v,'t) lru_ops = ('k,'v,'t) Lru_ops.lru_ops


(** Functional interface *)
module F_ops = Lru_ops

(** FIXME make clear this is functional LRU *)
module Make_lru(K:Stdlib.Map.OrderedType)(V:sig type t end) = struct

  module Internal = struct
    module V' = struct
      include V
      let weight _ = 1
    end

    module Lru = Lru.F.Make(K)(V')

    include Lru

    let insert k v t = Lru.add k v t

    let delete k t = Lru.remove k t

    let trim_1 t = Lru.pop_lru t
  end
  open Internal

  open Lru_ops

  (* to make self-contained at top level *)
  type t = Internal.Lru.t

  let lru_ops = 
    let trim t = 
      let rec f (acc,t) = 
        match size t > capacity t with
        | true -> (
            pop_lru t |> function
            | None -> (acc,t)
            | Some(kv,t) -> f (kv::acc,t))
        | false -> (acc,t)
      in
      f ([],t) |> fun (acc,t) -> (List.rev acc,t)
    in
    { empty; is_empty; capacity; size; find; insert; delete; promote; trim_1; trim }

end

(** Like Pqwy Lru, but with some small renamings, weights ignored
   (everything has weight 1) and a parametric type for the operations. *)
module Mutable = struct

  type ('k,'v,'t) lru_ops = (module Lru.M.S with type k = 'k and type v = 'v and type t = 't)

  module type K = Stdlib.Hashtbl.HashedType

  module type S = sig
    type k
    include K with type t := k
    type v
  end

  module type T = sig
    type k
    type v
    type t
    val lru_ops : (k,v,t) lru_ops
  end

  module Make(S:S) : T with type k = S.k and type v = S.v = struct
    include S
    module K = struct include S type t = k end
    module V = struct
      type t = v
      let weight _ = 1 (* every entry has weight 1 *)
    end
    module Lru = Lru.M.Make(K)(V)
    type t = Lru.t
    let lru_ops : _ lru_ops = (module Lru)
  end

  module type S' = sig
    type k
    type v
  end

  module Make_with_pervasives(S':S') = struct
    include Make(struct 
        include S' 
        let equal (x:k) (y:k) = (x=y)
        let hash (x:k) = Hashtbl.hash x
      end)
  end
end
