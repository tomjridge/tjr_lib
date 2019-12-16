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

module Make(K:Stdlib.Map.OrderedType)(V:sig type t end) = struct

  module V' = struct
    include V
    let weight _ = 1
  end

  module Internal = struct
    module Lru = Lru.F.Make(K)(V')

    include Lru

    let insert k v t = Lru.add k v t

    let delete k t = Lru.remove k t

    let trim_1 t = Lru.pop_lru t
  end
  open Internal

  open Lru_ops

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

