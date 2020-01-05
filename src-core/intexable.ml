(** Values indexed by int are quite common. *)

type 'a intexable = int * 'a


(** Like a map, but the key is actually projected from the original
   key, and we need to provide an order on that key *)
module Map_with_projection_from_key = struct

  module Make(
      S: sig 
        type k
        type k'
        val project: k -> k'
        val compare: k' -> k' -> int
      end) : 
  sig 
    type 'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : S.k -> 'a t -> bool
    val add : S.k -> 'a -> 'a t -> 'a t
    val remove : S.k -> 'a t -> 'a t
    val bindings : 'a t -> (S.k' * 'a) list
    val find_opt : S.k -> 'a t -> 'a option
  end
  = struct
    module Map_from_k' = Map.Make(struct type t = S.k' let compare = S.compare end)
    module M = Map_from_k'
    let f = S.project
    type 'a t = 'a Map_from_k'.t
    let empty, is_empty = M.(empty,is_empty)
    open M
    let mem k t = mem (f k) t
    let add k v t = add (f k) v t
    let remove k t = remove (f k) t
    let bindings t = bindings t (* NOTE this will return k' keys *)
    let find_opt k t = find_opt (f k) t
  end
  
end

module Map_make(S:sig type a end) 
  : sig
    type 'a t 
    type k = S.a intexable
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : k -> 'a t -> bool
    val add : k -> 'a -> 'a t -> 'a t
    val remove : k -> 'a t -> 'a t
    val bindings : 'a t -> (int * 'a) list
    val find_opt : k -> 'a t -> 'a option
  end
= struct
  open S

  module Internal_ = struct
    module S = struct 
      type k = a intexable 
      type k' = int 
      let project : k -> k' = fst
      let compare: k' -> k' -> int = Pervasives.compare
    end

    module M = Map_with_projection_from_key.Make(S)
  end

  type k = Internal_.S.k
  include Internal_.M
end

