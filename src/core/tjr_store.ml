(* a generic functional store ---------------------------------------- *)

(* FIXME needed? *)
module Int = struct
  type t = int 
  let compare: t -> t -> int = Pervasives.compare 
end

module Map_int = Map.Make(Int)  (* or use Tjr_map.Map_int *)

module STORE : sig
  type 'a r = private int  (* ref *)
  val of_int: int -> 'a r
  val to_int: 'a r -> int
end = struct
  type 'a r = int
  let of_int: int -> 'a r = fun n -> (n :> 'a r)
  let to_int n = n
end

open STORE

(* universal type *)
type univ

type t = { map: univ Map_int.t; free: int }

let mk_ref: 'a. 'a -> t -> (t * 'a r) = 
  fun x t -> 
    let r = t.free in
    let free = r+1 in
    let map = Map_int.add r (Obj.magic x) t.map in
    ({map;free},of_int r)

let set: 'a. 'a r -> 'a -> t -> t = 
  fun r x t ->
    let map = Map_int.add (to_int r) (Obj.magic x) t.map in
    {t with map=map}

let get: 'a. 'a r -> t -> 'a = 
  fun r t ->
    Obj.magic(Map_int.find (to_int r) t.map)


(* FIXME we may want a more sophisticated version which stores the
   type of the values in the map, alongside the values
   themselves... or perhaps if we want this our values should be pairs
   with the first argument the type representation... or use a GADT *)
