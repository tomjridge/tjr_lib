(* a generic functional store ---------------------------------------- *)

(* FIXME needed? *)
module Int = struct
  type t = int 
  let compare: t -> t -> int = Pervasives.compare 
end

module Map_int = Map.Make(Int)  (* or use Tjr_map.Map_int *)

module Refs : sig
  type 'a r = private int  (* ref *)
  val of_int: int -> 'a r
  val to_int: 'a r -> int
end = struct
  type 'a r = int
  let of_int: int -> 'a r = fun n -> (n :> 'a r)
  let to_int n = n
end
open Refs

(* universal type *)
type univ


module Store : sig
  type t = { map: univ Map_int.t; free: int; urefs:(int * string) list }
end = struct
  type t = { map: univ Map_int.t; free: int; urefs:(int * string) list }
end
include Store


let initial_store = { map=Map_int.empty; free=0; urefs=[] }

let mk_ref: 'a. 'a -> t -> (t * 'a r) = 
  fun x t -> 
    let r = t.free in
    let free = r+1 in
    let map = Map_int.add r (Obj.magic x) t.map in
    ({map;free;urefs=t.urefs},of_int r)

let set: 'a. 'a r -> 'a -> t -> t = 
  fun r x t ->
    let map = Map_int.add (to_int r) (Obj.magic x) t.map in
    {t with map}

let get: 'a. 'a r -> t -> 'a = 
  fun r t ->
    try       
      Obj.magic(Map_int.find (to_int r) t.map)
    with Not_found ->
      Printf.printf "%s : unknown key: %d \n%!" __LOC__ (to_int r);
      failwith __LOC__
        


(** Uninitialised references, with a string name *)
module Unsafe = struct 

  (** Uninitialised references, with a string name *)
  let mk_uref name t =
    let r = t.free in
    let free = r+1 in
    ({t with free;urefs=(r,name)::t.urefs},of_int r)

  (** Check that all urefs have been initialized *)
  let check_urefs ~print t = 
    t.urefs |> List.map (fun (r,n) ->
      Map_int.find_opt r t.map |> fun v -> 
      let v' = (v <> None) in
      (match print with
       | true -> 
         Printf.printf "uref %d %s: %B\n%!" r n v'
       | false -> ());
      (r,n,v'))


  let all_urefs_initialized t : bool =
    check_urefs ~print:false t 
    |> List.map (fun (r,n,v) -> v) 
    |> List.for_all (fun x -> x)

  (* FIXME we may want a more sophisticated version which stores the
     type of the values in the map, alongside the values
     themselves... or perhaps if we want this our values should be pairs
     with the first argument the type representation... or use a GADT *)

end
