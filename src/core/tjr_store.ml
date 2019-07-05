(** A functional store. 

Because of set and get, this module should probably not be opened. *)

open Util

(** Internal implementation *)
module Internal = struct

  (* universal type *)
  type univ

  (* map to option - we might have uninitialized refs; we also have a
     "read_only" flag, which prevents further modification *)
  type fstore = { map: univ Map_int.t; free: int; urefs:(int*string) list; read_only:bool }

  type 'a m = fstore -> 'a * fstore


  type 'a ref_ = int

  let empty_fstore = { map=Map_int.empty; free=0; urefs=[]; read_only=false }

  let fail_if_ro t = 
    match t.read_only with 
    | true -> failwith (
      Printf.sprintf "%s: attempt to modify store, but store is read-only" __LOC__)
    | false -> ()
    
                    
  let mk_ref: 'a. 'a -> 'a ref_ m = 
    fun x t -> 
    fail_if_ro t;
    let r = t.free in
    let free = r+1 in
    let map = Map_int.add r (Obj.magic x) t.map in
    (r,{t with map;free})


  let set: 'a. 'a ref_ -> 'a -> unit m = 
    fun r x t ->
    fail_if_ro t;
    let x' : univ = Obj.magic x in
    let map = Map_int.add r x' t.map in
    ((),{t with map})


  let get (r:'a ref_) : 'a m = 
    fun t ->
    try       
      let v : 'a = Obj.magic(Map_int.find r t.map) in
      (v,t)
    with Not_found ->
      Printf.printf "%s : unknown key: %d \n%!" __LOC__ r;
      failwith __LOC__

  let _ = get

  

  (** Uninitialised references, with a string name *)
  module Uninitialized_refs = struct 

    (** Uninitialised references, with a string name *)
    let mk_uref ~name t =
      let r : 'a ref_ = t.free in
      let free = r+1 in
      (r,{t with free;urefs=(r,name)::t.urefs})

    let uref_initialized u t = 
      Map_int.find_opt u t.map |> function
      | None -> false
      | Some _ -> true

    (** Check that all urefs have been initialized *)
    let urefs_status t = 
      t.urefs |> List_.map (fun (u,name) -> 
        (name,uref_initialized u t))

    let _ = urefs_status


    let urefs_initialized ~print t : bool =
      t |> urefs_status |> List.partition (fun (n,b) -> b) |> fun (yes,no) -> 
      (if print then no |> List.iter (fun (n,_) -> Printf.printf "uref uninitialised: %s\n" n));
      match no with
      | [] -> true
      | _ -> false

    (* FIXME we may want a more sophisticated version which stores the
       type of the values in the map, alongside the values
       themselves... or perhaps if we want this our values should be pairs
       with the first argument the type representation... or use a GADT *)
  end

  include Uninitialized_refs

  
end
(* open Internal *)

module With_sig : sig
  type fstore 
  type 'a ref_ 
  val empty_fstore : fstore
  val mk_ref : 'a -> fstore -> 'a ref_ * fstore
  val set : 'a ref_ -> 'a -> fstore -> fstore
  val get : 'a ref_ -> fstore -> 'a
  val mk_uref : name:string -> fstore -> int * fstore
  val uref_initialized : 'a ref_ -> fstore -> bool
  val urefs_status : fstore -> (string * bool) list
  val urefs_initialized : print:bool -> fstore -> bool
end = struct
  include Internal
      
  let set r v s = set r v s |> snd

  let get r s = get r s |> fst
end
(* open With_sig *)

(* let empty_fstore = empty_fstore *)

include With_sig



(*
type ref_ops = {
  mk_ref: 'a. 'a -> fstore -> 'a ref_ * fstore;
  set: 'a. 'a ref_ -> 'a -> fstore -> fstore;
  get: 'a. 'a ref_ -> fstore -> 'a
}

type uref_ops = {
  mk_uref : name:string -> fstore -> int * fstore;
  urefs_status : fstore -> (string * bool) list;
  uref_initialized : 'a. 'a ref_ -> fstore -> bool;
  urefs_initialized : print:bool -> fstore -> bool;
}  

let ref_ops = { mk_ref; set; get }
let uref_ops = { mk_uref; urefs_status; uref_initialized; urefs_initialized }
*)


(** An imperative version; the functional store can be accessed via a normal ocaml reference *)
module Make_imperative_fstore() = struct

  let fstore = ref empty_fstore

  let ref x = 
    mk_ref x !fstore |> (fun (r,s) -> 
      fstore := s;
      r)

  let mk_uref ~name = 
    mk_uref ~name !fstore |> (fun (r,s) -> 
      fstore := s;
      r)

  let urefs_initialized ~print =
    urefs_initialized ~print !fstore

  let ( := ) r v = 
    fstore := set r v !fstore

  let (!) r = 
    get r !fstore
end
