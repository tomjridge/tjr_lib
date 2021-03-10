(** A two-generation LRU with batch eviction. Compared to v2, this is based directly on mutable hashtables.

This tries to take advantage of the extremely good performance of
   hashtables.

We assume a limit N on the number of most-recent items we want to
   track.

The idea is to maintain two maps (hence "two generations"). The first
   is of size n1 <=N, and contains the n1 most recently used
   elements. The second is typically of size N, and the next (N-n1)
   most-recent elements are contained in this set. In the case where
   the set is of size n2 < N, then only the most recent n1+n2 items
   are tracked (FIXME?)

When the first map reaches the limit N, it is demoted to the second
   map, and the second map is dropped. At this point, we need to
   expunge multiple elements from the cache, so this is "batch
   eviction".

Looking up an item consults the first map first, then the second, then
   the underlying map.

Compared to standard LRU, the implementation is likely simpler and
   faster. Of course, you don't have full information about the
   ordering of the most-recently used items. But in the case where you
   are maintaining a cache, this doesn't matter so much.

NOTE a quick google search finds that this may be similar:
   https://github.com/dominictarr/hashlru , with some benchmarking
   here: https://github.com/dominictarr/bench-lru

Don't open - record field clashes, type clashes

 *)
open Tjr_monad


(** {2 Version without delete moved to OLD 2021-03-10} *)

(* --------------------------------------------------------------------- *)

(* FIXME move *)
module Hashtbl = struct
  include Hashtbl
  (** merge m1 and m2, creating a new tbl (and leaving originals
     unaffected); prefer bindings from m1 *)
  let merge ~m1 ~m2 = 
    (* copy m2 *)
    Hashtbl.copy m2 |> fun tbl -> 
    (* update with newer entries from m1; FIXME could also use
         filter_map_inplace, which might be faster *)
    m1 |> Hashtbl.iter (fun k v -> Hashtbl.replace tbl k v);
    tbl

  let to_list tbl = tbl |> Hashtbl.to_seq |> List.of_seq
end
    


(** {2 Version with delete} *)

(** The unknown implementation for the lower map - just find_opt *)
module Map_lower = struct
  type ('k,'v,'t) map_lower = {
    find_opt : 'k -> ('v option,'t) m;
    (* insert   : 'k -> 'v -> (unit,'t) m; *)
  }
end
include Map_lower


(** Entries in the internal maps can record that an entry has been
   deleted, or that an entry is present in the lower map m3; 

{[ type 'v entry = [ `Inserted of 'v | `Deleted | `Lower_some of 'v | `Lower_none ] ]}

*)
module Entry = struct
  type 'v entry = [ `Inserted of 'v | `Deleted | `Lower_some of 'v | `Lower_none ]

  type 'v dirty_entry = [ `Inserted of 'v | `Deleted ]
end
include Entry



(** Cache state; internal *)
module Cache_state = struct
  type ('k,'v) cache_state = {
    max_sz: int;
    mutable m1 : ('k,'v entry)Hashtbl.t;
    mutable m2 : ('k,'v entry)Hashtbl.t;
  }
  (** INVARIANT because of the implementation, there can't be a dirty
     entry in m2, but a clean entry in m1 (proof: how could the clean
     entry have gotten there?) *)

  let create_initial_cache ~max_sz = 
    let _ = assert(max_sz >= 1) in
    let m1 : ('k,'v entry)Hashtbl.t = Hashtbl.create max_sz in
    let m2 : ('k,'v entry)Hashtbl.t = Hashtbl.create max_sz in
    {max_sz;m1;m2}

  (** Convert dirty entries to clean, but keep track of dirties *)
  let clean_tbl ~dirties (tbl:('k,'v entry)Hashtbl.t) =
    tbl |> Hashtbl.filter_map_inplace (fun k v -> 
        match v with
        | `Inserted v -> (
            Hashtbl.replace dirties k (`Inserted v);
            Some (`Lower_some v))
        | `Deleted -> (
            Hashtbl.replace dirties k `Deleted;
            Some `Lower_none)
        | `Lower_some v -> Some (`Lower_some v)
        | `Lower_none -> Some `Lower_none)

  let clean c = 
    let dirties = Hashtbl.create (Hashtbl.length c.m1) in
    clean_tbl ~dirties c.m2;
    clean_tbl ~dirties c.m1;
    dirties |> Hashtbl.to_list

  let _ = clean


  let tbl_to_dirties ~acc tbl = 
    tbl |> Hashtbl.iter (fun k v -> 
        match v with
        | `Inserted v -> Hashtbl.replace acc k (`Inserted v)
        | `Deleted -> Hashtbl.replace acc k `Deleted
        | `Lower_some _ -> ()
        | `Lower_none -> ())


  let dirties c : ('k * 'v dirty_entry) list = 
    let acc = Hashtbl.create (Hashtbl.length c.m1) in
    tbl_to_dirties ~acc c.m2;
    tbl_to_dirties ~acc c.m1;
    acc |> Hashtbl.to_list 

  let _ = dirties

end
open Cache_state


(** API with explicit cache state *)
module Api_explicit = struct
  (** API; 'c is the cache type *)
  type ('k,'v,'c,'t) ops = {
    find_opt   : m3:('k, 'v, 't) Map_lower.map_lower -> c:'c -> 'k -> ('v option, 't) m;
    insert     : c:'c -> 'k -> 'v -> (unit, 't) m;
    delete     : c:'c -> 'k -> (unit, 't) m;
    needs_trim : c:'c -> (bool, 't) m;
    trim       : c:'c -> (('k * 'v entry) list, 't) m;
    bindings   : c:'c -> (('k * 'v entry) list, 't) m;
    dirties    : c:'c -> (('k * 'v dirty_entry) list, 't) m;
    clean      : c:'c -> ( ('k*'v dirty_entry) list,'t)m;
    debug      : c:'c -> ( ('k,'v entry)Hashtbl.t * ('k,'v entry)Hashtbl.t )
  }    
end
open Api_explicit


(** API we implement, without explicit state; find_opt, insert, delete, trim etc *)
module Map_m = struct
  type ('k,'v,'t) map_m = {
    find_opt   : 'k -> ('v option,'t) m;  (* deleted maps to none *)
    insert     : 'k -> 'v -> (unit,'t) m;
    delete     : 'k -> (unit,'t)m;
    needs_trim : unit -> (bool,'t)m;
    trim       : unit -> ( ('k* ('v entry))list,'t)m; 
    bindings   : unit -> ( ('k* ('v entry))list,'t)m;
    dirties    : unit -> (('k * 'v dirty_entry) list, 't) m;
    clean      : unit -> ( ('k * 'v dirty_entry)list,'t)m;
    debug      : unit -> ( ('k,'v entry)Hashtbl.t * ('k,'v entry)Hashtbl.t )
  }
end


(** For some applications, we want very precise control of the cache,
   ie not to have the cache state hidden inside functions, but instead
   be referenced explicitly *)
module With_explicit_cache = struct

  module type S = sig
    type k
    type v
    type t
    val monad_ops: t monad_ops
  end


  module Make(S:S) : sig 
    open S
    type c 
    val c_to_cache_state: c -> (k,v)cache_state (* reveal impl *)
    val create_initial_cache : max_sz:int -> c
    val ops: (k,v,c,t) ops 
  end = struct    
    open S

    type c = (k,v)cache_state
    let create_initial_cache = create_initial_cache
    let c_to_cache_state c = c

    let ( >>= ) = monad_ops.bind
    let return = monad_ops.return

    (* FIXME should we delete entries from m2 when promoting to m1? or
       just mask?  *)

    let find_opt ~(m3:_ Map_lower.map_lower) ~(c:(k,v)cache_state) k =
      Hashtbl.find_opt c.m1 k |> function
      | None -> begin
          Hashtbl.find_opt c.m2 k |> function
          | None -> (
              (* pull from below, and promote to m1 *)
              m3.find_opt k >>= function
              | None -> 
                Hashtbl.replace c.m1 k `Lower_none;
                return None
              | Some v -> 
                Hashtbl.replace c.m1 k (`Lower_some v);
                return (Some v))
          | Some `Deleted -> (
              (* deleted in m2; promote to m1 and return *)
              Hashtbl.replace c.m1 k `Deleted;
              return None)
          | Some (`Lower_some v) -> (
              (* should we delete from m2 here? or just note that
                 the entry is masked by m1 *)
              Hashtbl.replace c.m1 k (`Lower_some v);
              return (Some v))
          | Some `Lower_none -> (
              Hashtbl.replace c.m1 k `Lower_none;
              return None)
          | Some (`Inserted v) -> (
              Hashtbl.replace c.m1 k (`Inserted v);
              return (Some v))
        end
      | Some `Deleted -> return None
      | Some (`Lower_some v) -> return (Some v)
      | Some `Lower_none -> return None
      | Some (`Inserted v) -> return (Some v)

    let _ = find_opt

    (** insert without worrying about size *)
    let insert ~c k v = 
      Hashtbl.replace c.m1 k (`Inserted v) |> return

    let delete ~c k = 
      Hashtbl.replace c.m1 k `Deleted |> return      

    (** Note this includes deleted entries in the size FIXME should it? *)
    let needs_trim ~c = 
      Hashtbl.length c.m1 |> fun s ->
      return (s > c.max_sz)

    let trim ~c : ((k * v entry) list,t)m = begin
      (* NOTE clearing and reusing seems faster than creating a new
         hashtable *)
      let tbl1 = c.m1 in
      let tbl2 = c.m2 in
      (* remove bindings in m2 which are shadowed by m1 *)
      tbl2 |> Hashtbl.filter_map_inplace (fun k v -> 
          Hashtbl.mem tbl1 k |> function
          | true -> None
          | false -> Some v);
      (* get the remaining bindings in m2 *)
      let xs = Hashtbl.to_seq tbl2 |> List.of_seq in
      (* clear m2 *)
      Hashtbl.clear tbl2;
      (* swap references *)
      c.m2 <- tbl1;
      c.m1 <- tbl2;          
      (* then return the bindings from earlier, which should now be
         flushed to m3 by the calling code *)
      return xs
    end

    let bindings ~c = 
      let tbl = Hashtbl.merge ~m1:c.m1 ~m2:c.m2 in
      tbl |> Hashtbl.to_list |> return

    let _ = bindings
      
    let dirties ~c = dirties c |> return

    let clean ~c = clean c |> return

    let debug ~c = (c.m1,c.m2)

    let ops = { find_opt;insert;delete;needs_trim;trim;bindings;dirties;clean;debug }

  end
end

(** Version without explicit cache state *)
let make (type t k v)
    ~(monad_ops: t monad_ops)
    ~(m3:(k,v,t)Map_lower.map_lower)
    ~max_sz
  =
  let module S = struct
    type nonrec k = k
    type nonrec v = v
    type nonrec t = t
    let monad_ops = monad_ops
  end
  in
  let open With_explicit_cache.Make(S) in
  let c = create_initial_cache ~max_sz in
  let find_opt k = ops.find_opt ~m3 ~c k in
  let insert k v = ops.insert ~c k v in
  let delete k = ops.delete ~c k in
  let needs_trim () = ops.needs_trim ~c in
  let trim () = ops.trim ~c in
  let bindings () = ops.bindings ~c in
  let dirties () = ops.dirties ~c in
  let clean () = ops.clean ~c in
  let debug () = ops.debug ~c in
  Map_m.{ find_opt;insert;delete;needs_trim;trim;bindings;dirties;clean;debug }
    

let _ = make


(** {2 Common instances} *)

let make_imperative ~m3 ~max_sz = make ~monad_ops:Tjr_monad.imperative_monad_ops ~m3 ~max_sz
let make_lw ~m3 ~max_sz = make ~monad_ops:Tjr_monad.lwt_monad_ops ~m3 ~max_sz
      

(* --------------------------------------------------------------------- *)

module Test() = struct

  (* FIXME replace with make_std *)
  module S = struct
    type t
    type 'a u = unit -> 'a

    let run_u (f: 'a u) : 'a = f ()

    let bind_u : 'a u -> ('a -> 'b u) -> 'b u =
      fun a f -> a () |> f

    let return_u: 'a -> 'a u = fun a () -> a

    let monad_ops : t monad_ops = {bind=Obj.magic bind_u;return=Obj.magic return_u}

    (* let run (f: ('a,t)m) : 'a = run_u (Obj.magic f) *)

    let coerce_ut: 'a u -> ('a,t)m = fun x -> Obj.magic x
    let coerce_tu: ('a,t)m -> 'a u = fun x -> Obj.magic x

    let run (f: ('a,t)m) : 'a = coerce_tu f |> run_u

  end
  open S
  let return = monad_ops.return

  let m3 : _ Map_lower.map_lower = {
    find_opt=(fun k -> return (Some k));
  }

  (* FIXME put these somewhere else *)

  let pp_list pp_elt xs = "["^(String.concat "," (List.map pp_elt xs))^"]"

  let pp_int x = string_of_int x

  let pp_int_list = pp_list pp_int

  let pp_intxint (i,j) = Printf.sprintf "(%d,%d)" i j

  let pp_intxint_list = pp_list pp_intxint 

  module With_delete = struct

    let pp_intxint' (i,j) = Printf.sprintf "(%d,%s)" i 
        begin match j with 
         | `Deleted -> "Deleted" 
         | `Lower_some v -> "Lower_some "^(string_of_int v) 
         | `Lower_none -> "Lower_none"
         | `Inserted j -> "Inserted "^(string_of_int j) 
        end

    let pp_intxint'_list = pp_list pp_intxint' 

    let debug tbl = tbl |> Hashtbl.to_seq |> List.of_seq |> pp_intxint'_list

    let made = make ~monad_ops ~m3 ~max_sz:3

    let Map_m.{ find_opt; insert; delete; needs_trim; trim; bindings; dirties; clean; debug=_ } = made

    let debug () = 
      let m1,m2 = made.debug () in
      Printf.printf "m1: %s\n" (debug m1);
      Printf.printf "m2: %s\n" (debug m2);
      ()

    let dest_Some = function
      | None -> failwith "dest_Some"
      | Some x -> x

    let _ = 
      Printf.printf "%s: test with delete\n" __FILE__;
      debug ();
      Printf.printf "find_opt 1: %d\n" (run (find_opt 1) |> dest_Some);
      debug ();
      Printf.printf "find_opt 7: %d\n" (run (find_opt 7) |> dest_Some);
      debug ();
      Printf.printf "find_opt 8: %d\n" (run (find_opt 8) |> dest_Some);
      debug ();
      Printf.printf "find_opt 3: %d\n" (run (find_opt 3) |> dest_Some);
      debug ();
      Printf.printf "trim: %s\n" (run (trim ()) |> pp_intxint'_list);
      debug ();

      Printf.printf "find_opt 2: %d\n" (run (find_opt 2) |> dest_Some);
      debug ();
      Printf.printf "find_opt 3: %d\n" (run (find_opt 3) |> dest_Some);
      debug ();
      Printf.printf "find_opt 8: %d\n" (run (find_opt 8) |> dest_Some);
      debug ();
      Printf.printf "find_opt 9: %d\n" (run (find_opt 9) |> dest_Some);
      debug ();
      Printf.printf "delete 9: %s\n" (run (delete 9); "()");
      debug ();
      Printf.printf "insert 1,99: %s\n" (run (insert 1 99); "()");
      debug ();
      Printf.printf "trim: %s\n" (run (trim ()) |> pp_intxint'_list);
      debug ();      
      ()

      (* https://gist.github.com/tomjridge/c1393f2a05a1725e22fee2b5beadb9f6 *)
      
  end


end
