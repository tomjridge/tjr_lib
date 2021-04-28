(** A two-generation LRU with batch eviction. Compared to v3, this is the pure part only (no monad or handling of slow operations).

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

(* --------------------------------------------------------------------- *)

(** {2 Version with delete} *)


(** Entries in the internal maps can record that an entry has been
   deleted, or that an entry is present in the lower map m3; 

{[ type 'v entry = [ `Inserted of 'v | `Deleted | `Lower_some of 'v | `Lower_none ] ]}

*)
type 'v entry = [ `Inserted of 'v | `Deleted | `Lower_some of 'v | `Lower_none ]

type 'v dirty_entry = [ `Inserted of 'v | `Deleted ]



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


(** API; 'c is the cache type *)
type ('k,'v,'c) ops = {
  (* find_opt   : c:'c -> 'k -> 'v option; (\* needed? *\) *)
  find_raw   : c:'c -> 'k -> 'v entry option;
  insert     : c:'c -> 'k -> 'v -> unit;
  insert_raw : c:'c -> 'k -> 'v entry -> unit;
  delete     : c:'c -> 'k -> unit;
  needs_trim : c:'c -> bool;
  trim       : c:'c -> ('k * 'v entry) list;
  bindings   : c:'c -> ('k * 'v entry) list;
  dirties    : c:'c -> ('k * 'v dirty_entry) list;
  clean      : c:'c -> ('k*'v dirty_entry) list;
  debug      : c:'c -> ( ('k,'v entry)Hashtbl.t * ('k,'v entry)Hashtbl.t )
}    


module type S = sig
  type k
  type v
end

module type T = sig 
  module S : S
  open S
  type c 
  val c_to_cache_state: c -> (k,v)cache_state (* reveal impl *)
  val create_initial_cache : max_sz:int -> c
  val ops: (k,v,c) ops 
end

(** For some applications, we want very precise control of the cache,
   ie not to have the cache state hidden inside functions, but instead
   be referenced explicitly *)
module Make(S:S) : T with module S = S = struct
  module S = S
  open S

  type c = (k,v)cache_state
  let create_initial_cache = create_initial_cache
  let c_to_cache_state c = c


  (* FIXME should we delete entries from m2 when promoting to m1? or
     just mask?  At the moment, we just mask *)
  let find_raw ~(c:(k,v)cache_state) k =
    Hashtbl.find_opt c.m1 k |> fun x -> 
    match x with
    | None -> begin
        Hashtbl.find_opt c.m2 k |> function
        | None -> None  (* expect to pull from below, and promote to m1 *)
        | Some entry -> (
            (* deleted in m2; promote to m1 and return *)
            Hashtbl.replace c.m1 k entry;
            Some entry)
      end
    | Some entry -> Some entry                      

  (** insert without worrying about size *)
  let insert ~c k v = 
    Hashtbl.replace c.m1 k (`Inserted v)

  let delete ~c k = 
    Hashtbl.replace c.m1 k `Deleted 

  let insert_raw ~c k v = 
    Hashtbl.replace c.m1 k v

  (** Note this includes deleted entries in the size FIXME should it? *)
  let needs_trim ~c = 
    Hashtbl.length c.m1 |> fun s ->
    (s > c.max_sz)

  let trim ~c : (k * v entry) list = begin
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
    xs
  end

  let bindings ~c = 
    let tbl = Hashtbl.merge ~m1:c.m1 ~m2:c.m2 in
    tbl |> Hashtbl.to_list

  let _ = bindings

  let dirties ~c = dirties c

  let clean ~c = clean c 

  let debug ~c = (c.m1,c.m2)

  let ops = { find_raw;insert;insert_raw;delete;needs_trim;trim;bindings;dirties;clean;debug }

end



