(** An LRU with a slow lower layer, and batch eviction.    

Don't open - record field clashes, type clashes

 *)
open Tjr_monad


(** {2 Interfaces} *)

type ('k,'v) op = [
  | `Insert of 'k*'v
  | `Delete of 'k
]    

type 'v op' = [
  | `Insert of 'v
  | `Delete
]

(** Lowest level (bottom) operations, typically those supported by a
   persistent on-disk log. *)
module Bot = struct 
  type ('k,'v,'bot,'t) ops = {
    find_opt   : 'bot -> 'k -> ('v option, 't) m;
    insert     : 'bot -> 'k -> 'v -> (unit, 't) m;
    delete     : 'bot -> 'k -> (unit, 't) m;
    sync       : 'bot -> (unit,'t) m;
    exec       : 'bot -> sync:bool -> ('k,'v)op list -> (unit,'t)m;
  }
end

(** Top-level operations. *)
module Top = struct

  type ('k,'v,'c,'t) ops = {
    find_opt   : 'c -> 'k -> ('v option, 't) m;
    insert     : 'c -> 'k -> 'v -> (unit, 't) m;
    delete     : 'c -> 'k -> (unit, 't) m;
    sync       : 'c -> (unit,'t) m;
    (* sync_k    'c -> 'k -> (unit,'t)m; *)
    (* sync_ks   'c -> 'k list -> (unit,'t)m; *)
    exec       : 'c -> sync:bool -> ('k,'v)op list -> (unit,'t)m;
  }
  (* FIXME we may very well want to be able to sync a subset of keys *)
end


(** {2 Implementation} *)

type 'v entry' = 'v option * bool ref

type ('v,'t) entry = 
  | Plain of 'v entry' (** the ref is "dirty" flag *)
  | Finding of ('v option,'t)m

(* let is_dirty (e:_ entry') = !(snd e) *)

let lower_none () = (None, ref false)
let lower_some v = (Some v, ref false)
let insert v = (Some v, ref true)
let delete () = (None, ref true)


type ('k,'v,'lru,'t) cache_state = {
  mutable is_syncing: bool;

  mutable syncing: (unit,'t)m;
  (** If we are syncing, then we service operations we can
     immediately, and other operations wait for the sync. This field
     is only meaningful if is_syncing is true. *)

  lru: 'lru; 
  (** Lru state is mutable *)

  trim_delta: int; 
  (** Number of entries we trim each time the cache gets too big;
     trim_delta should be much less than cap *)

  lru_capacity: int; (** LRU capacity *)
}


module type S = sig
  type t = lwt

  type k
  type v
  type lru
  val lru_ops : (k,(v,t)entry,lru) Tjr_lru.Mutable.lru_ops


  type bot
  val bot : bot
  val bot_ops : (k,v,bot,t)Bot.ops
end

module Make(S:S) = struct
  open S
  module Lru = (val lru_ops)

  open Tjr_monad.With_lwt

  let initial_cache_state ~cap ~trim_delta = 
    assert(trim_delta > 0);
    assert(cap > 0);
    assert(trim_delta <= cap);
    {
      is_syncing   = false;
      syncing      = return ();
      lru          =  Lru.create cap;
      trim_delta;
      lru_capacity = cap
    }

  (* FIXME *)
  let maybe_trim () = return ()

  let rec find_opt c k = 
    (* try to find in cache *)
    Lru.find k c.lru |> function
    | None -> begin
        (* not in cache, so check whether we are syncing *)
        match c.is_syncing with
        | true -> 
          c.syncing >>= fun () -> 
          find_opt c k (* try again after sync completes *)
        | false -> 
          (* create the promise *)
          let p = 
            bot_ops.find_opt bot k >>= fun v -> 
            begin match v with 
            | None -> 
              Lru.add k (Plain (lower_none ())) c.lru
            | Some v -> 
              Lru.add k (Plain (lower_some v)) c.lru
            end;
            return v
          in
          Lru.add k (Finding p) c.lru;
          maybe_trim () >>= fun () -> 
          p                   
      end
    | Some (Plain x) -> 
      let (v,_) = x in
      return v
    | Some (Finding p) -> 
      p      
      
  let insert c k v = 
    Lru.add k (Plain (insert v)) c.lru;
    maybe_trim ()

  let delete c k = 
    Lru.add k (Plain (delete ())) c.lru;
    maybe_trim ()

  let rec sync c = 
    match c.is_syncing with 
    | true -> 
      c.syncing >>= fun () -> 
      sync c (* try again, since we may have updated entries whilst syncing *)
    | false -> 
      (* construct promise *)
      let p = 
        let dirties = ref [] in
        c.lru |> Lru.iter (fun k v -> 
            match v with 
            | Finding _ -> ()
            | Plain (v,dirty) -> 
              match !dirty with 
              | true -> (
                dirty:=false;
                let op = match v with
                  | None -> `Delete k
                  | Some v -> `Insert(k,v)
                in
                dirties:=op::!dirties)
              | false -> ());
        (* now process dirties as a batch *)
        bot_ops.exec bot ~sync:true (!dirties)        
      in
      c.is_syncing <- true;
      c.syncing <- p;
      p >>= fun () ->
      c.is_syncing <- false;
      return () 
      
  let exec c ~sync ops = return () (* FIXME *)
      (* perform updates on lru; then call sync; then maybe_trim *)

  let ops = Top.{ find_opt; insert; delete; sync; exec }
  
end
