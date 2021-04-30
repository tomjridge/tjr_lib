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
    initial_cache_state : cap:int -> trim_delta:int -> 'c;
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

module type T = sig
  type t = lwt
  type k
  type v
  type lru
  type cache_state'
  val ops: (k,v,cache_state',t)Top.ops
  val to_cache_state: cache_state' -> (k,(v,t)entry,lru,t)cache_state
  val of_cache_state: (k,(v,t)entry,lru,t)cache_state -> cache_state'
end

(** Functor make *)
module Make(S:S) : T with type k = S.k and type v = S.v and type lru = S.lru = struct
  include S
  module Lru = (val lru_ops)
  type cache_state' = (k,(v,t)entry,lru,t)cache_state
  let to_cache_state = fun x -> x
  let of_cache_state = fun x -> x

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

  (** This is called when the capacity may potentially be exceeded; in
     this case, we may need to trim some entries by flushing them to
     the bottom layer. *)
  let maybe_trim c = 
    let cap = c.lru_capacity in
    let sz = Lru.size c.lru in
    let no_trim = c.is_syncing || sz <= cap in
    match no_trim with 
    | true -> return () (* sync automatically trims *)
    | false -> 
      (* we want to issue a call to the lower layer, and prevent any
         concurrent operation overtaking this one - effectively we
         want to serialize calls to the lower layer; the point is that
         just by calling the lower layer, we don't guarantee that the
         promise resolves immediately - it may be delayed and some
         other operation occurs in the meantime, which may use a newer
         kv, leading to the newer kv being overwritten by the older;
      *)
      (* NOTE: we can drop "finding" entries if we choose *)
      let to_trim = ref [] in
      for _i = 1 to c.trim_delta do
        let x = Lru.lru c.lru in
        Lru.drop_lru c.lru;
        assert(x <> None); (* we must have elts to drop *)
        let op = Option.get x in
        let op = 
          match op with 
          | (k,Plain (Some v,dirty)) -> (
              match !dirty with
              | true -> `Insert(k,v)
              | false -> `None)
          | (k,Plain (None,dirty)) -> (
              match !dirty with
              | true -> `Delete k
              | false -> `None)
          | (k,Finding _) -> `None 
          (* we may drop a finding; but all extant computations
             waiting on the resolved value will still wait of course
             - we are not deleting the promise! *)
        in
        (match op with 
         | `None -> ()
         | `Insert (k,v) -> to_trim := `Insert(k,v)::!to_trim
         | `Delete k -> to_trim := `Delete(k)::!to_trim)
      done;
      (* we expect that sync should be true here - no point delaying
         writing to disk *)
      let p = bot_ops.exec bot ~sync:true !to_trim in
      c.is_syncing <- true;
      p >>= fun () -> 
      c.is_syncing <- false;
      return ()

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
        (* take the opportunity to trim some entries *)
        Lru.trim c.lru;
        (* now process dirties as a batch *)
        bot_ops.exec bot ~sync:true (!dirties)        
      in
      c.is_syncing <- true;
      c.syncing <- p;
      p >>= fun () ->
      c.is_syncing <- false;
      return () 

  (* perform updates on lru; then maybe call sync, or else maybe_trim *)
  let exec c ~sync:sync' ops = 
    (* FIXME what is c.is_syncing? it's fine - we may call sync which
       will wait till current sync completes *)
    begin
      ops |> List.iter (function
          | `Insert (k,v) -> Lru.add k (Plain (insert v)) c.lru
          | `Delete k -> Lru.add k (Plain (delete ())) c.lru)
    end;
    match sync' with
    | false -> maybe_trim c
    | true -> sync c

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
            maybe_trim c >>= fun () -> 
            return v
          in
          Lru.add k (Finding p) c.lru;          
          p                   
      end
    | Some (Plain (v,_)) -> return v
    | Some (Finding p) -> p
      
  let insert c k v = 
    Lru.add k (Plain (insert v)) c.lru;
    maybe_trim c

  let delete c k = 
    Lru.add k (Plain (delete ())) c.lru;
    maybe_trim c

  let ops = Top.{ initial_cache_state; find_opt; insert; delete; sync; exec }
  
end

(** make as a function.

    NOTE: This uses Stdlib pervasive equality and hashing on values of
   type k; this provides a simpler interface where the Lru
   functionality is constructed for the user *)
let make (type k v bot) ~bot_ops ~bot = 
  let open (struct
    type t = lwt

    (* construct lru ops using Tjr_lru.Mutable *)
    module S = struct
      type nonrec k = k
      let equal (k1:k) (k2:k) = (k1=k2) (* FIXME? *)
      let hash k = Hashtbl.hash k
      type nonrec v = (v,t)entry
    end
    module Lru = Tjr_lru.Mutable.Make(S)
    type lru = Lru.t
    let lru_ops = Lru.lru_ops

    (* now call Make *)
    module S2 = struct
      type nonrec t = lwt
      type nonrec k = k
      type nonrec v = v
      type nonrec lru = lru
      let lru_ops = lru_ops

      type nonrec bot = bot
      let bot_ops = bot_ops
      let bot = bot
    end        

    module M = Make(S2)
  end)
  in
  (module M : T with type k = k and type v = v)

let _ : 
bot_ops:('k, 'v, 'bot, lwt) Bot.ops ->
bot:'bot -> 
(module T with type k = 'k and type v = 'v)
 = make


module Test() = struct
  (* FIXME best way to test? *)
end
