(** A two-generation LRU with batch eviction.

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

 *)

type ('a,'t) m

type 't monad_ops = {
  bind : 'a 'b. ('a,'t) m -> ('a -> ('b,'t) m) -> ('b,'t) m;
  return: 'a. 'a -> ('a,'t)m
}

module Pvt(S:sig type t val monad_ops: t monad_ops end) = struct
  module S = S
  open S

  let ( >>= ) = monad_ops.bind
  let return = monad_ops.return

  type ('k,'v) map = {
    find_opt : 'k -> ('v option,t) m;
    insert   : 'k -> 'v -> (unit,t) m;
    size     : unit -> (int,t) m;
  }

  type ('k,'v) map_s = {
    find_opt : 'k -> ('v option,t) m;
  }


  let create ~max_sz ~(demote_m1: unit -> (unit,t) m) ~(m1: _ map) ~(m2: _ map) ~(m3: _ map) = 
    assert(max_sz >= 1);
    let open (struct

      let insert k v = 
        m1.size () >>= fun s1 -> 
        match s1 < max_sz with
        | true -> (
            (* can insert directly into m1 *)
            m1.insert k v)
        | false -> (
            demote_m1 () >>= fun () -> 
            m1.insert k v)

      let find_opt k =
        m1.find_opt k >>= function
        | None -> (
            m2.find_opt k >>= function
            | None -> (
                (* pull from below, and promote to m1 *)
                m3.find_opt k >>= function
                | None -> return None
                | Some v -> 
                  insert k v >>= fun () ->
                  return (Some v))
            | Some v -> (
                (* should we delete from m2 here? or just note that
                   the entry is masked by m1 *)
                m1.insert k v >>= fun () ->
                return (Some v)))
        | Some v -> return (Some v)

      (* what about size, for this datastruct? needed? *)
                      
      let size = m1.size
  
    end)
    in
    { find_opt }

end

module Test() = struct

  let _ = Printf.printf "%s: test\n" __FILE__

  (* example with hashtables and two references *)

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

  module P = Pvt(S)
  open P

  type ('k,'v) map_l = {
    find_opt : 'k -> 'v option;
    insert   : 'k -> 'v -> unit;
    size     : unit -> int;
    clear    : unit -> unit;
    tbl      : ('k,'v)Hashtbl.t ref
  }

  let coerce_map_l (m : _ map_l) : _ map = {
    find_opt=(fun k -> coerce_ut (fun () ->
        m.find_opt k));
    insert=(fun k v -> coerce_ut (fun () ->
        m.insert k v));
    size=(fun () -> coerce_ut (fun () ->
        m.size ()))
  }

  let make_map_l tbl =
    let find_opt k = 
      Hashtbl.find_opt !tbl k in
    let insert k v = Hashtbl.add !tbl k v in
    let size () = Hashtbl.length !tbl in
    let clear () = Hashtbl.clear !tbl in
    { find_opt; insert; size; clear;tbl }
    
  let make ~max_sz = 
    let m1_ref = ref (Hashtbl.create 10) in
    let m1 = make_map_l m1_ref in
    let m2_ref = ref (Hashtbl.create 10) in
    let m2 = make_map_l m2_ref in
    let m3_ref = ref (Hashtbl.create 10) in
    let m3 = make_map_l m3_ref in
    let demote_m1 () = 
      Printf.printf "demoting m1\n";
      (* NOTE clearing and reusing seems faster than creating a new
         hashtable *)
      m2.clear ();
      let m = !m2_ref in
      m2_ref:= !m1_ref;
      m1_ref:=m;
      return ()
    in
    let x = create ~max_sz ~demote_m1 ~m1:(coerce_map_l m1) ~m2:(coerce_map_l m2) ~m3:(coerce_map_l m3) in
    object
      method find_opt = x.find_opt
      method m1=m1
      method m2=m2
      method m3=m3
    end

  let _ = make

  (* FIXME put these somewhere else *)
    
  let pp_list pp_elt xs = "["^(String.concat "," (List.map pp_elt xs))^"]"

  let pp_int x = string_of_int x

  let pp_int_list = pp_list pp_int

  let pp_intxint (i,j) = Printf.sprintf "(%d,%d)" i j

  let pp_intxint_list = pp_list pp_intxint 

  (* specialize to int, int *)

  let debug tbl = tbl |> Hashtbl.to_seq |> List.of_seq |> pp_intxint_list

  

  let made = make ~max_sz:3

  let find_opt = made#find_opt

  let m1,m2,m3 = made#m1,made#m2,made#m3

  let _ = made

  let debug () = 
    Printf.printf "m1: %s\n" (debug !(made#m1.tbl));
    Printf.printf "m2: %s\n" (debug !(made#m2.tbl));
    Printf.printf "m3: %s\n" (debug !(made#m3.tbl));
    ()

  let dest_Some = function
    | None -> failwith "dest_Some"
    | Some x -> x

  let _ = 
    debug ();
    ([1;2;3;4;5;6;7;8;9;10] |> List.iter (fun x -> Hashtbl.add (!(m3.tbl)) x x));
    debug ();
    Printf.printf "find_opt 1: %d\n" (run (find_opt 1) |> dest_Some);
    debug ();
    Printf.printf "find_opt 7: %d\n" (run (find_opt 7) |> dest_Some);
    debug ();
    Printf.printf "find_opt 8: %d\n" (run (find_opt 8) |> dest_Some);
    debug ();
    Printf.printf "find_opt 3: %d\n" (run (find_opt 3) |> dest_Some);
    debug ();
    ()
    (* https://gist.github.com/tomjridge/464ea21205e44dc920fcba52376eaaa2 *)

end
