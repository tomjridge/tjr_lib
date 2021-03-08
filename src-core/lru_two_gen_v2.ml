(* this is all too much hassle; conclusion: we should use imperative programming, and at verification time we do the mapping to monads etc

(** A two-generation LRU with batch eviction. Compared to v1, this includes extra operations insert, delete, trim...

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

Don't open - map type clashes

 *)
open Monad_intf

(** The internal imperative map type, on which the impl is built;
   based on Hashtbl interfaces *)
type ('tbl,'k,'v,'t) map_i = {
  find_opt           : 'tbl -> 'k -> ('v option,'t) m;
  insert             : 'tbl -> 'k -> 'v -> (unit,'t) m;
  size               : 'tbl -> (int,'t) m;
  create             : unit -> ('tbl,'t)m;
  mem                : 'tbl -> 'k -> (bool,'t)m;
  filter_map_inplace : 'a 'b. ('a -> 'b -> ('b option,'t)m) -> 'tbl -> (unit,'t)m;
  bindings           : 'tbl -> (('k*'v)list,'t)m
}

(** The unknown implementation for the lower map *)
type ('k,'v,'t) map_l = {
  find_opt : 'k -> ('v option,'t) m;
  insert   : 'k -> 'v -> (unit,'t) m;
}

(** What we implement, a v small map; needs_trim is true if the size
   of m1 is > N; to delete, we typically insert with value None *)
type ('k,'v,'t) map_s = {
  find_opt   : 'k -> ('v option,'t) m;
  insert     : 'k -> 'v -> (unit,'t) m;
  needs_trim : unit -> (unit,'t)m;
  trim       : unit -> (('k*'v)list,'t)m; 
  bindings   : unit -> (('k*'v)list,'t)m;
}


(** 
       - m1: the top cache
       - m2: the lower cache
       - m3: the underlying map
*)
let make (type t) 
    ~(monad_ops: t monad_ops) 
    ~(ref_cell_ops: _ ref_cell_ops)
    ~(map_i: _ map_i)
    ~(m3: _ map_l)
  =
  let open struct
    let ( >>= ) = monad_ops.bind
    let return = monad_ops.return

    let { mkref; deref; assign } = ref_cell_ops

    let create ~max_sz = 
      assert(max_sz >= 1);
      map_i.create () >>= fun _m1 -> 
      mkref _m1 >>= fun r1 -> 
      map_i.create () >>= fun _m2 -> 
      mkref _m2 >>= fun r2 -> 

      let open struct

        let find_opt k =
          deref r1 >>= fun tbl -> 
          map_i.find_opt tbl k >>= function
          | None -> (
              map_i.find_opt tbl k >>= function
              | None -> (
                  (* pull from below, and promote to m1 *)
                  m3.find_opt k >>= function
                  | None -> return None
                  | Some v -> 
                    map_i.insert tbl k v >>= fun () ->
                    return (Some v))
              | Some v -> (
                  (* should we delete from m2 here? or just note that
                     the entry is masked by m1 *)
                  map_i.insert tbl k v >>= fun () ->
                  return (Some v)))
          | Some v -> return (Some v)

        
        (** insert without worrying about size *)
        let insert k v = 
          deref r1 >>= fun tbl -> 
          map_i.insert tbl k v

        let needs_trim () = 
          deref r1 >>= fun tbl -> 
          map_i.size tbl >>= fun s -> 
          return (s > max_sz)

        let trim () = 
          (* NOTE clearing and reusing seems faster than creating a new
             hashtable *)
          deref r1 >>= fun tbl1 ->
          deref r2 >>= fun tbl2 -> 
          (* remove bindings in m2 which are shadowed by m1 *)
          tbl2 |> map_i.filter_map_inplace (fun k v -> 
              map_i.mem tbl1 k >>= function
              | true -> return None
              | false -> return (Some v)) >>= fun () -> 
          (* then return the remainder *)
          map_i.bindings tbl2
          
          
          
          

      end
      in

end

let create (type t) ~monad_ops = 
  let module A = Make(struct type nonrec t = t let monad_ops = monad_ops end) in
  A.create


(* Implementation with hashtables and two references *)
module Imperative = struct

  type ('k,'v) map_l = {
    find_opt : 'k -> 'v option;
    insert   : 'k -> 'v -> unit;
    size     : unit -> int;
    clear    : unit -> unit;
    tbl      : ('k,'v)Hashtbl.t ref
  }

  let create (type t) ~(monad_ops:t monad_ops) = 
    let open struct
      let return = monad_ops.return 
      let ( >>= ) = monad_ops.bind 

      let coerce_map_l (m : _ map_l) : _ map = {
        find_opt=(fun k -> m.find_opt k |> return);
        insert=(fun k v -> m.insert k v |> return);
        size=(fun () -> m.size () |> return)
      }

      let make_map_l tbl =
        let find_opt k = 
          Hashtbl.find_opt !tbl k in
        let insert k v = Hashtbl.add !tbl k v in
        let size () = Hashtbl.length !tbl in
        let clear () = Hashtbl.clear !tbl in
        { find_opt; insert; size; clear;tbl }

      let make ~max_sz ~flush_m2 = 
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
          (* before flushing m2, we remove all keys in m1 (still live,
             may have been updated) *)
          let m = !m1_ref in
          !m2_ref |> Hashtbl.filter_map_inplace (fun k v -> 
              match Hashtbl.mem m k with
              | true -> None
              | false -> Some v);
          (* then flush the remainder *)
          Printf.printf "flushing m2\n";
          flush_m2 (Hashtbl.to_seq !m2_ref |> List.of_seq) >>= fun () -> 
          m2.clear ();
          let m = !m2_ref in
          m2_ref:= !m1_ref;
          m1_ref:=m;
          return ()
        in
        let x = create ~monad_ops ~max_sz ~demote_m1 
            ~m1:(coerce_map_l m1) ~m2:(coerce_map_l m2) ~m3:(coerce_map_l m3) 
        in
        object
          method find_opt = x.find_opt
          method m1=m1
          method m2=m2
          method m3=m3
        end

      let _ = make

    end
    in
    make
end

let create_imperative ~monad_ops ~max_sz ~flush_m2 = 
  Imperative.create ~monad_ops ~max_sz ~flush_m2

module Test() = struct

  let _ = Printf.printf "%s: test\n" __FILE__

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

  let make ~max_sz = create_imperative ~monad_ops ~max_sz 
      ~flush_m2:(fun _ -> Printf.printf "Flushing m2\n%!"; return ())

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
*)
