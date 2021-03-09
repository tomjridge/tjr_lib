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

Don't open - map type clashes

 *)
open Monad_intf


module Map_lower = struct
  (** The unknown implementation for the lower map *)
  type ('k,'v,'t) map_lower = {
    find_opt : 'k -> ('v option,'t) m;
    (* insert   : 'k -> 'v -> (unit,'t) m; *)
  }
end
(* open Map_lower  *)


(* --------------------------------------------------------------------- *)

(** {2 Version without delete} *)

module Map_s = struct
  (** What we implement, a v small map; needs_trim is true if the size
      of m1 is > N; to delete, we typically insert with value None *)
  type ('k,'v,'t) map_s = {
    find_opt   : 'k -> ('v option,'t) m;
    insert     : 'k -> 'v -> (unit,'t) m;
    needs_trim : unit -> (bool,'t)m;
    trim       : unit -> (('k*'v)list,'t)m; 
    bindings   : unit -> (('k*'v)list,'t)m;
    debug      : unit -> ( ('k,'v)Hashtbl.t * ('k,'v)Hashtbl.t )
  }
end
(* include Map_s *)

(** 
       - m3: the underlying map
*)
let make_without_delete (type t) 
    ~(monad_ops: t monad_ops) 
    ~(m3: _ Map_lower.map_lower)
    ~max_sz
  =
  let open struct
    let ( >>= ) = monad_ops.bind
    let return = monad_ops.return

    let _ = assert(max_sz >= 1)
    let tbl1 = Hashtbl.create max_sz 
    let tbl2 = Hashtbl.create max_sz 
    let m1 = ref tbl1 
    let m2 = ref tbl2

    let find_opt k =
      Hashtbl.find_opt (!m1) k |> function
      | None -> (
          Hashtbl.find_opt (!m2) k |> function
          | None -> (
              (* pull from below, and promote to m1 *)
              m3.find_opt k >>= function
              | None -> return None
              | Some v -> 
                Hashtbl.replace (!m1) k v |> fun () ->
                return (Some v))
          | Some v -> (
              (* should we delete from m2 here? or just note that
                 the entry is masked by m1 *)
              Hashtbl.replace (!m1) k v |> fun () ->
              return (Some v)))
      | Some v -> return (Some v)

    (** insert without worrying about size *)
    let insert k v = 
      Hashtbl.replace (!m1) k v |> return

    let needs_trim () = 
      Hashtbl.length (!m1) |> fun s ->
      return (s > max_sz)

    let trim () = begin
      (* NOTE clearing and reusing seems faster than creating a new
         hashtable *)
      let tbl1 = !m1 in
      let tbl2 = !m2 in
      (* remove bindings in m2 which are shadowed by m1 *)
      tbl2 |> Hashtbl.filter_map_inplace (fun k v -> 
          Hashtbl.mem tbl1 k |> function
          | true -> None
          | false -> Some v);
      (* get the remaining bindings in m2 which we return later as the
         "flushed" *)
      let xs = Hashtbl.to_seq tbl2 |> List.of_seq in
      (* clear m2 *)
      Hashtbl.clear tbl2;
      (* swap references *)
      m2 := tbl1;
      m1 := tbl2;          
      (* then return the bindings from earlier, which are now flushed *)
      return xs
    end


    let bindings () = 
      (* copy m2 *)
      Hashtbl.copy !m2 |> fun tbl -> 
      (* update with newer entries from m1 *)
      !m1 |> Hashtbl.iter (fun k v -> Hashtbl.replace tbl k v);
      (* return as list *)
      tbl |> Hashtbl.to_seq |> List.of_seq |> return

    let debug () = (!m1, !m2)

  end
  in
  Map_s.{ find_opt; insert; needs_trim; trim; bindings; debug }


let _ = make_without_delete


(* --------------------------------------------------------------------- *)

(** {2 Version with delete} *)

(** Entries in the internal maps can record that an entry has been
   deleted, or that an entry is present in the lower map m3 *)
module Entry = struct
  type 'v entry = [ `Some of 'v | `Deleted | `Lower of 'v ]
end
open Entry

(** What we implement *)
module Map_m = struct
  type ('k,'v,'t) map_m = {
    find_opt   : 'k -> ('v option,'t) m;  (* deleted maps to none *)
    insert     : 'k -> 'v -> (unit,'t) m;
    delete     : 'k -> (unit,'t)m;
    needs_trim : unit -> (bool,'t)m;
    trim       : unit -> ( ('k* ('v entry))list,'t)m; 
    bindings   : unit -> ( ('k* ('v entry))list,'t)m;
    debug      : unit -> ( ('k,'v entry)Hashtbl.t * ('k,'v entry)Hashtbl.t )
  }
end

let make_with_delete (type t k v)
    ~(monad_ops: t monad_ops)
    ~(m3:(k,v,t)Map_lower.map_lower)
    ~max_sz
  =
  let open struct
    let ( >>= ) = monad_ops.bind
    let return = monad_ops.return

    let _ = assert(max_sz >= 1)
    let tbl1 : (k,v entry)Hashtbl.t = Hashtbl.create max_sz 
    let tbl2 : (k,v entry)Hashtbl.t = Hashtbl.create max_sz 
    let m1 = ref tbl1 
    let m2 = ref tbl2

    let find_opt k =
      Hashtbl.find_opt (!m1) k |> function
      | None -> begin
          Hashtbl.find_opt (!m2) k |> function
          | None -> (
              (* pull from below, and promote to m1 *)
              m3.find_opt k >>= function
              | None -> return None
              | Some v -> 
                Hashtbl.replace (!m1) k (`Lower v) |> fun () ->
                return (Some v))
          | Some `Deleted -> 
            (* deleted in m2; promote to m1 and return *)
            Hashtbl.replace (!m1) k `Deleted |> fun () -> 
            return None
          | Some (`Lower v) -> 
            (* should we delete from m2 here? or just note that
               the entry is masked by m1 *)
            Hashtbl.replace (!m1) k (`Lower v) |> fun () ->
            return (Some v)
          | Some (`Some v) -> 
            (* should we delete from m2 here? or just note that
               the entry is masked by m1 *)
            Hashtbl.replace (!m1) k (`Some v) |> fun () ->
            return (Some v)
        end
      | Some `Deleted -> return None
      | Some (`Lower v) -> return (Some v)
      | Some (`Some v) -> return (Some v)

    let _ = find_opt

    (** insert without worrying about size *)
    let insert k v = 
      Hashtbl.replace (!m1) k (`Some v) |> return

    let delete k = 
      Hashtbl.replace (!m1) k `Deleted |> return      

    (** Note this includes deleted entries in the size FIXME should it? *)
    let needs_trim () = 
      Hashtbl.length (!m1) |> fun s ->
      return (s > max_sz)

    let trim () : ((k * v entry) list,t)m = begin
      (* NOTE clearing and reusing seems faster than creating a new
         hashtable *)
      let tbl1 = !m1 in
      let tbl2 = !m2 in
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
      m2 := tbl1;
      m1 := tbl2;          
      (* then return the bindings from earlier, which should now be
         flushed to m3 by the calling code *)
      return xs
    end

    let bindings () = 
      (* copy m2 *)
      Hashtbl.copy !m2 |> fun tbl -> 
      (* update with newer entries from m1 *)
      !m1 |> Hashtbl.iter (fun k v -> Hashtbl.replace tbl k v);
      (* return as list *)
      tbl |> Hashtbl.to_seq |> List.of_seq |> return

    let debug () = (!m1, !m2)

    let _ = bindings
  end
  in
  Map_m.{ find_opt; insert; delete; needs_trim; trim; bindings; debug }
    

let _ = make_with_delete

(* --------------------------------------------------------------------- *)

module Test() = struct

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

  module Without_delete = struct
    let make ~max_sz = make_without_delete ~monad_ops ~max_sz ~m3

    let _ = make

    (* specialize to int, int *)

    let debug tbl = tbl |> Hashtbl.to_seq |> List.of_seq |> pp_intxint_list

    let made = make ~max_sz:3

    let Map_s.{ find_opt; insert; needs_trim; trim; bindings; debug=_ } = made

    let debug () = 
      let m1,m2 = made.debug () in
      Printf.printf "m1: %s\n" (debug m1);
      Printf.printf "m2: %s\n" (debug m2);
      ()

    let dest_Some = function
      | None -> failwith "dest_Some"
      | Some x -> x

    let _ = 
      Printf.printf "%s: test without delete\n" __FILE__;
      debug ();
      Printf.printf "find_opt 1: %d\n" (run (find_opt 1) |> dest_Some);
      debug ();
      Printf.printf "find_opt 7: %d\n" (run (find_opt 7) |> dest_Some);
      debug ();
      Printf.printf "find_opt 8: %d\n" (run (find_opt 8) |> dest_Some);
      debug ();
      Printf.printf "find_opt 3: %d\n" (run (find_opt 3) |> dest_Some);
      debug ();
      Printf.printf "trim: %s\n" (run (trim ()) |> pp_intxint_list);
      debug ();

      Printf.printf "find_opt 2: %d\n" (run (find_opt 2) |> dest_Some);
      debug ();
      Printf.printf "find_opt 3: %d\n" (run (find_opt 3) |> dest_Some);
      debug ();
      Printf.printf "find_opt 8: %d\n" (run (find_opt 8) |> dest_Some);
      debug ();
      Printf.printf "find_opt 9: %d\n" (run (find_opt 9) |> dest_Some);
      debug ();
      Printf.printf "insert 1,99: %s" (run (insert 1 99); "()");
      debug ();
      Printf.printf "trim: %s\n" (run (trim ()) |> pp_intxint_list);
      debug ();
      ()
      (* https://gist.github.com/tomjridge/c1393f2a05a1725e22fee2b5beadb9f6 *)
  end



  module With_delete = struct

    let pp_intxint' (i,j) = Printf.sprintf "(%d,%s)" i (match j with `Deleted -> "Deleted" | `Lower v -> ("Lower "^(string_of_int v)) | `Some j -> (string_of_int j))

    let pp_intxint'_list = pp_list pp_intxint' 

    let debug tbl = tbl |> Hashtbl.to_seq |> List.of_seq |> pp_intxint'_list

    let made = make_with_delete ~monad_ops ~m3 ~max_sz:3

    let Map_m.{ find_opt; insert; delete; needs_trim; trim; bindings; debug=_ } = made

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
