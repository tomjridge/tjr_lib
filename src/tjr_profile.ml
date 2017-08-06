(* simple profiling ---------------------------------------- *)

(* open Tjr_profile, then use P.(add profiler ab) *)
type profiler = {
  mark: int -> unit;
  mark': int -> bool; (* to make it easier to include in asserts *)
  get: unit -> (int*int) list
}


module P = struct
  let dest_Some = function Some x -> x | _ -> (failwith "dest_Some")

  (* waypoints, typically per-function *)
  let ab = 1
  let ac = 13
  let bc = 2
  let cd = 3
  let de = 4
  let ef = 5
  let fg = 6
  let gh = 7
  let hi = 8
  let ij = 9
  let jk = 10

  let p_to_string i = (
    match i with
    | _ when i = ab -> "ab"
    | _ when i = ac -> "ac"
    | _ when i = bc -> "bc"
    | _ when i = cd -> "cd"
    | _ when i = de -> "de"
    | _ when i = ef -> "ef"
    | _ when i = fg -> "fg"
    | _ when i = gh -> "gh"
    | _ when i = hi -> "hi"
    | _ when i = ij -> "ij"
    | _ when i = jk -> "jk"
    | _ -> "FIXME"
  )              


end

open P

(* assume 64-bit ints, so last conversion is ok  *)
let now () = Core.Time_stamp_counter.(now () |> to_int63 |> Core.Int63.to_int |> dest_Some)

(* a list of points and their timestamps; typically per function *)
let mk_profiler () = 
  let xs = ref [] in
  (* record time at a particular point in the code; typically put this
     in an assert, and disable in production *)
  let mark waypoint = (xs := (waypoint,now())::!xs) in
  let mark' x = mark x; true in
  let get () = !xs in
  { mark; mark'; get} 

let print_profile ~xs = (
  let f last prev = (
    let (p2,t2) = last in
    let (p1,t1) = prev in
    let d = t2 - t1 in
    let s = Printf.sprintf "(%s,%s) %d" (p1|>p_to_string) (p2|>p_to_string) d in
    let _ = print_endline s in
    prev)
  in
  let _ = List.fold_left f (List.hd xs) xs in
  ())
