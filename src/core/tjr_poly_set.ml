(* FIXME add more of these operations as needed *)
type ('a,'t) set_ops = {
  empty: 't;
  mem:'a -> 't -> bool;
  add: 'a -> 't -> 't;
  remove: 'a -> 't -> 't;
  cardinal: 't -> int;
  elements: 't -> 'a list;
  min_elt_opt: 't -> 'a option;
  max_elt_opt: 't -> 'a option;
  split: 'a -> 't -> ('t * bool * 't);
}

(** NOTE the dummy argument, which can be none, identifies the target
   carrier type; this adds a small amount of extra type safety since
   we can keep carriers separate *)
let make_set_ops (type elt t) (dummy:t option) (compare: elt -> elt -> int) = 
  let module S = Set.Make(struct type t = elt let compare = compare end) in
  let to_t (x:S.t) : t = Obj.magic x in
  let from_t (x:t) : S.t = Obj.magic x in
  let empty = S.empty |> to_t in
  let mem x s = S.mem x (from_t s) in
  let add x s = S.add x (from_t s) |> to_t in
  let remove x s = S.remove x (from_t s) |> to_t in
  let cardinal s = S.cardinal (from_t s) in
  let elements s = S.elements (from_t s) in
  let min_elt_opt s = S.min_elt_opt (from_t s) in
  let max_elt_opt s = S.max_elt_opt (from_t s) in
  let split x s = S.split x (from_t s) |> fun (s1,b,s2) -> (to_t s1,b,to_t s2) in
  {empty; mem; add; remove; cardinal; elements; min_elt_opt; max_elt_opt; split}

let _ = make_set_ops
