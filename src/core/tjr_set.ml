(* basic support for sets *)


type ('e,'t) set_ops = {
  empty: unit -> 't;
  is_empty: 't -> bool;
  mem: 'e -> 't -> bool;
  add: 'e -> 't -> 't;
  remove: 'e -> 't -> 't;
  of_list: 'e list -> 't;
  union: 't -> 't -> 't;
  diff: 't -> 't -> 't;
  cardinal: 't -> int;
  choose: 't -> 'e;
  iter: ('e -> unit) -> 't -> unit
}


(* reuse OCaml's sets *)
module Make = functor (Ord : Set.OrderedType) -> struct
  
  module Set_ = Set.Make(Ord)

  let set_ops = Set_.{
    empty=(fun () -> empty); is_empty; mem; add; remove; of_list; union; diff; cardinal;choose;iter
  } 

end
