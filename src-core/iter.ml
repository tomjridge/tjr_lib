(** favourite iteraction operator *)

let iter_opt (f:'a -> 'a option) = 
  let rec loop x = 
    match f x with
    | None -> x
    | Some x -> loop x
  in
  fun x -> loop x

type ('a,'b) break_or_cont = Break of 'a | Cont of 'b

(** Iterate until reach break *)
let rec iter_break f (x:'a) = 
  f x |> function
  | Break b -> b
  | Cont a -> iter_break f a

(** Essentially the Y combinator; useful for anonymous recursive functions *)
let iter_k f (x:'a) =
  let rec k x = f ~k x in
  k x

let _ 
: (k:('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
= iter_k
  
