(* stepping state with error ---------------------------------------- *)

open Tjr_either

type ('a,'s,'e) t = 's -> ('s * ('a,'e) either option)

let step1: 's -> ('a,'s,'e) t -> ('s * ('a,'e) either option) = 
  fun s t -> t s

(* stop when we get to error or 'a *)
let rec run: 's -> ('a,'s,'e) t -> ('s * ('a,'e) either) = (
  fun s t ->
    match (step1 s t) with
    | (s',None) -> run s' t
    | (s',Some(Inl x)) -> (s',Inl x)
    | (s',Some(Inr e)) -> (s',Inr e))
                              
