(* the elusive either datatype ---------------------------------------- *)

type ('a,'b) either = Inl of 'a | Inr of 'b

let is_Inl = function
  | Inl _ -> true
  | _ -> false 

let is_Inr x = not (is_Inl x)

let dest_Inl = function
  | Inl x -> x
  | _ -> failwith "dest_Inl"


let dest_Inr = function
  | Inr x -> x
  | _ -> failwith "dest_Inr"


let cases f g = function
  | Inl x -> f x
  | Inr y -> g y
 
