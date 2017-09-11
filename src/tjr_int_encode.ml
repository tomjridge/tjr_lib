
(* Matthew Szudzik elegant pair *)
let elegant_pair x y = 
  if x < y  (* ie x<>Max(x,y) *)
  then y*y+x
  else x*x+x+y

let isqrt x = 
  float_of_int x |> sqrt |> int_of_float  (* truncates, ie towards 0 *)

let elegant_unpair z = 
  let sqrt_z = isqrt z in
  let z' = sqrt_z * sqrt_z in
  if z - z' < sqrt_z 
  then (z-z',sqrt_z)
  else (sqrt_z,z-z'-sqrt_z)

let rec encode_list = function
  | [] -> 0
  | x::xs -> 1+ elegant_pair x (encode_list xs)

let rec decode_list = function
  | 0 -> []
  | x -> (x-1) |> elegant_unpair |> fun (x,y) -> x::decode_list y

let _tmp = [1;2;3;4] |> encode_list
let _tmp = [1;2;3;4] |> encode_list |> decode_list
(* this produces quite large numbers *)

