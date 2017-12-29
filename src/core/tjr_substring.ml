(* This file provides common support for substring/span parsing *)

(* simple span-based substrings ----------------------------------- *)

(* we don't use this type directly *)
type 'a poly_substring = {
  s_:'a;
  i_:int;
  j_:int
}

let length x = x.j_ - x.i_

(* example terminal parsers ----------------------------------------- *)

type substring_ = string poly_substring

let mk_substring s = {s_=s;i_=0;j_=String.length s}

module String_position = struct
type string_position = {
  s_:string;
  i_:int
}
end
open String_position

(* returns the end indexes *)
type terminal_parser = string_position -> int list

let eps s = [s.i_]


let re ~re s = 
  if (Str.string_match re s.s_ s.i_) then
    [Str.match_end ()]  (* FIXME return all results? *)
  else []

let upto_re ~re s =
  try 
    Str.search_forward re s.s_ s.i_ |> fun k ->
    [k]
  with Not_found -> []

let a lit s = re ~re:Str.(quote lit |> regexp) s

let upto_a lit = upto_re ~re:Str.(quote lit |> regexp)

(* sequencing *)
let ( **> ) p1 p2 = 
  fun s -> 
  p1 s 
  |> List.map (fun i_ -> p2 {s with i_})
  |> List.concat



(* example ---------------------------------------------------------- *)


module X = functor (Y:sig end) -> struct

let _ = upto_a ")" {s_="...)";i_=0}

let bracket = (a "(") **> (upto_a ")" **> (a ")"))

let _ = bracket

let _ = bracket {s_="(xxx)";i_=0 }
end
