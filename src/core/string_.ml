(* strings ---------------------------------------- *)

include String

(* to make self_contained *)
let list_last xs = 
  assert (xs<>[]);
  List.hd (List.rev xs)

module Span = struct

  type t = {s:string;i:int;j:int} (* s,i,j - the part of the string i<=..<j *)

  let of_string s = {s;i=0;j=String.length s}

  let to_string = function {s;i;j} -> String.sub s i (j-i)

  let length = function {s;i;j} -> j-i

  let pred_to_indexes : (t->int list) -> t -> int list = (
    fun p s -> (
        let l = length s in
        let r = ref [] in
        let _ = 
          for i = 0 to l -1 do
            {s=s.s;i=s.i+i;j=l}|>p|>(fun xs -> r:=xs @ !r)
          done
        in
        List.rev !r
      ))

  let equal t1 t2 = (to_string t1 = to_string t2) (* FIXME inefficient *)

  (* s - small; b - big *)
  let starts ~prefix b = (
    let s = prefix in
    let l = length s in
    if l <= length b then
      (if equal s { b with j=(b.i+l) } then true else false)
    else false
  )


end

let indexes ~(sub:string) b = Span.(
    let s = sub|>of_string in
    let b = b|>of_string in
    pred_to_indexes (fun x -> if starts ~prefix:s x then [x.i] else []) b
  )


(* let starts ~prefix b = indexes prefix b |>List.mem 0 *)

(* more efficient version *)
let starts_with ~prefix b =
  let len = String.length prefix in
  len > String.length b |> function 
  | true -> false
  | false -> 
    let rec f j = 
      if j >= len then true else
      prefix.[j] = b.[j] &&
      f (j+1)
    in
    f 0
    

let ends ~suffix b = 
  indexes ~sub:suffix b |> 
  List.mem (String.length b - String.length suffix)

let ends_with = ends

let contains ~sub b = indexes ~sub b <> []


let split_at s n = (String.sub s 0 n, String.sub s n (String.length s - n))


(* this replaces the first occurrence; error if no occurrence *)
let replace_first ~sub ~rep b =
  indexes ~sub b |> 
  (fun xs -> split_at b (List.hd xs)) |> 
  (fun (b1,b2) -> (
       let (_,b3) = split_at b2 (String.length sub) in
       b1^rep^b3))


(* this replaces the last occurrence; error if no occurrence *)
let replace_last ~sub ~rep b =
  indexes ~sub b |> 
  (fun xs -> split_at b (list_last xs)) |> 
  (fun (b1,b2) -> (
       let (_,b3) = split_at b2 (String.length sub) in
       b1^rep^b3))


(* FIXME may want to have a non-recursive replace, so it works if s2 contains s1 *)
let replace_all ~sub ~rep b = (
  let b = ref b in
  while (contains ~sub !b) do
    b:=replace_first ~sub ~rep !b
  done;
  !b)


let drop n s = split_at s n|>snd

(* more efficient version? *)
let drop n s =
  String.length s |> fun l ->
  if n >= l then "" else
    String.sub s n (l-n)


let split_on_first ~sub b = (
  indexes ~sub b |> 
  (fun is_ -> 
     let (b1,b2) = split_at b (List.hd is_) in
     (b1,drop (String.length sub) b2)))


let split_on_last ~sub b = (
  indexes ~sub b |> 
  (fun is_ -> 
     let (b1,b2) = split_at b (list_last is_) in
     (b1,drop (String.length sub) b2)))


(* if b is abcxxxdefxxxghi then split_on_sb xxx b is [abc,def,ghi];
   if starts with xxx, then result list starts with "" *)
let split_on_all ~sub b = (
  let xs = ref [] in
  let b = ref b in
  while (contains ~sub !b) do
    let (b1,b2) = split_on_first ~sub !b in
    xs:=b1::!xs;
    b:=b2      
  done;
  xs:=!b::!xs;
  List.rev !(xs))


let concat_strings ~sep xs = (
  match xs with 
  | [] -> "" 
  | [x] -> x
  | x::xs -> List.fold_left (fun a b -> a^sep^b) x xs)


(* FIXME not on jsoo

(* check whole string matches re; re uses Str syntax *)
let matches ~re s = Str.(
    string_match (regexp("^"^re^"$")) s 0
  )
*)

let replace_list ~subs s = (
  let s = ref s in
  let _ = List.iter (fun (sub,rep) -> s:=replace_all ~sub ~rep !s) subs in
  !s
)


(* explode; convert to list of char *)
let explode s = 
  let s = ref s in
  let r = ref [] in
  let _ = 
    while(!s <> "") do
      r:=(String.get !s 0)::!r;
      s:=String.sub !s 1 (String.length !s - 1)
    done
  in
  List.rev !r

(* implode; convert list of char to string *)
let implode s = 
  let s = Bytes.init (List.length s) (List.nth s) in
  Bytes.unsafe_to_string s
