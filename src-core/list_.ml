(** Extra list functions; see also {!Core_kernel.List} *)

include List


(** {2 last, butlast } *)

let last xs = List.hd (List.rev xs)

let rev_butlast xs = xs |> rev |> tl

let butlast xs = rev_butlast xs |> rev


(** {2 List creation: from_to, map_range etc} *)

(*
let from_to l h = 
  let rec f l sofar = 
    if l>=h then List.rev sofar else f (l+1) (l::sofar)
  in 
  f l [] 
*)

(** tail recursive [l..h-1] *)
let from_upto l h = 
  let rec f l sofar = 
    if l>=h then List.rev sofar else f (l+1) (l::sofar)
  in 
  f l [] 


(** f l .. f (h-1) *)
let rec map_range ~f l h = 
  if l >= h then [] else (f l)::(map_range ~f (l+1) h)

(** min, min+step, ... max-1  FIXME inefficient;  *)
let mk_range ~min ~max ~step = 
  let xs = ref [] in
  let n = ref min in
  while !n < max do
    xs:=!n::!xs;
    n:=!n+step
  done;
  List.rev !xs 


(** {2 Splitting} *)

(** cf split_n; prefer list arg last, so can revapply *)
let split_at i xs = Core_kernel.List.split_n xs i


(** {2 Take and drop} *)

(** NOTE not tail recursive *)
let rec take n xs = 
  if n = 0 then [] else List.hd xs :: take (n-1) (List.tl xs)

let rec drop n xs = 
  if n = 0 then xs else drop (n-1) (List.tl xs)


(** {2 Misc} *)

(** concat_with f [b1; b2; ...] is ...(f (f b1 b2) b3) *)
let concat_with : ('a -> 'b -> 'a) -> 'b list -> 'a = 
  fun f xs -> 
  match xs with
    x::xs -> List.fold_left f x xs



(* FIXME uncomment any of these that are still relevant 

(* folding ---------------------------------------------------------- *)


let fold_left_ ~step ~init_state xs = 
  List.fold_left 
    (fun a b -> step ~state:a b)
    init_state
    xs

let with_each_elt' = fold_left_

let with_each_elt
  ~list
  ~step
  ~init
  = fold_left_ ~step ~init_state:init list

(* folding with assoc and comm. operation --------------------------- *)

(* NOTE assumes xs not empty; add a neutral element to guarantee this *)
let fold_assoc_comm ~op xs =
  assert(xs<>[]);
  with_each_elt'
    ~step:(fun ~state n -> op state n)
    ~init_state:(List.hd xs)
    (List.tl xs)



(* take, drop ------------------------------------------------------- *)

(** NOTE not tail recursive *)
let rec take n xs = 
  if n = 0 then [] else List.hd xs :: take (n-1) (List.tl xs)

let rec drop n xs = 
  if n = 0 then xs else drop (n-1) (List.tl xs)

let split_while ~f xs = 
  let rec loop acc xs =
    match xs with 
    | [] -> (List.rev acc, [])
    | x::xs ->
      match f x with
      | true -> loop (x::acc) xs
      | false -> (List.rev acc, xs)
  in
  loop [] xs

let take_while ~f xs = split_while ~f xs |> fst
let take_while ~f xs = split_while ~f xs |> snd

(* filter ----------------------------------------------------------- *)

(* FIXME duplicate of filter_rev_map; this follows JS naming *)
let rev_filter_map f xs =
  ([],xs) |> iter_opt (function
      | _,[] -> None
      | xs',x::xs -> 
        f x |> function
        | None -> Some(xs',xs)
        | Some y -> Some(y::xs',xs))
  |> fun (xs',[]) -> xs'

let _ = rev_filter_map

let include_ : ('a -> bool) -> 'a list -> 'a list = 
  fun p xs -> List.filter p xs

let exclude : ('a -> bool) -> 'a list -> 'a list = 
  fun p xs -> List.filter (fun x -> not (p x)) xs

(** Map a function from 'a to 'b option, but remove None; NOTE returns
   a reversed version of the list *)
let filter_rev_map ~(f:'a -> 'b option) xs =
  with_each_elt
    ~list:xs
    ~step:(fun ~state x -> match f x with
      | None -> state
      | Some fx -> fx::state)
    ~init:[]


(* assoc ------------------------------------------------------------ *)

(** "inverse" assoc, ie assoc (swap_pairs xs) *)
let assoc_inv : 'a -> ('b * 'a) list -> 'b =
  fun x xs -> xs |> List.map (function (x,y) -> (y,x)) |>
  List.assoc x

(* convert assoc list to map using a binary search tree; assumes assoc
   list is sorted *)
let rec assoc_list_to_bst kvs = 
  match kvs with
  | [] -> fun k -> None
  | [(k,v)] -> fun k' -> if k=k' then Some v else None
  | _ -> 
     List.length kvs 
     |> fun n -> 
        kvs |> split_at (n/2) 
        |> fun (xs,(k,v)::ys) -> 
           let f1 = assoc_list_to_bst xs in
           let f2 = assoc_list_to_bst ((k,v)::ys) in
           fun k' -> if k' < k then f1 k' else f2 k'


(* assoc_list_remdups   --------------------------------------------- *)

let assoc_list_remdups kvs =
  let rec loop (keys_sofar,kvs_sofar, rest) =
    match rest with
    | [] -> kvs_sofar
    | (k,v)::rest -> 
      match List.mem k keys_sofar with
      | true -> loop (keys_sofar,kvs_sofar,rest)
      | false -> loop (k::keys_sofar,(k,v)::kvs_sofar,rest)
  in
  loop ([],[],kvs)



(* two lists -------------------------------------------------------- *)

(* let index xs = xs |> mapi ~f:(fun i t -> (t,i)) *)

(* interleave two lists, starting with xs *)
let interleave xs ys = (
  assert(length xs = length ys || length xs = length ys + 1);
  let rec f ys xs = (
    match (ys,xs) with
    | [],[] -> []
    | [y],[] -> [y]
    | [],[x] -> [x] (* FIXME *)
    | y::ys,x::xs -> y::x::(f ys xs))
  in
  (hd xs)::(f ys (tl xs))
)


(* lists as sets ---------------------------------------------------- *)
module List_as_set = struct

  let subset xs ys = List.for_all (fun x -> List.mem x ys) xs

  let equal xs ys = subset xs ys && subset ys xs

end

let unique xs = 
  xs 
  |> with_each_elt'
    ~step:(fun ~state x -> if List.mem x state then state else x::state)
    ~init_state:[]


(* prefix, suffix etc ----------------------------------------------- *)

let prefix ~prefix ys =
  let n = List.length prefix in
  List.length prefix <= List.length ys
  && (take n ys = prefix)

let suffix ~suffix xs =
  let n = List.length suffix in
  n <= List.length xs 
  && drop (List.length xs - n) xs = suffix


(* int list --------------------------------------------------------- *)

(* over |N not ZZ *)
let max_list xs = 
  fold_assoc_comm ~op:(max) (0::xs)


(* list scan -------------------------------------------------------- *)

(* iterate over a list until the first Some x; return this (or None if no such elt *)
let iter_till_some (f: 'a -> 'b option) xs =
  (None,xs) |> iter_opt (fun (ret,xs) ->
      match ret with 
      | Some x -> None
      | None -> (
          match xs with 
          | [] -> None
          | x::xs -> 
            f x |> function
            | None -> Some (None,xs)
            | Some ret -> Some(Some ret,[])))
  |> function (ret,_) -> ret

let _ : ('a -> 'b option) -> 'a list -> 'b option = iter_till_some



(* OLD -------------------------------------------------------------- *)
(*
(* begin section ---------------------------------------------------- *)
(* NOTE this section copied verbatim from JS core_kernel *)
(* passes in the index of the elt to the map function; useful for numbering *)
let rev_mapi l ~f =
  let rec loop i acc = function
    | [] -> acc
    | h :: t -> loop (i + 1) (f i h :: acc) t
  in
  loop 0 [] l

let mapi l ~f = List.rev (rev_mapi l ~f)
*)
*)
