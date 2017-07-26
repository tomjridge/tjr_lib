include List

let concat_with : ('a -> 'b -> 'a) -> 'b list -> 'a = (
  fun f xs -> 
    match xs with
      x::xs -> List.fold_left f x xs)

let last xs = List.hd (List.rev xs)

let butlast xs = xs|>List.rev|>List.tl|>List.rev

let inc : ('a -> bool) -> 'a list -> 'a list = (fun p xs -> List.filter p xs)

let exc : ('a -> bool) -> 'a list -> 'a list = (
  fun p xs -> List.filter (fun x -> not (p x)) xs)

let assoc_inv : 'a -> ('b * 'a) list -> 'b =
  fun x xs -> xs |> List.map (function (x,y) -> (y,x)) |>
  List.assoc x


let rec from_to l h = if l>h then [] else l :: from_to (l+1) h


let rec take n xs = if n = 0 then [] else List.hd xs :: take (n-1) (List.tl xs)

let rec drop n xs = if n = 0 then xs else drop (n-1) (List.tl xs)

let take_while = Core_kernel.Std.List.take_while
let drop_while = Core_kernel.Std.List.drop_while
let split_while = Core_kernel.Std.List.split_while


(* passes in the index of the elt to the map function; useful for numbering *)
let mapi = Core_kernel.Std.List.mapi

let index xs = xs |> mapi ~f:(fun i t -> (t,i))

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


let split_at i xs = (take i xs, drop i xs)

(* convert assoc list to map; assumes assoc list is sorted *)
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
                   
                     
(*

let _ = from_to 1 10 |> List.map (fun x -> (x,2*x)) |> assoc_list_to_bst
        |> fun f -> f 2

 *)


let fold_left_ ~step ~init_state xs = 
  List.fold_left 
    (fun a b -> step ~state:a b)
    init_state
    xs

let with_each_elt = fold_left_



module List_as_set = struct

let subset xs ys = List.for_all (fun x -> List.mem x ys) xs

let equal xs ys = subset xs ys && subset ys xs

end
