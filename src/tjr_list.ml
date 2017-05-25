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
