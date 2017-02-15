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


