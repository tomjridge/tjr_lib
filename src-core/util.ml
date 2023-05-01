(** Essentially the Y combinator; useful for anonymous recursive
    functions. The k argument is the recursive callExample:

    {[
      iter_k (fun ~k n -> 
          if n = 0 then 1 else n * k (n-1))

    ]}


*)
let iter_k f (x:'a) =
  let rec k x = f ~k x in
  k x  

let dest_Some = function (Some x) -> x | _ -> failwith "dest_Some"

let dest_Ok = function (Ok x) -> x | _ -> failwith "dest_Ok"



(** {2 Misc} *)

module Map_int = Map.Make(struct type t = int let compare: t -> t -> int = Stdlib.compare end)

module Map_string = Map.Make(struct type t = string let compare: t -> t -> int = Stdlib.compare end)


module Set_int = Set.Make(
struct 
  type t = int 
  let compare : t -> t -> int = Stdlib.compare 
end)

module Set_string = Set.Make(
struct 
  type t = string
  let compare : t -> t -> int = Stdlib.compare 
end)
