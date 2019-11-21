let dest_Some = function (Some x) -> x | _ -> failwith "dest_Some"

let dest_Ok = function (Ok x) -> x | _ -> failwith "dest_Ok"



(** {2 Misc} *)

module Map_int = Map.Make(struct type t = int let compare: t -> t -> int = Pervasives.compare end)

module Map_string = Map.Make(struct type t = string let compare: t -> t -> int = Pervasives.compare end)


module Set_int = Set.Make(
struct 
  type t = int 
  let compare : t -> t -> int = Pervasives.compare 
end)

module Set_string = Set.Make(
struct 
  type t = string
  let compare : t -> t -> int = Pervasives.compare 
end)
