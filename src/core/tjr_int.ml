module type TYPE_ISOMORPHIC_TO_INT = sig 
  type t
  val int2t: int -> t
  val t2int: t -> int
end

module Make_type_isomorphic_to_int() : TYPE_ISOMORPHIC_TO_INT = struct
  type t = int
  let int2t i = i
  let t2int i = i
end

let compare: int -> int -> int = Pervasives.compare
