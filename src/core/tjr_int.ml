module Make_type_isomorphic_to_int() : sig
  type t
  val int2t: int -> t
  val t2int: t -> int
end = struct
  type t = int
  let int2t i = i
  let t2int i = i
end
