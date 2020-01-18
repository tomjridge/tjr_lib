(** Faking linear values *)

(** If we just want to read the value, this is presumably fine and
    doesn't mark the container as unusable. Only when we modify
    should we create a new ref and mark the old as used *)


module Internal : sig 
  type 'a lin

  (** Lift a function to the linear value; marks the original lin value as unusable *)
  val lin_lift: ('a -> 'b) -> 'a lin -> 'b lin

  (** Get the value for reading only *)
  val lin_get: 'a lin -> 'a

  val mk_lin: 'a -> 'a lin
end = struct
  type 'a lin = 'a option ref
      
  let mk_lin x = ref (Some x)

  let lin_lift f (x:'a lin) = 
    match !x with 
    | None -> failwith "lin_lift: attempt to use stale linear value"
    | Some x -> 
      f x |> fun x ->
      ref (Some x)

  let lin_get (x:'a lin) = 
    match !x with 
    | None -> failwith "lin_get: attempt to use stale linear value"
    | Some x -> x 
end
