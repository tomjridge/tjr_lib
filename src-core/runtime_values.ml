(** Manage values that are provided only at runtime.

Safe to open
*)


module Priv : sig
  type 'a runtime_value
  val rv_declare : unit -> 'a runtime_value
  val rv_get : 'a runtime_value -> 'a
  val rv_set : ?reset:bool -> 'a runtime_value -> 'a -> unit
end = struct
  
  type 'a runtime_value = { mutable rv: 'a option }

  let rv_declare () = { rv=None }

  let rv_get { rv } = 
    match rv with
    | None -> failwith "Uninitialized runtime value"
    | Some x -> x
      
  let rv_set ?reset:(reset=false) rv v =
    match rv.rv with
    | None -> rv.rv <- Some v
    | Some _ -> 
      match reset with
      | false -> failwith "Runtime value already set"
      | true -> rv.rv <- Some v

  let _ = rv_set    
end

include Priv

type 'a rv = 'a runtime_value
