(* generic monad *)

module type M_ = sig
  type ('a,'e) m

  val ( >>= ) : ('a,'e) m -> ('a -> ('b,'e) m) -> ('b,'e) m
  val return : 'a -> ('a,'e) m
  val err : 'e -> ('a,'e) m

end


module M = struct

  type (_,'e) m = 
      Return : 'a -> ('a,'e) m 
    | Bind:  ('a,'e) m * ('a -> ('b,'e) m) -> ('b,'e) m
    | Err: 'e -> ('a,'e) m

  let ( >>=) = fun m f -> Bind(m,f)

  let return x = Return x

  let err e = Err e

end


module X_ : M_ = M

include M


(* generic version parameterized over the basic primitive operations *)
module Alternative = struct

  type (_,'prim,'e) m = 
      Return : 'a -> ('a,'prim,'e) m 
    | Bind:  ('a,'prim,'e) m * ('a -> ('b,'prim,'e) m) -> ('b,'prim,'e) m
    | Err: 'e -> ('a,'prim,'e) m
    | Prim: 'prim -> ('a,'prim,'e) m

  let ( >>=) = fun m f -> Bind(m,f)

  let return x = Return x

  let err e = Err e


  let example = 
    let read () = Prim (`Read ()) in
    let write (s:string) = Prim(`Write s) in
    read () >>= fun s ->
    write s >>= fun () -> 
    return ()

  let _ = example

end
