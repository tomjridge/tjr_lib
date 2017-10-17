(* double negation monad; state passing *)

(* FIXME not at all sure about this 
type ('a,'m) m = ('a -> 'm) -> 'm

let return (x:'a) : ('a,'m)m = fun k -> k x

let bind x  g = 
  fun h ->  
    x @@ (fun a -> g a h)

let _ : ('a,'m)m -> ('a -> ('b,'n)m) -> ('b,'n)m = bind

let ( >>= ) = bind

type monad_ops = {
  return: 'a 'm. 'a -> ('a,'m)m;
  bind: 'a 'b 'm. ('a,'m)m -> ('a -> ('b,'m)m) -> ('b,'m)m
}

(* lwt example ------------------------------------------------------ *)

module U = Unix

type 'e extra_ops = {
  (* run whether error or result; any errors ignored *)
  finalize: 'a 'm 'n. (unit -> (unit,'n)m) -> ('a,'m)m -> ('a,'m)m;
  (* run on error *)
  catch: 'a 'm 'n. (unit -> (unit,'n)m) -> ('a,'m)m -> ('a,'m)m;
  err: 'a 'm. 'e -> ('a,'m)m
}

type ipp = U.sockaddr (*  expect ADDR_INET ip * port *)
type quad = { local:ipp; remote: ipp }

let ex (type fd) 
    ~monad_ops
    ~socket ~close ~setsockopt ~bind ~listen ~accept ~getpeername ~extra ~quad 
  = 
  let (return,bind') = (monad_ops.return,monad_ops.bind) in
  let ( >>= ) = bind' in
  socket U.PF_INET U.SOCK_STREAM 0 >>= fun (srvr:fd) ->
  extra.finalize (fun () -> close srvr) (
    (* hack to speed up recovery *)
    setsockopt srvr U.SO_REUSEADDR true >>= fun () ->
    let addr = quad.local in
    bind srvr addr >>= fun () ->
    listen srvr 5 >>= fun () ->
    accept srvr >>= fun (c,_) ->
    if getpeername c <> quad.remote then 
      (* connection doesn't match quad *)
      close c >>= fun () -> extra.err `Error_incorrect_peername
    else
      return c)

let _ = ex



(*

(* 'a m = ('a -> ww) -> ww, where ww = (lazy)list of (w -> w) *)

(* coinductive *)
type ('a,'w) trans = Finished of 'a | Step of ('w -> 'w * (unit -> ('a,'w) trans))

type ('a,'w) m' = ('a -> ('a,'w) trans) -> ('a,'w) trans

module X_ = struct

  type w

  type 'a m = ('a -> ('a,w) trans) -> ('a,w) trans

  let return (x:'a) : 'a m = fun (k:('a -> ('a,w) trans)) -> k x

  let bind (x:'a m) (g: 'a -> 'b m) : 'b m = 
    fun (h : 'b -> ('a,'w) trans) ->  
      x @@ (fun a -> g a h)

end

let return x : ('a,'w) m' = fun k -> k x

(* FIXME *)
let bind (x:('a,'w)m') (g:'a -> ('b,'w)m') : ('b,'w) m' = 
  fun (h : 'b -> ('b,'w) trans) ->  
    x @@ (fun a -> g a h)

let ( >>= ) = bind

(*
let with_state f : (unit,'w) m' = fun (k:unit -> 'w trans) -> 
  Step(fun w -> f w,k)
*)

let with_state (type a w b) (f:w -> a * w) (g: a -> (b,w)m') : (b,w)m' = 
  fun (h: b -> (b,w) trans) -> 
  Step(fun w -> 
      f w |> fun (a,w') ->
      w',fun () -> g a h)


let _ = with_state

(* example ---------------------------------------------------------- *)

module Example = functor (X_ : sig end) -> struct

  type w = int
  type 'a m = ('a -> ('a,w) trans) -> ('a,w) trans

  let inc : unit m = fun (k:unit -> (unit,w) trans) -> 
    let ww = fun w -> 
      Printf.printf "incrementing %d" w;
      w+1
    in
    Step (fun w -> ww w,k)


  (* FIXME note that for "with error" we stop when we hit an exceptional state *)
  let run ~(state:w) ~(code:'a m) =
    (code @@ fun a -> Finished a) |> fun trans ->
    let rec run ~(state:w) ~(trans:('a,w) trans) = 
      match trans with
      | Finished a -> (a,state)
      | Step(f) ->
        f state |> fun (state,rest) -> run ~state ~trans:(rest())
    in
    run ~state ~trans
  
  let _ : state:w -> code:'a m -> ('a * w) = run

  let code = inc >>= fun () -> inc >>= fun () -> inc

  let _ : unit m = code

  let _ = run ~code ~state:0

end



(* example with error ----------------------------------------------- *)


module Example2 = functor (X_ : sig end) -> struct

  module Example = Example(X_)
  open Example

  (* FIXME note that for "with error" we stop when we hit an exceptional state *)
  let run ~(is_exceptional:w -> bool) ~(state:w) ~(code:'a m) =
    (code @@ fun a -> Finished a) |> fun trans ->
    let rec run ~(state:w) ~(trans:('a,w) trans) = 
      match is_exceptional state with
      | true -> (None,state)
      | false -> 
        match trans with
        | Finished a -> (Some a,state)
        | Step(f) ->
          f state |> fun (state,rest) -> run ~state ~trans:(rest())
    in
    run ~state ~trans

  (* the exceptional state *)
  let is_exceptional = fun w -> w = 1

  (* following stops when w is 1 *)
  let _ = run ~code ~state:0

end
*)


*)
