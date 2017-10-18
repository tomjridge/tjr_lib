(* subset (the ones I needed at a certain point in time) of Unix, in monad *)

open Lwt
type 'a m = 'a t
let return = Lwt.return
let bind = Lwt.bind

type wrap_ops = {
  wrap: 'a.  (unit -> 'a m) -> 'a m
}

let wrap_ops = {
  wrap=(fun f -> f ())
}

let mk_lwt_net_ops () = 
  let wrap = wrap_ops.wrap in

  let open Lwt_unix in
  (* socket call doesn't throw error; use lwt for guidance as to which
     calls need to be in monad *)

  let socket dom sock_ty prot_ty = 
    wrap @@ fun () -> socket dom sock_ty prot_ty |> return in 

  let setsockopt fd opt b = wrap @@ fun () -> setsockopt fd opt b |> return in

  let bind fd addr = wrap @@ fun () -> bind fd addr in

  let listen fd max_pending = wrap @@ fun () -> listen fd max_pending |> return in

  let accept fd = wrap @@ fun () -> accept fd in

  let getpeername fd = wrap @@ fun () -> getpeername fd |> return in

  let close_EBADF fd = wrap @@ fun () -> close fd in

  let connect fd addr = wrap @@ fun () -> connect fd addr in

  let write fd buf off len = wrap @@ fun () -> write fd buf off len in

  let read fd buf off len = wrap @@ fun () -> read fd buf off len in

  `Net_ops(socket,setsockopt,bind,listen,accept,getpeername,close_EBADF,connect,write,read)

(* ensure types match up *)
let wf_net_ops ops = 
  (* this for typing *)
  let _ = fun () -> 
    mk_lwt_net_ops () = ops 
  in
  true

let dest_net_ops ops = 
  assert(wf_net_ops ops);
  ops |> function
    `Net_ops(socket,setsockopt,bind,listen,accept,getpeername,close_EBADF,connect,write,read) ->
    fun k ->
      k ~socket ~setsockopt ~bind ~listen ~accept ~getpeername ~close_EBADF ~connect
        ~write ~read

let _ = dest_net_ops

let _ = wf_net_ops



