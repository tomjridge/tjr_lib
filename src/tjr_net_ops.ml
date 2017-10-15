(* subset (the ones I needed at a certain point in time) of Unix, in monad *)

open Tjr_monad.Private

let mk_unix_net_ops ~monad_ops = 
  let (return,bind,err) = Tjr_monad.(monad_ops.return,monad_ops.bind,monad_ops.err) in
  let ( >>= ) = bind in

  let wrap = fun x ->
    try
      return () >>= x 
    with Unix.Unix_error(e,s1,s2) -> err e
  in
  let _ = wrap in

  let open Unix in
  (* socket call doesn't throw error; use lwt for guidance as to which
     calls need to be in monad *)
  let socket ~dom ~sock_ty ~prot_ty = 
    wrap @@ fun () -> return @@ socket dom sock_ty prot_ty in 
  let setsockopt ~fd ~opt ~b = 
    wrap @@ fun () -> return @@ setsockopt fd opt b in
  let bind ~fd ~addr = wrap @@ fun () -> return @@ bind fd addr in
  let listen ~fd ~max_pending = wrap @@ fun () -> return @@ listen fd max_pending in
  let accept ~fd = wrap @@ fun () -> return @@ accept fd in
  let getpeername ~fd = wrap @@ fun () -> return @@ getpeername fd in
  let close ~fd = wrap @@ fun () -> return @@ close fd in
  let connect ~fd ~addr = wrap @@ fun () -> return @@ connect fd addr in
  let write ~fd ~buf ~off ~len = 
    wrap @@ fun () -> return @@ write fd buf off len in
  let read ~fd ~buf ~off ~len = wrap @@ fun () -> return @@ read fd buf off len in
  `Net_ops(socket,setsockopt,bind,listen,accept,getpeername,close,connect,write,read)

(* ensure types match up *)
let wf_net_ops ops = 
  let _ = fun () -> mk_unix_net_ops ~monad_ops:(failwith "") = ops in
  true

let dest_net_ops ops = 
  assert(wf_net_ops ops);
  ops |> function
    `Net_ops(socket,setsockopt,bind,listen,accept,getpeername,close,connect,write,read) ->
    fun k ->
      k ~socket ~setsockopt ~bind ~listen ~accept ~getpeername ~close ~connect
        ~write ~read
  
let _ = dest_net_ops
(*
[ `Net_ops of
    (dom:Unix.socket_domain ->
     sock_ty:Unix.socket_type ->
     prot_ty:int -> (Unix.file_descr, 'a) Tjr_monad.Private.m) *
    (fd:Unix.file_descr ->
     opt:Unix.socket_bool_option -> b:bool -> (unit, 'b) Tjr_monad.Private.m) *
    (fd:Unix.file_descr ->
     addr:Unix.sockaddr -> (unit, 'c) Tjr_monad.Private.m) *
    (fd:Unix.file_descr -> max_pending:int -> (unit, 'd) Tjr_monad.Private.m) *
    (fd:Unix.file_descr ->
     (Unix.file_descr * Unix.sockaddr, 'e) Tjr_monad.Private.m) *
    (fd:Unix.file_descr -> (Unix.sockaddr, 'f) Tjr_monad.Private.m) *
    (fd:Unix.file_descr -> (unit, 'g) Tjr_monad.Private.m) *
    (fd:Unix.file_descr ->
     addr:Unix.sockaddr -> (unit, 'h) Tjr_monad.Private.m) ] ->
(socket:(dom:Unix.socket_domain ->
         sock_ty:Unix.socket_type ->
         prot_ty:int -> (Unix.file_descr, 'a) Tjr_monad.Private.m) ->
 setsockopt:(fd:Unix.file_descr ->
             opt:Unix.socket_bool_option ->
             b:bool -> (unit, 'b) Tjr_monad.Private.m) ->
 bind:(fd:Unix.file_descr ->
       addr:Unix.sockaddr -> (unit, 'c) Tjr_monad.Private.m) ->
 listen:(fd:Unix.file_descr ->
         max_pending:int -> (unit, 'd) Tjr_monad.Private.m) ->
 accept:(fd:Unix.file_descr ->
         (Unix.file_descr * Unix.sockaddr, 'e) Tjr_monad.Private.m) ->
 getpeername:(fd:Unix.file_descr -> (Unix.sockaddr, 'f) Tjr_monad.Private.m) ->
 close:(fd:Unix.file_descr -> (unit, 'g) Tjr_monad.Private.m) ->
 connect:(fd:Unix.file_descr ->
          addr:Unix.sockaddr -> (unit, 'h) Tjr_monad.Private.m) ->
 'i) ->
'i*)

let _ = wf_net_ops
