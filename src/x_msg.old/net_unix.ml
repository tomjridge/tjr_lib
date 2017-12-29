(* subset (the ones I needed at a certain point in time) of Unix, in monad *)

(* abbreviation *)
module M_ = Tjr_monad.Step_monad


(* basic types and monad -------------------------------------------- *)

type unix_net_error = Unix_net_error of Unix.error

type state = {
  error: [ `Net_err of unix_net_error ] option
}

type 'a m = ('a,state) M_.m
let return = M_.return
let bind = M_.bind

let dest_exceptional s = s.error
let catch f x = M_.catch ~dest_exceptional f x


let wrap=(fun f -> M_.(Step(fun s -> 
  try s,(f () |> fun x -> fun () -> Finished x) 
  with Unix.Unix_error(e,s1,s2) -> 
    {error=Some(`Net_err (Unix_net_error e))},
    fun () -> failwith __LOC__)))


let mk_unix_net_ops () = 
  let open Unix in
  (* socket call doesn't throw error; use lwt for guidance as to which
     calls need to be in monad *)

  let socket dom sock_ty prot_ty = 
    wrap @@ fun () -> socket dom sock_ty prot_ty in 

  let setsockopt fd opt b = wrap @@ fun () -> setsockopt fd opt b in

  let bind fd addr = wrap @@ fun () -> bind fd addr in

  let listen fd max_pending = wrap @@ fun () -> listen fd max_pending in

  let accept fd = wrap @@ fun () -> accept fd in

  let getpeername fd = wrap @@ fun () -> getpeername fd in

  let close_EBADF fd = wrap @@ fun () -> close fd in

  let connect fd addr = wrap @@ fun () -> connect fd addr in

  let write fd buf off len = wrap @@ fun () -> write fd buf off len in

  let read fd buf off len = wrap @@ fun () -> read fd buf off len in

  `Net_ops(socket,setsockopt,bind,listen,accept,getpeername,close_EBADF,connect,write,read)

(* ensure types match up *)
let wf_unix_net_ops ops = 
  (* this for typing *)
  let _ = fun () -> 
    mk_unix_net_ops () = ops 
  in
  true

let dest_net_ops ops = 
  assert(wf_unix_net_ops ops);
  ops |> function
    `Net_ops(socket,setsockopt,bind,listen,accept,getpeername,close_EBADF,connect,write,read) ->
    fun k ->
      k ~socket ~setsockopt ~bind ~listen ~accept ~getpeername ~close_EBADF ~connect ~write ~read

let _ = dest_net_ops

let _ = wf_unix_net_ops


(* hidden ----------------------------------------------------------- *)

(*
module Internal = struct
  (* now use the messaging library *)

  module Net_msg = Msg.Make(Internal_monad)

  include struct
    open M_
    let err e = Step(fun s -> {error=Some e},fun () -> failwith __LOC__)
  end

  let extra = Net_msg.{
    err;
    catch;
  }

(*
  (* now instantiate the unix net ops *)
  module Unix_net_ops = Make_unix_net_ops(Internal_monad)

  let net_ops = 
    let wrap_ops = Unix_net_ops.{
      wrap=(fun f -> M_.(Step(fun s -> 
        try s,(f () |> fun x -> fun () -> Finished x) 
        with Unix.Unix_error(e,s1,s2) -> 
          {error=Some(`Net_err (Unix_net_error e))},
          fun () -> failwith __LOC__)))
    }
    in
    Unix_net_ops.mk_unix_net_ops ~monad_ops ~wrap_ops

  let dest_net_ops = Unix_net_ops.dest_net_ops

  let net_msg = Net_msg.mk_net_msg ~monad_ops ~extra ~net_ops ~dest_net_ops
  let _ = net_msg
*)
end
*)

(*

(* export defns we need --------------------------------------------- *)

module Export : sig    
  (* type 'a m = 'a Internal_monad.m *)
  val send_string :
    conn:Unix.file_descr -> string_:string -> unit m
  val send_strings :
    conn:Unix.file_descr -> strings:string list -> unit m
  val recv_string : conn:Unix.file_descr -> string m
  val recv_strings : conn:Unix.file_descr -> string list m
end = struct
  open Internal
  type 'a m = 'a Internal_monad.m
  let (listen_accept,connect,send_string,send_strings,recv_string,recv_strings) = 
    net_msg @@ 
    fun ~listen_accept ~connect ~send_string ~send_strings ~recv_string ~recv_strings -> 
    (listen_accept,connect,send_string,send_strings,recv_string,recv_strings)

  let _ : conn:Unix.file_descr -> string_:string -> unit Net_msg.M_.m = send_string
end

*)


(*
module Unix_net_ops = struct

  module M_ = Internal_monad
  open M_

  (* wrap takes a unit -> 'a and returns an 'a m, that is guaranteed
     to only evalute (in the normal sense) the function when the world
     is passed; for unix, we also trap exceptions and return them in
     the monad *)
      

  type wrap_ops = {
    wrap: 'a.  (unit -> 'a) -> 'a m
  }


  let mk_unix_net_ops ~monad_ops ~wrap_ops = 

    let wrap = wrap_ops.wrap in

    let open Unix in
    (* socket call doesn't throw error; use lwt for guidance as to which
       calls need to be in monad *)

    let socket dom sock_ty prot_ty = 
      wrap @@ fun () -> socket dom sock_ty prot_ty in 

    let setsockopt fd opt b = wrap @@ fun () -> setsockopt fd opt b in

    let bind fd addr = wrap @@ fun () -> bind fd addr in

    let listen fd max_pending = wrap @@ fun () -> listen fd max_pending in

    let accept fd = wrap @@ fun () -> accept fd in

    let getpeername fd = wrap @@ fun () -> getpeername fd in

    let close_EBADF fd = wrap @@ fun () -> close fd in

    let connect fd addr = wrap @@ fun () -> connect fd addr in

    let write fd buf off len = wrap @@ fun () -> write fd buf off len in

    let read fd buf off len = wrap @@ fun () -> read fd buf off len in
    
    `Net_ops(socket,setsockopt,bind,listen,accept,getpeername,close_EBADF,connect,write,read)

  (* ensure types match up *)
  let wf_net_ops ops = 
    (* this for typing *)
    let _ = fun () -> 
      mk_unix_net_ops ~monad_ops:(failwith "") ~wrap_ops:(failwith "") = ops 
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

end

*)
