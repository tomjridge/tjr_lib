(* subset (the ones I needed at a certain point in time) of Unix, in monad *)

module Make_unix_net_ops = functor (M:Tjr_monad.MONAD) -> struct 

  module M_ = M
  open M_

  (* wrap takes a unit -> 'a and returns an 'a m, that is guaranteed
     to only evalute (in the normal sense) the function when the world
     is passed; for unix, we also trap exceptions and return them in
     the monad *)
      

  type wrap_ops = {
    wrap: 'a.  (unit -> 'a) -> 'a m
  }


  let mk_unix_net_ops ~monad_ops ~wrap_ops = 
    let (return,bind) = (monad_ops.return,monad_ops.bind) in
    let ( >>= ) = bind in

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

module Unix : sig (* FIXME *) end = struct

  module Step_monad = Tjr_monad.Step_monad

  type unix_net_error = Unix_net_error of Unix.error

  type state = {
    error: [ `Net_err of unix_net_error ] option
  }

  module Unix_monad = struct
    open Step_monad
    type 'a m = ('a,state) Step_monad.m
    let return = return
    let bind = bind
    type monad_ops = {
      return: 'a. 'a -> 'a m;
      bind: 'a 'b. 'a m -> ('a -> 'b m) -> 'b m
    }
    let monad_ops = { return;bind}
  end

  let monad_ops = Unix_monad.monad_ops
 
  (* now use the messaging library *)
    
  module Net_msg = Tjr_net_msg.Make(Unix_monad)

  include struct
    open Step_monad
    let err e = Step(fun s -> {error=Some e},fun () -> failwith __LOC__)
    let dest_exceptional s = s.error
    let rec catch f x = Step_monad.catch ~dest_exceptional f x
  end
  
  let extra = Net_msg.{
    err;
    catch;
  }

  (* now instantiate the unix net ops *)
  module Unix_net_ops = Make_unix_net_ops(Unix_monad)

  let net_ops = 
    let wrap_ops = Unix_net_ops.{
      wrap=(fun f -> Step_monad.(Step(fun s -> 
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

  let (send_string,send_strings,recv_string,recv_strings) = 
    net_msg @@ 
    fun ~send_string ~send_strings ~recv_string ~recv_strings -> 
    (send_string,send_strings,recv_string,recv_strings)
    
  let _ : conn:Unix.file_descr -> string_:string -> unit Net_msg.M_.m = send_string

  (* TODO export these 4 functions; the monad is Unix_monad *)


  (* FIXME it would actually be easier to use preprocessor and
     generate different versions :( *)

end



(*
    let wrap = fun x ->
      try
        return () >>= x 
      with Unix.Unix_error(e,s1,s2) -> err e
    in
    let _ = wrap in
*)




(* unix instance ---------------------------------------------------- *)

(* direct calls to OCaml's standard unix module, but with explicit error handling *)

(*
module Unix_ = struct

  

  type ('l,'e) state' = {
    lower_layer_error: 'l option;
    error: 'e option
  }

  (* not that we have a problem if the 'l and 'e are not fixed - have
     to define monad after these are fixed *)


  type state
  type 'a m = state -> state * [ `Finished of 'a | `Rest of unit -> 'a m]



  let err e = fun w -> { w with error=Some e}

  type 'a m = 

  let return_ : 'a. 'a -> ('a,'a*'m) Pub.m = fun x -> (x,failwith "")

  let _ = return_

  let monad_ops = {
    Pub.return=return_;
    bind=failwith "";
    err=failwith ""
  }

  let monad_ops = 

end
*)
