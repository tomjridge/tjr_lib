(* various monads *)

module type MONAD = sig

  type 'a m

  type monad_ops = {
    return: 'a. 'a -> 'a m;
    bind: 'a 'b. 'a m -> ('a -> 'b m) -> 'b m
  }

end

module Free = struct

  type 'a m = 
    | Return: 'a -> 'a m
    | Bind: 'a m * ('a -> 'b m) -> 'b m

  (* NB side-effecting functions should be put into the monad first eg
     not f, but fun () -> f, and then this should be invoked at the right
     point *)

  
  let return x = Return x
  let bind x f = Bind(x,f) 
  

  type monad_ops = {
    return: 'a. 'a -> 'a m;
    bind: 'a 'b. 'a m -> ('a -> 'b m) -> 'b m
  }

  let monad_ops = { return; bind }


  module Example = struct

    module U = Unix

    type 'e extra_ops = {
      (* run whether error or result; any errors ignored *)
      finalize: 'a. (unit -> unit m) -> 'a m -> 'a m;
      (* run on error *)
      catch: 'a. (unit -> unit m) -> 'a m -> 'a m;
      err: 'a. 'e -> 'a m
    }

    type ipp = U.sockaddr (*  expect ADDR_INET ip * port *)

    type quad = { local:ipp; remote: ipp }


    let ex (type fd) 
        ~socket ~close ~setsockopt ~bind ~listen ~accept ~getpeername ~extra ~quad 
      = 
      let (return,bind') = (monad_ops.return,monad_ops.bind) in
      let ( >>= ) = bind' in
      return () >>= fun () ->
      socket U.PF_INET U.SOCK_STREAM 0 >>= fun (srvr:fd) ->
      extra.finalize (fun () -> close srvr) (
        return () >>= fun () ->
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

(*
    let rec lwt_interpreter : type a. a m -> a Lwt.t = function
      | Return x -> Lwt.return x
      | Bind(x,f) -> Lwt.bind (lwt_interpreter x) (fun r -> lwt_interpreter (f r))
*)
(*
    let lwt_ex ~extra ~quad = 
      Lwt_unix.(
        (* FIXME to avoid wrapping all calls, perhaps we just define an interpreter directly rather than giving this example structure ? *)
        let socket x y z = return (socket x y z) in
        let _ = socket in
        ex ~socket ~close ~setsockopt ~bind ~listen ~accept ~getpeername ~extra ~quad)
*)
  end

end


module Double_negation = struct

end


module Public = struct
  type ('a,'m) m = 'm

  type ('e,'m) monad_ops = {
    return: 'a. 'a -> ('a,'m)m;
    bind: 'a 'b. ('a,'m)m -> ('a -> ('b,'m)m) -> ('b,'m)m;
    err: 'a. 'e -> ('a,'m)m
  }
end

module Private : sig
  type ('a,'m) m
  type ('e,'m) monad_ops = {
    return: 'a. 'a -> ('a,'m)m;
    bind: 'a 'b. ('a,'m)m -> ('a -> ('b,'m)m) -> ('b,'m)m;
    err: 'a. 'e -> ('a,'m)m
  }
  val reveal: ('a,'m) m -> ('a,'m)Public.m
  val inject: 'a -> 'm -> ('a,'m)m
end = struct
  include Public
  let reveal x = x
  let inject = fun x m -> m
end
  


module Step_monad = struct

  (* similar to dn monad, but simpler *)

  type ('a,'w) m = Finished of 'a | Step of ('w -> 'w * (unit -> ('a,'w) m))

  let return (x:'a) : ('a,'w) m = Finished x

  let rec bind (x:('a,'w)m) (f:'a -> ('b,'w)m) : ('b,'w)m = 
    match x with 
    | Finished a -> f a
    | Step g -> Step(fun w -> g w |> fun (w',rest) -> (w',fun () -> bind (rest()) f))

  let ( >>= ) = bind

  let with_state (type a w b) (f:w -> a*w) (g:a->(b,w)m) : (b,w)m = 
    Step(fun w ->
        f w |> fun (a,w') ->
        w',fun () -> g a)

  let catch ~dest_exceptional (handle_:'e -> ('a,'w)m) =
    let rec g (x:('a,'w)m) = 
      match x with 
      | Finished x -> Finished x
      | Step f ->
        Step(fun s ->
          match dest_exceptional s with
          | None -> 
            f s |> fun (s',rest) ->
            s',fun () -> g (rest()))
    in
    g
  


  (* state passing with error *)

(* FIXME this should not be with error? *)
  type ('e,'w,'m) monad_ops = {
    return: 'a. 'a -> ('a,'m)m;
    bind: 'a 'b. ('a,'m)m -> ('a -> ('b,'m)m) -> ('b,'m)m;
(*    err: 'a. 'e -> ('a,'m)m; *)
  }


end
