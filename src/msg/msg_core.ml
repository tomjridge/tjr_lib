(* core msg types *)

open Msg_prelude

module M : sig 
  type 'a m
  val return: 'a -> 'a m
  val bind: 'a m -> ('a -> 'b m) -> 'b m
  val catch: ('e -> 'a m) -> 'a m -> 'a m
end = struct
  type 'a m = 'a
  let return: 'a -> 'a m = fun x -> x
  let bind: 'a m -> ('a -> 'b m) -> 'b m = fun x f -> f x
  let catch x = failwith ""
end

open M (* replace this with the particular monad required; see Makefile *)


type 'file_descr net_ops = 
  [`Net_ops of
            (Unix.socket_domain -> Unix.socket_type -> int -> 'file_descr m) *
              ('file_descr -> Unix.socket_bool_option -> bool -> unit m) *
              ('file_descr -> Unix.sockaddr -> unit m) *
              ('file_descr -> int -> unit m) *
              ('file_descr -> ('file_descr * Unix.sockaddr) m) *
              ('file_descr -> Unix.sockaddr m) *
              ('file_descr -> unit m) *
              ('file_descr -> Unix.sockaddr -> unit m) *
              ('file_descr -> bytes -> int -> int -> int m) *
              ('file_descr -> bytes -> int -> int -> int m) ]
    
let wf_net_ops ops (ops:'fd net_ops) = true

let dest_net_ops (ops:'a net_ops) = ops |> function
    `Net_ops(socket,setsockopt,bind,listen,accept,getpeername,close_EBADF,connect,write,read) ->
    fun k ->
      k ~socket ~setsockopt ~bind ~listen ~accept ~getpeername ~close_EBADF ~connect ~write ~read


let mk_msg_lib (type fd) ~(net_ops:fd net_ops) ~dest_net_ops =
  let ( >>= ) = bind in
(*  let err = extra.err in *)

  (* ASSUMES close_EBADF should ensure that we only return EBADF on error *)
  dest_net_ops net_ops @@ fun ~socket ~setsockopt ~bind ~listen ~accept ~getpeername ~close_EBADF ~connect ~write ~read ->

  (* accept connections for this quad only *)
  let listen_accept ~quad = 
    socket Unix.PF_INET Unix.SOCK_STREAM 0 >>= fun (srvr:fd) ->
    begin
      (* hack to speed up recovery *)
      setsockopt srvr Unix.SO_REUSEADDR true >>= fun () ->
      bind srvr quad.local >>= fun () -> 
      listen srvr 5 >>= fun () ->
      accept srvr >>= fun (c,_) ->
      getpeername c >>= fun pn -> 
      if pn <> quad.remote then 
        (* connection doesn't match quad *)
        close_EBADF c >>= fun () -> return `Error_incorrect_peername
      else
        return @@ `Connection c
    end
    |> catch (function 
      (* NOTE this is an error from the lower level *)
      | e -> close_EBADF srvr >>= fun () -> return @@ `Net_err e)
  in

  let _ = listen_accept in  
  (* FIXME would be nice to know which errors each function could
     throw... include this in monad type? *)


  let connect ~quad = 
    socket Unix.PF_INET Unix.SOCK_STREAM 0 >>= fun (c:fd) ->
    begin
      (* hack to speed up recovery *)
      setsockopt c Unix.SO_REUSEADDR true >>= fun () ->
      bind c quad.local >>= fun () ->
      connect c quad.remote >>= fun () ->
      return @@ `Connection c
    end
    |> catch (function
      | e -> close_EBADF c >>= fun () -> return @@ `Net_err e)
  in


  (* send, recv ------------------------------------------------------- *)

  (* send length as 4 bytes, then the string itself; NOTE for
     performance, it is quite important to try to call write with a
     buffer which includes everything to do with the message *)
  let send_string ~conn ~string_ =
    return () >>= fun () ->
    String.length string_ |> fun len ->
    let buf = Bytes.create (4+len) in
    i2bs ~buf ~off:0 ~i:len ~n:4;
    Bytes.blit_string string_ 0 buf 4 len;
    (* now write the buffer *)
    write conn buf 0 (4+len) >>= fun nwritten ->
    assert_(nwritten=4+len);  (* FIXME or loop? *)
    return ()
  in


  (* send nstrings, followed by strings *)
  let send_strings ~conn ~(strings:string list) =
    Marshal.to_string strings [] |> fun string_ ->
    send_string ~conn ~string_
  in

  (* actually read len bytes *)
  let rec read_n ~conn ~buf ~off ~len = 
    return () >>= fun () ->
    len |> function
    | 0 -> return ()
    | _ -> 
      read conn buf off len >>= fun nread ->
      read_n ~conn ~buf ~off:(off+nread) ~len:(len-nread)
  in

  let read_length ~conn : int m = 
    return () >>= fun () ->
    Bytes.create 4 |> fun buf ->
    read_n ~conn ~buf ~off:0 ~len:4 >>= fun () ->
    bs2i ~buf ~off:3 ~n:4 |> fun i ->
    return i
  in

  let recv_string ~conn : string m = 
    return () >>= fun () ->
    read_length ~conn >>= fun len -> 
    Bytes.create len |> fun buf ->          
    read_n ~conn ~buf ~off:0 ~len >>= fun () ->
    Bytes.unsafe_to_string buf |> return
  in


  (* FIXME marshal is a bit platform-specific? *)
  let recv_strings ~conn : string list m = 
    return () >>= fun () ->
    recv_string ~conn >>= fun s ->
    Marshal.from_string s 0 |> fun (ss:string list) ->
    return ss
  in

  fun k -> k ~listen_accept ~connect ~send_string ~send_strings ~recv_string ~recv_strings


let _ = mk_msg_lib
