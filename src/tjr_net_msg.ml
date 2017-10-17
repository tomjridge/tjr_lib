(* messaging over a connection *)


(* aux -------------------------------------------------------------- *)


(* int <-> byte_x4 conversion *)

(* assert that is always checked *)
let assert_ b = if b then () else assert false

let base=256

let strip_byte i = i/base,i mod base

let co_strip_byte b i = i*base+b

(* strip n bytes from i and write into buf *)
let i2bs ~buf ~off ~i ~n =
  assert_ (i>=0);
  let rec f ~off ~i ~n = n |> function
    | 0 -> ()
    | _ ->
      i |> strip_byte |> fun (i,b) ->
      Bytes.set buf off (b |> Char.chr);
      f ~off:(off+1) ~i ~n:(n-1)
  in
  f ~off ~i ~n

(* read n bytes from buf (going down from off!) and return int *)
let bs2i ~buf ~off ~n = 
  let rec f ~off ~i ~n = n |> function
    | 0 -> i
    | _ -> 
      Bytes.get buf off |> Char.code |> fun b ->
      co_strip_byte b i |> fun i ->
      f ~off:(off-1) ~i ~n:(n-1)
  in
  f ~off ~i:0 ~n


let _ = assert (
  let i = 123456 in
  Bytes.create 4 |> fun buf ->
  i2bs ~buf ~off:0 ~i ~n:4;
  bs2i ~buf ~off:3 ~n:4 = i)



(* messaging -------------------------------------------------------- *)

open Unix (* for PF_NET SOCK_STREAM etc *)

(* Lwt_unix has file_descr <> Unix.file_descr *)

(* type 'a conn = File_descr of 'a *)
type ip = inet_addr
type port = int
type ipp = sockaddr (*  expect ADDR_INET ip * port *)
type quad = { local:ipp; remote: ipp }


module Make = functor (M:Tjr_monad.MONAD) -> struct

  module M_ = M
  open M_

  type 'e extra_ops = {
    err: 'a. 'e -> 'a m;
    (* run whether error or result; any errors ignored *)
    (* finalize: 'a. (unit -> unit m) -> 'a m -> 'a m; *)
    (* finally: 'a. ('e -> 'a m) -> 'a m -> 'a m; *)
    (* run on error *)
    (*catch: 'a. (unit -> unit m) -> 'a m -> 'a m;*)
    catch: 'a. ('e -> 'a m) -> 'a m -> 'a m;
  }


  let mk_net_msg (type fd) ~monad_ops ~extra ~net_ops ~dest_net_ops =
    let (return,bind) = (monad_ops.return,monad_ops.bind) in
    let ( >>= ) = bind in
    let err = extra.err in

    (* ASSUMES close_EBADF should ensure that we only return EBADF on error *)
    dest_net_ops net_ops @@ fun ~socket ~setsockopt ~bind ~listen ~accept ~getpeername ~close_EBADF ~connect ~write ~read ->

    (* accept connections for this quad only *)
    let listen_accept ~quad = 
      socket PF_INET SOCK_STREAM 0 >>= fun (srvr:fd) ->
      begin
        (* hack to speed up recovery *)
        setsockopt srvr SO_REUSEADDR true >>= fun () ->
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
      |> extra.catch (function 
          (* NOTE this is an error from the lower level *)
          | `Net_err e -> close_EBADF srvr >>= fun () -> return @@ `Net_err e)
    in

    let _ = listen_accept in  
    (* FIXME would be nice to know which errors each function could
       throw... include this in monad type? *)


    let connect ~quad = 
      socket PF_INET SOCK_STREAM 0 >>= fun (c:fd) ->
      begin
        (* hack to speed up recovery *)
        setsockopt c SO_REUSEADDR true >>= fun () ->
        bind c quad.local >>= fun () ->
        connect c quad.remote >>= fun () ->
        return @@ `Connection c
      end
      |> extra.catch (function
          | `Net_err e -> close_EBADF c >>= fun () -> return @@ `Net_err e)
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
    let send_strings ~conn ~strings =
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

    fun k -> k ~send_string ~send_strings ~recv_string ~recv_strings


  let _ = mk_net_msg


end

