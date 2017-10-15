(* messaging over a connection *)



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


open Unix

(* Lwt_unix has file_descr <> Unix.file_descr *)

type 'a conn = File_descr of 'a
type ip = inet_addr
type port = int
type ipp = sockaddr (*  expect ADDR_INET ip * port *)
type quad = { local:ipp; remote: ipp }

open Tjr_monad.Private

type extra_ops = {
  (* run whether error or result; any errors ignored *)
  finalize: 'a 'm 'n. (unit -> (unit,'n)m) -> ('a,'m)m -> ('a,'m)m;
  (* run on error *)
  catch: 'a 'm 'n. (unit -> (unit,'n)m) -> ('a,'m)m -> ('a,'m)m;
}

let mk (type fd) ~monad_ops ~net_ops ~dest_net_ops ~extra =
  let (return,bind,err) = Tjr_monad.(monad_ops.return,monad_ops.bind,monad_ops.err) in
  let ( >>= ) = bind in

  dest_net_ops net_ops @@ fun ~socket ~setsockopt ~bind ~listen ~accept ~getpeername ~close ~connect ~write ~read ->
  
  (* accept connections for this quad only *)
  let listen_accept ~quad = 
    socket PF_INET SOCK_STREAM 0 >>= fun (srvr:fd) ->
    extra.finalize (fun () -> close srvr) (
      (* hack to speed up recovery *)
      setsockopt srvr SO_REUSEADDR true >>= fun () ->
      let addr = quad.local in
      bind srvr addr >>= fun () ->
      listen srvr 5 >>= fun () ->
      accept srvr >>= fun (c,_) ->
      if getpeername c <> quad.remote then 
        (* connection doesn't match quad *)
        close c >>= fun () -> err `Error_incorrect_peername
      else
        return c)
  in


  let connect ~quad = 
    socket PF_INET SOCK_STREAM 0 >>= fun (c:fd) ->
    extra.catch (fun () -> close c) (
      (* hack to speed up recovery *)
      setsockopt c SO_REUSEADDR true >>= fun () ->
      bind c quad.local >>= fun () ->
      connect c quad.remote >>= fun () ->
      return c)
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

  let read_length ~conn : (int,'m)m = 
    return () >>= fun () ->
    Bytes.create 4 |> fun buf ->
    read_n ~conn ~buf ~off:0 ~len:4 >>= fun () ->
    bs2i ~buf ~off:3 ~n:4 |> fun i ->
    return i
  in

  let recv_string ~conn : (string,'m)m = 
    return () >>= fun () ->
    read_length ~conn >>= fun len -> 
    Bytes.create len |> fun buf ->          
    read_n ~conn ~buf ~off:0 ~len >>= fun () ->
    Bytes.unsafe_to_string buf |> return
  in


  (* FIXME marshal is a bit platform-specific? *)
  let recv_strings ~conn : (string list,'m)m = 
    return () >>= fun () ->
    recv_string ~conn >>= fun s ->
    Marshal.from_string s 0 |> fun (ss:string list) ->
    return ss
  in

  ()

