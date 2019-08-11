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

include struct
open Unix (* for PF_NET SOCK_STREAM etc *)

(* Lwt_unix has file_descr <> Unix.file_descr *)

(* type 'a conn = File_descr of 'a *)
type ip = inet_addr
type port = int
type ipp = sockaddr (*  expect ADDR_INET ip * port *)
type quad = { local:ipp; remote: ipp }
end

