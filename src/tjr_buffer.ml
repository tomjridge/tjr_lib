(* a resizable, mutable buffer -------------------------------------- *)

(* NOTE primarily this is intended for filesystems *)

(*

Requirements:

- based on bigarrays
- resizable
- persistence is not necessary 

Operations to support:

- blitting to/from strings?/bigarrays, over ranges
- truncation - bigger and smaller

*)

type bigarray = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type 'buf buf_ops = {

  create: int -> 'buf;

  len: 'buf -> int;

  resize: int -> 'buf -> 'buf;

  to_string: 'buf -> string;

  (* get_internal: 'buf -> bigarray * int;  (* internal repn, and size *) *)
  (* don't use bigarray blit operations - the buffer is mutable anyway
     - but we need to make sure the size is updated if needed *)
  (* NOTE use state passing style, so we can swap out easily *)
  blit_bigarray_to_buf: 
    src:bigarray -> soff:int -> len:int -> dst:'buf -> doff:int -> 'buf;

  blit_buf_to_bigarray: 
    src:'buf -> soff:int -> len:int -> dst:bigarray -> doff:int -> unit;
}

(* impl ------------------------------------------------------------- *)

type buf = {
  ba:bigarray;
  size_:int
}

module Biga = struct
  let length ba = Bigarray.Array1.dim ba

  let create n = Bigarray.(Array1.(create char c_layout n))
end


let blit_bigarray ~src ~soff ~len ~dst ~doff = 
  Bigarray.Array1.sub src soff len |> fun src ->
  Bigarray.Array1.sub dst doff len |> fun dst ->
  Bigarray.Array1.blit src dst

let blit_bigarray_to_bytes ~src ~soff ~len ~dst ~doff = 
  for i=0 to len-1 do
    Bytes.set dst (doff+i) (Bigarray.Array1.get src (soff+i));
    ()
  done

let bigarray_to_string ~src ~off ~len = 
  (* ASSUMES not (len < 0 or n > Sys.max_string_length) *)
  match len < 0 || len > Sys.max_string_length with
  | true -> 
    (Printf.sprintf "bigarray_to_string: len is %d\n" len |> print_endline);
    failwith __LOC__
  | false ->
    Bytes.create len |> fun dst ->
    blit_bigarray_to_bytes ~src ~soff:off ~len ~dst ~doff:0;
    Bytes.unsafe_to_string dst

let zero = Biga.create 0

let ba0 = {ba=zero; size_=0}

let mk_buf_ops () = 
  let create len = 
    if len = 0 then ba0 else
      {
        ba=Biga.create len;
        size_=len 
      } 
  in
  let len' b = b.size_ in
  let len = len' in
  let resize len buf = 
    let ba_len = Biga.length buf.ba in
    match () with
    | _ when len = 0 -> ba0
    | _ when len < ba_len -> {buf with size_=len}
    | _ -> 
      (* FIXME may need to init new bytes *)
      (* need to actually resize and do some copying *)
      (* either double the size, or (if not enough) then use the
           requested len *)
      let new_len = max len (2*ba_len) in
      (* FIXME new buf is not initialized *)
      let new_buf = Biga.create new_len in
      (* copy contents *)
      blit_bigarray 
        ~src:buf.ba ~soff:0 ~len:buf.size_ ~dst:new_buf ~doff:0;
      {ba=new_buf; size_=len}
  in
  let to_string buf = bigarray_to_string ~src:buf.ba ~off:0 ~len:buf.size_ in
  let blit_bigarray_to_buf ~src ~soff ~len ~dst ~doff = 
    (* assume buf is large enough, or resize? assume *)
    (if len' dst < doff + len then failwith __LOC__);
    let ba = dst.ba in
    blit_bigarray ~src ~soff ~len ~dst:ba ~doff;
    {dst with ba}  (* ba is mutable, so not necessary *)
  in
  let blit_buf_to_bigarray ~src ~soff ~len ~dst ~doff = 
    (if len' src < soff+len && len <> 0 then failwith __LOC__);
    (if Biga.length dst < doff+len && len <> 0 then failwith __LOC__);
    let ba = src.ba in
    blit_bigarray ~src:ba ~soff ~len ~dst ~doff;
    ()
  in
  { create; len; resize; to_string; blit_bigarray_to_buf; blit_buf_to_bigarray }
