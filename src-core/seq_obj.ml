(** Sequences *)

(**
Example:

{[
    let range (l:int) (h:int) = 
      seq_f#make (fun x -> x+1) l |> 
      seq_f#map (fun x -> if x >= h then None else Some x) 

    let _ = range

    let nums = (range 0 10) |> seq_f#peek_filter 100 

    let _ = assert(nums=[0;1;2;3;4;5;6;7;8;9])

    let nums = (range 0 10) |> seq_f#drop 5 |> seq_f#peek_filter 5

    let _ = assert(nums=[5;6;7;8;9])

]}

Object seq_f has type:
{[
< drop : int -> 'a seq2 -> 'a seq2;
  make : ('a -> 'a) -> 'a -> (< get : 'a; next : 'c > as 'c);
  map : ('a -> 'b) -> 'a seq2 -> 'b seq2; peek : int -> 'a seq2 -> 'a list;
  peek_filter : int -> 'a option seq2 -> 'a list;
  read : int -> 'a seq2 -> 'a list * 'a seq2;
  read_filter : int -> 'a option seq2 -> 'a list * 'a option seq2 >

]}

*)


let iter_k f (x:'a) =
  let rec k x = f ~k x in
  k x

let _ 
: (k:('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
= iter_k
  

(* eg use:
let _ = 
  iter_k (fun ~k n -> 
      if n = 0 then 1 else n * k (n-1))
*)

type 'a seq1 = 'a 

type 'a seq2 = <
  get: 'a;
  next: 'a seq2
>

(** f for functional; m for mutable *)
let seq_f = object (s1)
  method read n (seq: _ seq2) = 
    (n,seq,([]:'a list)) |> iter_k (fun ~k (n,seq,(acc:'a list)) -> 
        if n<=0 then (List.rev acc,seq) 
        else k (n-1,seq#next,seq#get::acc))

  (** filters out the none elts; FIXME this requires a fun to 'a opt *)
  method read_filter n (seq: 'a option seq2) = 
    (n,seq,([]:'a list)) |> iter_k (fun ~k (n,seq,(acc:'a list)) -> 
        if n<=0 then (List.rev acc,seq) 
        else 
          match seq#get with
          | None -> k (n-1,seq#next,acc)
          | Some x -> k (n-1,seq#next,x::acc))

  method map (f:'a -> 'b) (seq: 'a seq2) : 'b seq2 = object
    method get = seq#get |> f
    method next = seq#next |> s1#map f
  end

  method peek n seq = s1#read n seq |> fst

  method peek_filter n seq = s1#read_filter n seq |> fst

  method drop n seq = s1#read n seq |> snd

  method make (step:'a -> 'a ) (init:'a) = object (s3)
    method get = init
    method next = s1#make step (step init)
  end
(*
        method make_map ~map ~init : 'b seq2 = object (s6)
          method get = map init
          method next = s3#make_map ~map ~init:(step_ init)
        end
*)
end

let _ = seq_f


(* examples *)

let seq_example () = 
  let open struct

    let range (l:int) (h:int) = 
      seq_f#make (fun x -> x+1) l |> 
      seq_f#map (fun x -> if x >= h then None else Some x) 

    let _ = range

    let nums = (range 0 10) |> seq_f#peek_filter 100 

    let _ = assert(nums=[0;1;2;3;4;5;6;7;8;9])

    let nums = (range 0 10) |> seq_f#drop 5 |> seq_f#peek_filter 5

    let _ = assert(nums=[5;6;7;8;9])

    let _ = seq_f
  end
  in
  ()


(*
    method with_step_opt (step: 'a -> 'a option) = object (s7)
    end
*)


(*
      method plain = object (s4)
        method make ~(init:'a) = (init : _ seq1)
        method read ~n ~(seq:'a seq1) = 
          (n,seq,([]:'a list)) |> iter_k (fun ~k (n,seq,(acc:'a list)) -> 
              if n<=0 then (List.rev acc,seq) 
              else k (n-1,step_ seq,seq::acc))
        method take ~n ~seq = s4#read ~n ~seq |> fst
        method drop ~n ~seq = s4#read ~n ~seq |> snd
      end
*)
