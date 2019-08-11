(* generic tree routines *)
(* FIXME commenting to remove ppx_sexp_conv, which is messign up other ppxs 

open Sexplib.Std

(* the id is useful when treating the tree as a graph *)
type ('n,'l) tree = 
  | N of ('n,'l)node
  | L of 'l leaf 

and ('n,'l) node = { nid:int option; nlbl: 'n; cs: ('n,'l) tree list } 

and 'l leaf = { lid:int option; llbl:'l} 
[@@deriving sexp]

let _N (nlbl,cs) = N{nid=None; nlbl; cs}
let _L llbl = L{lid=None;llbl}

(* these just for short type annotations *)
type n
type l
type t = (n,l)tree

let rec subtrees t = (
  match t with
  | N {nid;nlbl;cs} -> t :: (
      cs |> List.map subtrees |> List.concat)
  | L xs -> [t])

(*
let mapi = Tjr_list.mapi

let index xs = mapi ~f:(fun i t -> (t,i)) xs

let _ = index

let index_subtrees t : (t * int) list = 
  t |> subtrees |> index
*)

let rec tree_map ~(fid:int option->int option) ~(fn:'n -> 'm) 
    ~(fl:'l -> 'h) ~t 
  = (
  let rec f t = 
    match t with
    | N n -> N{nid=(fid n.nid); nlbl=(fn n.nlbl); cs=(List.map f n.cs)}
    | L n -> L{lid=(fid n.lid); llbl=(fn n.llbl)}
  in
  f t)
          
(* n always takes a N; l always takes a leaf *)
let rec tree_fold' ~n ~l t = (
  match t with
  | L {lid;llbl} -> l ~lid ~llbl
  | N{nid;nlbl;cs} -> (
      let cs =List.map (tree_fold' ~n ~l) cs in
      n ~nid ~nlbl ~cs))

let tree_index t = (
  let r = ref 0 in
  tree_map ~fid:(fun id -> r:=!r+1;Some !r) ~fn:(fun x -> x) 
    ~fl:(fun x -> x) ~t
)

let get_id x = (
  match x with
  | N n -> n.nid
  | L n -> n.lid)


(* text specification of trees *)
module Tree_from_text = struct

  (* first string is the id *)
  type tree = 
      N of string * string * tree list 
    | L of string * string  [@@deriving yojson]

  let _f () = tree_to_yojson  (N("n1","hello",[L("l1","world"); L("l2","asdsa")]))

end
*)
