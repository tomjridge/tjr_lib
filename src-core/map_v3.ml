(** NOTE this is just draft scribblings *)

module type S = 
sig
  type key
  type value
  type t
  (* val compare: key -> key -> int *)
  val empty : t
  val is_empty : t -> bool
  val mem : key -> t -> bool
  val add : key -> value -> t -> t
  val update : key -> (value option -> value option) -> t -> t
  val singleton : key -> value -> t
  val remove : key -> t -> t
  val merge :
    (key -> value option -> value option -> value option) -> t -> t -> t
  val union : (key -> value -> value -> value option) -> t -> t -> t
  val compare : (value -> value -> int) -> t -> t -> int
  val equal : (value -> value -> bool) -> t -> t -> bool
  val iter : (key -> value -> unit) -> t -> unit
  val fold : (key -> value -> 'b -> 'b) -> t -> 'b -> 'b
  val for_all : (key -> value -> bool) -> t -> bool
  val exists : (key -> value -> bool) -> t -> bool
  val filter : (key -> value -> bool) -> t -> t
  val filter_map : (key -> value -> value option) -> t -> t
  val partition : (key -> value -> bool) -> t -> t * t
  val cardinal : t -> int
  val bindings : t -> (key * value) list
  val min_binding : t -> key * value
  val min_binding_opt : t -> (key * value) option
  val max_binding : t -> key * value
  val max_binding_opt : t -> (key * value) option
  val choose : t -> key * value
  val choose_opt : t -> (key * value) option
  val split : key -> t -> t * value option * t
  val find : key -> t -> value
  val find_opt : key -> t -> value option
  val find_first : (key -> bool) -> t -> key * value
  val find_first_opt : (key -> bool) -> t -> (key * value) option
  val find_last : (key -> bool) -> t -> key * value
  val find_last_opt : (key -> bool) -> t -> (key * value) option
  val map : (value -> value) -> t -> t
  val mapi : (key -> value -> value) -> t -> t
  val to_seq : t -> (key * value) Seq.t
  val to_rev_seq : t -> (key * value) Seq.t
  val to_seq_from : key -> t -> (key * value) Seq.t
  val add_seq : (key * value) Seq.t -> t -> t
  val of_seq : (key * value) Seq.t -> t
end
 

type ('k,'v) map_kv = (module S with type key='k and type value='v)

(** NOTE introduces an existential type when unpacked *)
let make (type key value) compare = 
  let module C = struct type t = key let compare = compare end in
  let module S = struct
    type nonrec value = value
    let compare = compare 
    include Map.Make(C) 
    type nonrec t = value t
  end 
  in
  ((module S) : (key,value) map_kv)

(* following hard to make? *)
type ('k,'v,'t) map = (module S with type key='k and type value='v and type t = 't)

module Example() = struct
  module S = Int
  module T = struct
    include Map.Make(S)
    type value = int
    type nonrec t = value t
  end
  let t : (int,int,T.t) map = (module T)
end
