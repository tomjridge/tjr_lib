(** Maps based on first class modules *)

open Map

module type Intf = sig
  type k
  type v
  type t
  val empty: t
  val mem: k -> t -> bool
  val add: k -> v -> t -> t
  (* etc *)
end

module Pvt = struct

  module type S = sig
    type k
    type v
    val k_cmp: k -> k -> int
  end

  module Make(S:S) = struct
    include S

    module Ord = struct type t = k let compare = k_cmp end
    
    module M = Map.Make(Ord)

    type t = v M.t

    let empty = M.empty
    let mem = M.mem
    let add = M.add
  end

  module Ex = Make(struct type k =int type v = int let k_cmp = Stdlib.compare end)

  module Ex' = (Ex : Intf)

end
open Pvt

type ('k,'v,'t) map = (module Intf with type k='k and type v='v and type t='t)


module Pvt_example = struct

  let int_int_map : (int,int,Ex.t) map = (module Pvt.Ex)

  (* example: a map carrying its operations with it *)

  let m = (int_int_map,Ex.empty)

end
