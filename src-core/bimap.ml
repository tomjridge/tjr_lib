(** A bidirectional map, implemented as a pair of sets of pairs (!) *)

(** Add a partition operation that takes a lo and a hi element, and
   returns the range between lo and hi inclusive FIXME move to tjr_lib
   and add map equivalent *)
module Set_extra(S:Set.S) = struct
  
  let geq elt s = 
    S.split elt s |> fun (_,b,s) -> 
    s |> (if b then S.add elt else fun x -> x)

  let leq elt s = 
    S.split elt s |> fun (s,b,_) -> 
    s |> (if b then S.add elt else fun x -> x)

  let partition3 lo hi m = 
    m |> geq lo |> leq hi
end

module Bimap_ops = struct
  type ('x,'y,'t) bimap_ops = {
    empty    : 't;
    add      : 'x*'y -> 't -> 't;
    remove   : 'x*'y -> 't -> 't;
    find_x   : 'x -> 't -> 'y list;
    find_y   : 'y -> 't -> 'x list;
    remove_x : 'x -> 't -> 't;
    remove_y : 'y -> 't -> 't;    
  }
end

type ('x,'y,'t) bimap_ops = ('x,'y,'t) Bimap_ops.bimap_ops


module Make_bimap(X:Stdlib.Set.OrderedType)(Y:Stdlib.Set.OrderedType) = struct
  module Xy = struct
    type t = X.t*Y.t
    let compare: t -> t -> int = fun (x1,y1) (x2,y2) -> 
      X.compare x1 x2 |> function
      | 0 -> Y.compare y1 y2
      | x -> x
  end

  module Yx = struct
    type t = Y.t*X.t
    let compare: t -> t -> int = fun (y1,x1) (y2,x2) -> 
      Y.compare y1 y2 |> function
      | 0 -> X.compare x1 x2
      | x -> x
  end

  module Set_xy = struct
    module S = Set.Make(Xy)
    include S
    include Set_extra(S)
  end

  module Set_yx = struct
    module S = Set.Make(Yx)
    include S
    include Set_extra(S)
  end

  type t = {
    xy: Set_xy.t;
    yx: Set_yx.t
  }

  let empty = {
    xy=Set_xy.empty;
    yx=Set_yx.empty
  }

  let add (x,y) t = 
    { xy = Set_xy.add (x,y) t.xy;
      yx = Set_yx.add (y,x) t.yx;
    }

  let remove (x,y) t = 
    { xy = Set_xy.remove (x,y) t.xy;
      yx = Set_yx.remove (y,x) t.yx;
    }


  let find_x x0 t = 
    let cmp a b = X.compare a b in
    let eq a b = cmp a b = 0 in
    (* NOTE the pred has to be monotonically increasing *)
    Set_xy.find_first_opt (fun (x,_) -> cmp x0 x <=0) t.xy |> function
    | None -> Set_xy.empty
    | Some lo -> 
      match eq (fst lo) x0 with 
      | false -> Set_xy.empty
      | true ->         
        (* NOTE the pred has to be monotonically decreasing *)
        Set_xy.find_last (fun (x,_) -> cmp x x0 <= 0) t.xy |> fun hi -> 
        Set_xy.partition3 lo hi t.xy

  let find_y y0 t = 
    let cmp a b = Y.compare a b in
    let eq a b = cmp a b = 0 in
    (* NOTE the pred has to be monotonically increasing *)
    Set_yx.find_first_opt (fun (y,_) -> cmp y0 y <=0) t.yx |> function
    | None -> Set_yx.empty
    | Some lo -> 
      match eq (fst lo) y0 with 
      | false -> Set_yx.empty
      | true ->         
        (* NOTE the pred has to be monotonically decreasing *)
        Set_yx.find_last (fun (y,_) -> cmp y y0 <= 0) t.yx |> fun hi -> 
        Set_yx.partition3 lo hi t.yx


  let remove_x x0 t =
    find_x x0 t |> fun s ->
    Set_xy.fold (fun (x,y) acc -> remove (x,y) acc) s t

  let remove_y y0 t = 
    find_y y0 t |> fun s ->
    Set_yx.fold (fun (y,x) acc -> remove (x,y) acc) s t


  let bimap_ops = Bimap_ops.{
    empty; 
    add;
    remove;
    find_x=(fun x0 t -> find_x x0 t |> Set_xy.to_seq |> List.of_seq |> List.map (fun (_,y) -> y));
    find_y=(fun y0 t -> find_y y0 t |> Set_yx.to_seq |> List.of_seq |> List.map (fun (_,x) -> x));
    remove_x;
    remove_y
  }

end

module Bimap_test() = struct

  module Int_ = struct
    type t = int
    let compare: t -> t -> int = Pervasives.compare
  end

  module B = Make_bimap(Int_)(Int_)

  let b0 = ListLabels.fold_left ~f:(fun a (x,y) -> B.add (x,y) a) ~init:B.empty
             [(1,1);(1,2);(1,3);(2,2);(3,2);(4,2)]

  let bimap_ops = B.bimap_ops

  open B
  let to_list t = t.xy |> Set_xy.to_seq |> List.of_seq

  let to_string t = t |> to_list |> List.map (fun (x,y) -> Printf.sprintf "(%d,%d)" x y) |> String_.concat_strings ~sep:";" |> Printf.sprintf "[ %s ]"

  let _ = 
    assert(bimap_ops.find_x 1 b0 = [1;2;3]);
    assert(bimap_ops.find_y 2 b0 = [1;2;3;4]);
    assert(bimap_ops.find_y 3 b0 = [1]);
    assert(bimap_ops.find_x 5 b0 = []);
    Printf.printf "Bimap tests passed!\n"
end
