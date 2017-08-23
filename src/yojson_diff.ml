(* diff two yojson structures and produce a concrete repn of diff *)

open Yojson.Safe

let is_prim (x:json) = match x with
  | `Bool _
  | `Float _
  | `Int _
  | `Intlit _
  | `Null
  | `String _
  | `Variant _  (* FIXME not really prim *)
    -> true
  | _ -> false
    
let rec diff ~(j1:json) ~(j2:json) = 
  match (j1,j2) with
  (* primitive cases first *)
  | _,_ when is_prim j1 && is_prim j2 -> (if j1=j2 then `Same else `Diff(j2))

  (* compound cases *)
  | `Assoc xs1, `Assoc xs2 -> (
      if (xs1=xs2) then `Same else 
        List.combine xs1 xs2 |> fun xs -> (* FIXME combine lengths *)
        `Diff_assoc(diff_assoc xs))
  | `List xs1, `List xs2 -> (
      if (xs1=xs2) then `Same else 
        List.combine xs1 xs2 |> fun xs -> (* FIXME combine lengths *)
        `Diff_list (diff_list xs))
  | `Tuple xs1, `Tuple xs2 -> (
    if (xs1=xs2) then `Same else 
      List.combine xs1 xs2 |> fun xs -> (* FIXME combine lengths *)
      `Diff_tuple (diff_tuple xs))

and diff_assoc xs = 
  (* FIXME for nested structures we should really descend and extend
       path *)
  let f ((s1,j1),(s2,j2)) = 
    assert (s1=s2);  (* FIXME assume structure is the same *)
    diff ~j1 ~j2
  in
  List.map f xs

and diff_list xs = 
  xs |> List.map (fun (j1,j2) -> diff ~j1 ~j2)

and diff_tuple xs = 
  xs |> List.map (fun (j1,j2) -> diff ~j1 ~j2) 


(* applying a diff -------------------------------------------------- *)

let rec apply_diff (orig:json) d =
  match orig,d with
  | _, `Same -> orig
  | _,`Diff d when is_prim orig -> d
  | `Assoc xs1, `Diff_assoc xs2 -> (
      List.combine xs1 xs2 |> fun xs -> 
      let f ((s,j),d) = s,apply_diff j d in
      List.map f xs |> fun xs -> `Assoc xs)
  | `List xs1, `Diff_list xs2 -> (
      List.combine xs1 xs2 |> fun xs -> 
      let f (j,d) = apply_diff j d in
      List.map f xs |> fun xs -> `List xs)
  | `Tuple xs1, `Diff_tuple xs2 -> (
    List.combine xs1 xs2 |> fun xs -> 
    let f (j,d) = apply_diff j d in
    List.map f xs |> fun xs -> `Tuple xs)
  | _,_ -> failwith __LOC__  (* structure different etc *)


module Example = functor(_:sig end) -> struct


  (* example ---------------------------------------------------------- *)

  type person = {first:string; last:string; age: int; address:string list }
  [@@deriving yojson]

  let p1 = {first="Alf"; last="Bert"; age=1; address=["1 Amber Street";"London"] }

  let j1 = p1 |> person_to_yojson

  let p2 = {first="Alf"; last="Charlie"; age=1; address=["2 Amber Street";"London"] }

  let j2 = p2 |> person_to_yojson

  let d = diff ~j1 ~j2


  (* FIXME alternative is just to flatten the structure and then use a
     line-based diff algorithm *)


  let d' = apply_diff j1 d

end
