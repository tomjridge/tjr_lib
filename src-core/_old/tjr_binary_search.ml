(* binary search *)

type 'e elt_ops = {
  compare: 'e -> 'e -> int  (* 0 is equal; neg means e1 < e2; pos means e1 > e2 *)
}

type ('e,'t) array_ops = {
  get: 't -> int -> 'e;
  length: 't -> int;
}


(* unlike typical binary search, we want to return the bounding
   elements (if possible) if the target is not present *)

let binary_search ~elt_ops ~array_ops arr target =
  let {get;length} = array_ops in
  let get = get arr in
  let n = length arr in
  let {compare} = elt_ops in
  let rec loop ~l ~r =
    (* invariants: n>0; l and r are valid indexes in array (ie 0...n-1); l >= 0; l<=r;  *)
    assert(n > 0 && l>=0 && l<=r && r<n);
    match l < r with 
    | true -> ( 
      (* typical case *)
      let i = (l+r)/2 in
      assert(l<=i && i <= r);
      let e = get i in
      match compare e target with
      | 0 -> `Found_at i
      | x when x < 0 -> 
        (* e is less than target, so start searching from i+1 *)
        let l' = i+1 in
        (* l' = i+1 = (l+r)/2 +1; can this be bigger than r? yes, iff
           (l+r)/2 = r; but this iff r=0 which is not possible by
           invariant n>0*)
        assert(l' <= r); 
        (* it follows that l' < n since r < n *)
        assert(l' < n);
        loop ~l:l' ~r
      | x when x > 0 -> (
        (* e is greater than target so start searching from i-1 *)
        let r' = i -1 in
        (* r' = i -1 = (l+r)/2 -1; can this be smaller than l? yes, iff (l+r)/2 = l, iff r=l or (r=l+1 and l is even) *)
        match i=l with
        | true -> `Found_greater_than_at i
        | false -> (
            assert(r'>= l);
            loop ~l ~r:r'))
      | _ -> failwith "impossible") 
    | false ->
      assert(l=r);
      let i = l in
      let e = get i in
      match compare e target with
      | 0 -> `Found_at i
      | x when x < 0 -> `Found_less_than_at i  (* but greater than at i+1 if present *)
      | x when x > 0 -> `Found_greater_than_at i
      | _ -> failwith "impossible"
  in
  if n=0 
  then `Empty_array 
  else loop ~l:0 ~r:(n-1)

let _ = binary_search

let binary_search ~elt_ops ~array_ops arr target =
  let {get;length} = array_ops in
  let n = length arr in
  binary_search ~elt_ops ~array_ops arr target |> function
  | `Empty_array -> `Empty_array
  | `Found_at i -> `Found_at i
  | `Found_less_than_at i ->
    if i+1 < n then `Found_bounding_indexes (i,i+1) else `Greater_than_all_elts
  | `Found_greater_than_at i -> 
    if i > 0 then `Found_bounding_indexes (i-1,i) else `Less_than_all_elts



(* testing ---------------------------------------------------------- *)

module type Test = sig val r: bool end

(* this use of first class modules ensures the test code only runs
   when assertions are enabled, whilst keeping top-level declaration
   syntax; FIXME move this into a test executable *)
let _ = assert(
  let m = 
    (module struct
      let _ = Printf.printf "Assertions enabled; running tests in Tjr_binary_search\n"

      let elt_ops : int elt_ops = { compare=Stdlib.compare }

      (* test on lists; obviously bin search is not efficient on lists *)
      let array_ops = {
        get=(fun xs i -> List.nth xs i);
        length=(fun xs -> List.length xs)
      }

      let binary_search' = binary_search ~elt_ops ~array_ops

      let test () = 
        let xs = [0;2;3] in
        let binary_search' = binary_search' xs in
        List.map binary_search' [-1;0;1;2;3;4;5]

      let r = test ()

      let _ = 
        assert(r = 
               [`Less_than_all_elts; `Found_at 0; `Found_bounding_indexes (0, 1);
                `Found_at 1; `Found_at 2; `Greater_than_all_elts; `Greater_than_all_elts])


      let test () = 
        let xs = [0] in
        let binary_search' = binary_search' xs in
        List.map binary_search' [-1;0;1]

      let r = test ()

      let _ = assert(r=[`Less_than_all_elts; `Found_at 0; `Greater_than_all_elts])

      let r = true
    end : Test) 
  in
  let module M = (val m) in
  M.r)
