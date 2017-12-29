(* a simple step monad ---------------------------------------------- *)

open Tjr_either

type ('a,'w) step_monad = Step of ('w -> 'w * ('a,('a,'w)step_monad) either)

type ('a,'w)k_ = ('a,('a,'w)step_monad) either

type ('a,'w)sm = ('a,'w)step_monad

let dest_Step (Step f) = f

let return a = Step(fun w -> (w,Inl a))

let rec bind : ('a,'w)sm -> ('a -> ('b,'w)sm) -> ('b,'w)sm = 
  fun a b ->
    Step(fun w ->
      dest_Step a |> fun a ->
      a w |> fun (w',rest) ->
      (w',
       match rest with
       | Inl a -> Inr(b a)
       | Inr a -> Inr(bind a b)))

let _ = bind


let rec fmap f a = 
  a |> dest_Step |> fun a ->
  Step(fun w -> 
      a w |> fun (w,rest) ->
      (w,
       match rest with
       |Inl a -> Inl (f a)
       | Inr a -> Inr (fmap f a)))

let _ = fmap

(* FIXME why not just lift a? *)
(*
let with_state 
    (type a b w) (a:w -> a*w) (b:a->(b,w)step_monad) : (b,w) step_monad
  =
  let a : (a,w)step_monad = Step(fun w -> a w |> fun (a,w) -> (w,Inl a)) in
  bind a b
*)

let with_state (type a b w) (a:w -> a*w) : (a,w)sm = 
  Step(fun w -> a w |> fun (a,w) -> (w,Inl a))

(* NOTE to run, we assume that there is some predicate
   dest_exceptional that indicates that we are not supposed to step
   further; in concrete cases, we must always check this condition before
   calling run *)

let run ~dest_exceptional w a =
  let rec run w a = 
    match dest_exceptional w with
    | Some _ -> Error (`Attempt_to_step_exceptional_state w)
    | None -> 
      dest_Step a |> fun a ->
      a w |> fun (w,rest) -> 
      match rest with 
      | Inl a -> Ok(w,a)
      | Inr a -> run w a
  in
  run w a
