(** Simple state-passing monad *)

include Tjr_step_monad

(** Monadic reference operations *)
type ('a,'s) mref = {
  get: unit -> ('a,'s) m;
  set: 'a -> (unit,'s) m
}
