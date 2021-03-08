(** Generic monad types. *)

(** {2 Monad ops} *)

type ('a,'t) m

(* $(PIPE2SH("""sed -n '/type[ ].*monad_ops = /,/^}/p' >GEN.monad_ops.ml_ """)) *)
type 't monad_ops = {
  return : 'a. 'a -> ('a,'t) m;
  bind   : 'a 'b. ('a,'t) m -> ('a -> ('b,'t) m) -> ('b,'t) m;
}

(** The async operation completes with unit almost immediately; the
   argument may be a long-running computation; it is scheduled for
   execution.

    NOTE Because even creating an 'a m in lwt schedules computation,
   we shield the argument to async to avoid computation.  *)
type 't async = (unit -> (unit,'t) m) -> (unit,'t) m 


(** A type to allow parameterization over a generic notion of "part of
   state". Presumably the part of the state is locked while the
   monadic operation completes, and then the lock is released. *)
(* $(PIPE2SH("""sed -n '/type[ ].*with_state = /,/^}/p' >GEN.with_state.ml_ """)) *)
type ('s,'t) with_state = {
  with_state: 
    'a. 
      (* a function which has access to the state *)
      (state:'s -> 
       set_state:('s -> (unit,'t)m) -> 
       (* and returns a value of type 'a in the monad *)
       ('a,'t) m)
    -> ('a,'t)m
}
(** NOTE see also with_imperative_ref in Tjr_monad *)



(** {2 Events, with signal} *)

(** The type of events that resolve to ['a] *)
type 'a event

(** Multiple threads can be waiting on an event. Only one thread
   should signal the event. It is an error to signal the same event
   more than once. If this happens, the new signalled value is
   discarded and no further actions are taken. FIXME perhaps we should
   throw an exception or something. An event is a bit like a channel
   that multiple readers can register on (and all will receive any
   msgs), and which only carries at most one msg.


    NOTE lwt has two types, ['a t] for promises and ['a u] for
   resolvers. These are implemented using the same underlying
   structure.

*)
(* $(PIPE2SH("""sed -n '/type[ ].*event_ops = /,/^}/p' >GEN.event_ops.ml_ """)) *)
type 't event_ops = {
  ev_create: 'a. unit -> ('a event,'t) m;
  ev_wait: 'a. 'a event -> ('a,'t) m;
  ev_signal: 'a. 'a event -> 'a -> (unit,'t)m;
}



(** {2 Mutexes and condition variables} *)

(** What we need from mutexes and condition vars *)
(* $(PIPE2SH("""sed -n '/type[ ].*mutex_ops = /,/^}/p' >GEN.mutex_ops.ml_ """)) *)
type ('mutex,'cvar,'t) mutex_ops = {
  create_mutex : unit -> ('mutex,'t)m;
  create_cvar  : unit -> ('cvar,'t)m;
  lock         : 'mutex -> (unit,'t)m;
  signal       : 'cvar -> (unit,'t)m;  (* FIXME broadcast? *)
  unlock       : 'mutex -> (unit,'t)m;
  wait         : 'mutex -> 'cvar -> (unit,'t)m;
}



(** {2 Mutable ref cells} *)

module Ref : sig 
  type 'a ref_
  val int_to_ref : int -> int
  val ref_to_int : int -> int
end = struct
  type 'a ref_ = int
  let int_to_ref: int -> 'a ref_ = fun x -> x
  let ref_to_int: 'a ref_ -> int = fun x -> x 
end
open Ref

type ('l,'t) ref_cell_ops = {
  mkref: 'a. 'a -> ('a ref_,'t)m;
  deref: 'a. 'a ref_ -> ('a,'t)m;
  assign: 'a. 'a ref_ -> 'a -> (unit,'t)m;
}
