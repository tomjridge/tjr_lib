(** Simple testing/ trace checking. *)

(* 

We have:

- a type of spec states 's
- a type 'u of transitions of the implementation; typically this is
  't * 'lbl * 't, where 't is the state of the implementation

We need to provide a function: 's * 'u -> 's option

The interpretation is that this returns the next spec state (if the
transition is correct), or none otherwise.

We also need the initial spec state of course.

Then we can either check a sequence/list of transitions, or we can
focus on a single transition at a time, and maintain the spec state as
we go along.


*)



type 's spec_state = 's

type 'u impl_transition = 'u

type ('s,'u) check_ops = {
  initial_spec_state: 's;
  check_trans: 's * 'u -> 's option
}


let check_transition_sequence check_ops xs = 
  Tjr_list.fold_left_
    ~step:(fun ~state trans -> 
        match state with
        | None -> None
        | Some state -> check_ops.check_trans (state,trans))
    ~init_state:(Some check_ops.initial_spec_state)
    xs
  
