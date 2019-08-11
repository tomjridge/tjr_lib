(** Exhaustive state-space exploration for testing *)
open Util

module Internal_types = struct
  (** Test operations: step, check_state and check_step *)
  type ('op,'t) test_ops = {
    step: 't -> 'op -> 't list;
    check_state: 't -> unit;
    check_step: 't -> 'op -> 't -> unit;
  }

  (** Test state is a set of states we need to process (transition from)
      and a set of states we have already processed. *)
  type 'set test_state = { todo:'set; done_: 'set }

end
open Internal_types

module Internal(T:sig
    type op
    type t
    val test_ops: (op,t) test_ops
    val cmp: t -> t -> int
  end) = struct
  open T  (* so we don't have to qualify op and t *)


  (* open Base *)
  module C_arg = struct
    type t = T.t 
    open Base
    let compare=cmp 
    let sexp_of_t t = Sexp.Atom Caml.__FILE__ 
  end
  module C = Base.Comparator.Make(C_arg)

  let comparator=C.comparator

  module Set = struct
    include Base.Set.Using_comparator
    let card = length
    let choose = choose_exn
  end

  (** Exhaustive testing; from a set of initial states, and a set of ops
      to use to step states, generate all reachable states and check
      states and transitions. *)
  let {step; check_state; check_step} = test_ops

  (* count the number of steps, and also the cardinality of the todo
     and done sets *)
  let reps = ref 0

  let print_spinner ts = 
    match () with
    | _ when (!reps) mod 10000 = 0 ->
      Printf.printf " (todo: %d; done: %d)\n%!" (Set.card ts.todo) (Set.card ts.done_)
    | _ when (!reps) mod 1000 = 0  -> Printf.printf ".%!"
    | _ -> ()

  (* ops are the list of operations to use to step the state *)
  let step_ ops ts = 
    reps:=!reps+1;
    print_spinner ts;
    match Set.is_empty ts.todo with
    | true -> None
    | false -> 
      (* choose an elt *)
      let s = Set.choose ts.todo in
      (* remove from test state *)
      let ts = { 
        todo=Set.remove ts.todo s; 
        done_=Set.add ts.done_ s } 
      in
      (* process s by using ops to step s to get succ states s' *)
      let ts = ref ts in
      ops |> List.iter (fun op -> 
          test_ops.step s op |> fun _S' -> 
          _S' |> List.iter (fun s' -> 
              match Set.mem !ts.done_ s' with
              | true -> ()
               | false -> 
                check_state s';
                check_step s op s';
                let {todo;done_} = !ts in                  
                ts:={todo=Set.add todo s'; done_}));
      Some(!ts)

  let test ~(ops:op list) ~(init_states:t list) = 
    Printf.printf "%s: tests starting\n%!" __MODULE__;
    let ts = ref (
        Some {todo=Set.of_list ~comparator init_states; done_=Set.empty ~comparator}) 
    in
    while !ts <> None do
      ts:=step_ ops (!ts|>dest_Some) 
    done
end

module Internal2 = struct
  let test (type op' t') ~cmp ~test_ops = 
    let module I = Internal(struct 
        type op=op' type t=t' let cmp=cmp let test_ops = test_ops end)
    in
    I.test 
end
open Internal2

let test ~cmp ~(step:'t -> 'op -> 't list) ~check_state ~check_step =
  test ~cmp ~test_ops:{step;check_state;check_step}
