(** Exhaustive state-space exploration for testing *)


open Tjr_set

let dest_Some = function (Some x) -> x | _ -> failwith "dest_Some"


type ('op,'t) test_ops = {
  step: 't -> 'op -> 't list;
  check_state: 't -> unit;
  check_step: 't -> 'op -> 't -> unit
}



type 'set test_state = { todo:'set; done_: 'set }


let test ~set_ops ~test_ops =
  let reps = ref 0 in

  let card = set_ops.cardinal in

  let step ops ts = 
    reps:=!reps+1;
    begin  (* visual feedback that something is happening *)
      match () with
      | _ when (!reps) mod 10000 = 0 ->
        Printf.printf " (todo: %d; done: %d)\n%!" (card ts.todo) (card ts.done_)
      | _ when (!reps) mod 1000 = 0  -> Printf.printf ".%!"
      | _ -> ()
    end;
    match set_ops.is_empty ts.todo with
    | true -> None
    | false -> 
      let s = set_ops.choose ts.todo in
      let ts = { todo=set_ops.remove s ts.todo; done_=set_ops.add s ts.done_ } in
      let ts =
        Tjr_list.with_each_elt
          ~list:ops
          ~init:ts
          ~step:(fun ~state op ->
            test_ops.step s op |> fun next_states ->
            begin
              List.iter (fun s' -> test_ops.check_state s'; test_ops.check_step s op s') next_states;
              Tjr_list.with_each_elt
                ~list:next_states
                ~step:(fun ~state:ts s' ->
                  match set_ops.mem s' ts.done_ with 
                  | true -> ts
                  | false -> set_ops.add s' ts.todo |> fun todo -> { ts with todo })
                ~init:ts
            end)
      in
      Some ts
  in

  let test ~ops ~init_states = 
    Printf.printf "%s: tests starting\n%!" __LOC__;
    let ts = ref (Some {todo=set_ops.of_list init_states; done_=set_ops.empty()}) in
    while !ts <> None do
      ts:=step ops (!ts|>dest_Some) 
    done
  in

  test


(* keep applying the step function; assume that eventually this
   returns the empty list of next states, so that testing halts *)
let test_till_no_successor_states ~test_ops =
  let reps = ref 0 in

  let step ops todo = 
    reps:=!reps+1;
    begin  (* visual feedback that something is happening *)
      match () with
      | _ when (!reps) mod 10000 = 0 ->
        Printf.printf " (todo: %d)\n%!" (List.length todo) 
      | _ when (!reps) mod 1000 = 0  -> Printf.printf ".%!"
      | _ -> ()
    end;
    match todo=[] with
    | true -> None
    | false -> 
      let s = List.hd todo in
      let todo = List.tl todo in
      let todo =
        Tjr_list.with_each_elt
          ~list:ops
          ~init:todo
          ~step:(fun ~state op ->
            test_ops.step s op |> fun next_states ->
            begin
              List.iter (fun s' -> test_ops.check_state s'; test_ops.check_step s op s') next_states;
              Tjr_list.with_each_elt
                ~list:next_states
                ~step:(fun ~state:todo s' ->
                  s'::todo)
                ~init:todo
            end)
      in
      Some todo
  in

  let test ~ops ~init_states = 
    Printf.printf "%s: tests starting\n%!" __LOC__;
    let todo = ref (Some init_states) in 
    while !todo <> None do
      todo:=step ops (!todo|>dest_Some) 
    done
  in

  test
