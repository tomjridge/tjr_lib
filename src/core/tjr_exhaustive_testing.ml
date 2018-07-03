(** Exhaustive state-space exploration for testing *)


open Tjr_set

let dest_Some = function (Some x) -> x | _ -> failwith "dest_Some"


type ('op,'t) test_ops = {
  step: 't -> 'op -> 't list;
  check_state: 't -> unit;
  check_step: 't -> 't -> unit
}



type 'set test_state = { todo:'set; done_: 'set }


let test ~set_ops ~test_ops =
  Printf.printf "%s: tests starting\n%!" __LOC__;
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
    let s = set_ops.choose ts.todo in
    let ts = { todo=set_ops.remove s ts.todo; done_=set_ops.add s ts.done_ } in
    let ns = ops |> List.map (fun op -> test_ops.step s op) |> List.concat in
    let ns = ns |> set_ops.of_list in
    (* FIXME following isn't quite right because we may need to check the transition even if next state has been seen *)
    (* check next states *)
    ns |> set_ops.iter test_ops.check_state;
    (* check steps *)
    let ns = set_ops.diff ns ts.done_ in
    ns |> set_ops.iter (fun s' -> test_ops.check_step s s');
    match set_ops.is_empty ns with 
    | true -> None
    | false -> Some { ts with todo=set_ops.union ns ts.todo }
  in

  let test ~ops ~init_states = 
    let ts = ref (Some {todo=set_ops.of_list init_states; done_=set_ops.empty()}) in
    while !ts <> None do
      ts:=step ops (!ts|>dest_Some) 
    done
  in

  test
