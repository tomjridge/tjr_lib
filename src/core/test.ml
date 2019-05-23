(** Support for testing. Testing can be enabled/disabled. *)


(* testing ---------------------------------------------------------- *)

(** This is a reference to a "runner" function that takes a test
   function to run, and runs it. If tests are enabled, the runner
   simply executes the function. If tests are disabled, the runner
   discards the function. *)

let run_test : ((unit -> unit) -> unit) ref = 
  ref (fun f -> f ())
  |> Global.register ~name:(__MODULE__^".run_test ref")

let test f = (!run_test) f

(** Enable the test runner *)
let enable () = run_test := fun f -> f()

(** Disable the test runner *)
let disable () = run_test := fun _f -> ()
