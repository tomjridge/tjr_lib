(** Simple record of the "runlevel" we have reached. *)

type runlvl = {runlvl:int}

open struct
  let to_int {runlvl} = runlvl
end

(** Some run levels *)

(* l0 is compile time *)
let l1_module_init = {runlvl=1}

(** Presumably post-module-init is just prior to starting to execute main *)
(* let l2_post_module_init = {runlvl=2} *)

(** this is also the point at which main starts running *)
let l6_main_init = {runlvl=6}

(** the point after main init, but before main proper *)
let l7_post_main_init = {runlvl=7}


(** Runlevel mutable ref *)

let runlvl = ref l1_module_init


(** Various functions *)

let set r = runlvl:=r

let note_main_starts () = runlvl:=l6_main_init

let note_post_main_init () = runlvl:=l7_post_main_init

let is_running_main () = to_int !runlvl >= to_int l6_main_init

let can_read_config_file () = !runlvl = l6_main_init


(* FIXME todo: ensure that any config file that is forced, is forced
   after main starts running; so add Runlevel.note_main_starts to
   beginning of relevant mains *)
