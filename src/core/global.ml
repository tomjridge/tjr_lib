(** A global state manager which Keeps track of eg config, bool vars etc *)

(** This flag, if true, forces names of global vars to be printed when
   registered *)
let debug_global = ref true

module Internal = struct

  let names = ref []

  let print_all_names () = 
    List.iter (fun name -> print_endline name) (!names)

end
open Internal

let register ~(name:string) a = 
  names:=name::!names;
  (!debug_global |> function
      | true -> Printf.printf "%s: registered value with name %s\n%!" __MODULE__ name
      | false -> ());
  a

