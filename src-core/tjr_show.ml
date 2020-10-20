(** Convert expressions to strings for debugging and logging, via
   sexps; uses ppx_sexp_message

You need to add ppx_sexp_message to the pps section of the dune file

See https://github.com/janestreet/ppx_sexp_message for doc

 *)


module Test() = struct

  open Sexplib.Std

  let _ = Printf.printf "Tjr_show test...\n"
  
  let _ = 
    let arg2 = "and some more" in
    let x = 99 in
    Sexplib.Sexp.to_string_hum 
      [%message "This is a test"
        ~arg1:(Some 1 : int option) (* explicit tag *)
        (arg2 : string) (* implicit tag *)
        ~_:("and even more") (* no tag *)
        (x : int)  (* the var name/expression is included as a string for the tag *)
      ]
    |> print_endline

end
