(* various utils to make writing shell scripts easier *)

(*

ocaml fs shell; main objects: string, string list; easy list operations (xs <- xs(\ not (endswith ".ml" it))  ; call outs to commands (bash ls -al x) |> (\. "$it.ml") ... this has automatic coercion of the function from s>s to ss>s via map
sh "cd /tmp"
xs=sh "ls /tmp" // issued from child bash process
with xs; // imperative operations follow, xs=ref xs; or maybe this is implicit; operations specialized to work with xs; or maybe there is some implicit argument? but we could easily specialize to a ss ref; so perhaps we have a functor that specializes and is then opened...then we don't even need special notation

remove " . .. "; 
filter (fun i -> i <> "." && i <> ".." && i <> "special.ml" );
filter (endswith ".ml")
map(fun i -> "$i.ml") // use ppx_string_interpolate
!xs.map print

and if we want to build functions:
let some_fun (xs:ss ref) = (
  let module X = mk_X(struct let xs=xs) in
  let open X in // or let open (mk_X(...))
  ....)


and we could wrap this up with osh, which calls readline, and then executes by using into the top level

The . notation is really working with a threaded state. (\ state. f ~state x y z)
*)

(*
#require "tjr_lib";;
#require "ppx_string_interpolate";;
*)

open Tjr_file
open Tjr_string

let cur = ref []

let set_cur xs = cur:=xs; ignore(!cur|>List.map print_endline); ()

let get_cur () = !cur

(* versions of common functions specialized to update cur *)

let filter f = get_cur()|>List.filter f|>set_cur

let map f = get_cur()|>List.map f|>set_cur

let readdir () = readdir () |> set_cur

(* TODO add a find equivalent; also set-like operations using string
   list; also save_cur, for caling functions... and save_cwd etc *)

(*

(* testing ---------------------------------------------------------- *)

filter (fun i -> i|>starts ".");;  (* FIXME don't have a label, so we can allow this... *)

let _ =
  cd "/tmp";
  readdir ();
  filter (fun i -> i<>"." && i<>"..");
  filter (fun i -> ends ~suffix:".ml" i);
  map (fun i -> print_endline i; i);
  map (fun i -> [%str "$(i).old"]);
;;  

let _ = (get_cur () |> List.map print_endline)

*)
