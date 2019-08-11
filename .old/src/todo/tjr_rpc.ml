(* very minimal rpc using json -------------------------------------- *)


module V1 = struct

  (* in order to send a function call to the server, we need the
     function name and the args *)

  type json = Yojson.Safe.json

  type 'a iso = { in_:'a -> json; out: json -> 'a }

  type 'a call' = {
    name: string;
    f_: ('a -> unit);
    iso: 'a iso (* call and forget *)
  }

  type empty

  (* the ' versions are where we don't wait for the reply *)

  let mk_server' kk  = 
    let r : (string * empty call') list ref = ref [] in
    let register (c:'a call') = 
      let c = ((Obj.magic c) : empty call') in
      r:=(c.name,c)::!r
    in
    let find name = List.assoc name !r in
    let execute (j:json) = 
      (* find function and execute with args *)
      match j with
      | `List [`String name; args] -> 
        find name |> fun c -> 
        args |> c.iso.out |> c.f_
      | _ -> failwith __LOC__ (* should be impossible if we call correctly *)
    in
    kk ~register ~execute

  let _ = mk_server'

  (* call from client; kk is the thing that sends a json msg to server *)
  let execute_on_server' ~kk ~call ~args = 
    (* if we have the call' in hand, we have everything we need to
       marshall the call *)
    let args = call.iso.in_ args in
    let json : json = `List [`String call.name; args] in
    kk json

  let _ = execute_on_server'

end  
    
include V1




    
module Example = functor (_:sig end) -> struct

  (* FIXME *)

end




