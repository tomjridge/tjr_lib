(* unix msging instance ----------------------------------------------- *)

open Net_unix

(* abbreviation *)
module Step_monad = Tjr_monad.Step_monad 


(* basic types and monad -------------------------------------------- *)

type unix_net_error = Unix_net_error of Unix.error

type state = {
  error: [ `Net_err of unix_net_error ] option
}

module Internal_monad = struct
  open Step_monad
  type 'a m = ('a,state) Step_monad.m
  let return = return
  let bind = bind
  type monad_ops = {
    return: 'a. 'a -> 'a m;
    bind: 'a 'b. 'a m -> ('a -> 'b m) -> 'b m
  }
  let monad_ops = { return;bind}
end
include Internal_monad


(* hidden ----------------------------------------------------------- *)

module Internal = struct
  (* now use the messaging library *)

  module Net_msg = Msg.Make(Internal_monad)

  include struct
    open Step_monad
    let err e = Step(fun s -> {error=Some e},fun () -> failwith __LOC__)
    let dest_exceptional s = s.error
    let rec catch f x = Step_monad.catch ~dest_exceptional f x
  end

  let extra = Net_msg.{
    err;
    catch;
  }

  (* now instantiate the unix net ops *)
  module Unix_net_ops = Make_unix_net_ops(Internal_monad)

  let net_ops = 
    let wrap_ops = Unix_net_ops.{
      wrap=(fun f -> Step_monad.(Step(fun s -> 
        try s,(f () |> fun x -> fun () -> Finished x) 
        with Unix.Unix_error(e,s1,s2) -> 
          {error=Some(`Net_err (Unix_net_error e))},
          fun () -> failwith __LOC__)))
    }
    in
    Unix_net_ops.mk_unix_net_ops ~monad_ops ~wrap_ops

  let dest_net_ops = Unix_net_ops.dest_net_ops

  let net_msg = Net_msg.mk_net_msg ~monad_ops ~extra ~net_ops ~dest_net_ops
  let _ = net_msg
end


(* export defns we need --------------------------------------------- *)

module Export : sig    
  (* type 'a m = 'a Internal_monad.m *)
  val send_string :
    conn:Unix.file_descr -> string_:string -> unit m
  val send_strings :
    conn:Unix.file_descr -> strings:string list -> unit m
  val recv_string : conn:Unix.file_descr -> string m
  val recv_strings : conn:Unix.file_descr -> string list m
end = struct
  open Internal
  type 'a m = 'a Internal_monad.m
  let (listen_accept,connect,send_string,send_strings,recv_string,recv_strings) = 
    net_msg @@ 
    fun ~listen_accept ~connect ~send_string ~send_strings ~recv_string ~recv_strings -> 
    (listen_accept,connect,send_string,send_strings,recv_string,recv_strings)

  let _ : conn:Unix.file_descr -> string_:string -> unit Net_msg.M_.m = send_string
end

include Export

(* FIXME it would actually be easier to use preprocessor and
   generate different versions :( *)

