(** A simple config file functor *)

module Make(S:sig 
    type config [@@deriving yojson]
    val filename: string
end) = struct
  open S

  (** Read config from file *)
  let config = 
    Yojson.Safe.from_file filename |> config_of_yojson |> function
    | Ok r -> r | _ -> failwith __LOC__
end
  
