(** A simple config file functor *)

(** Given a config type, and a filename, bind the value config to the
   Yojson-deserialized contents of the file. *)
module Make(S:sig 
    type config [@@deriving yojson]
    val default_config: config option
    val filename: string
  end) = struct
  open S

  (** Read config from file, or use default_config if it exists *)
  let config = 
    try
      Yojson.Safe.from_file filename |> config_of_yojson |> function
      | Ok r -> r 
      | _ -> failwith ""
    with _ -> 
    match default_config with
    | None -> failwith __LOC__
    | Some c -> c
end

