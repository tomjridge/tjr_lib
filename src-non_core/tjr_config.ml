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
    Tjr_file.file_exists filename |> function
    | false -> (
        match default_config with
        | None -> 
          Printf.sprintf "No config file %s exists, and no config default defined\n%s" filename __LOC__
          |> failwith
        | Some c -> c)
    | true -> (
        try 
          Yojson.Safe.from_file filename |> config_of_yojson |> function
          | Ok r -> r 
          | _ -> failwith __LOC__
        with _ -> 
          (match default_config with 
           | None -> ()
           | Some c -> (
               c |> config_to_yojson |> Yojson.Safe.pretty_to_string |> fun c ->
               Printf.printf "Config file %s should look like:\n%s\n" filename c));
          Printf.sprintf "Config file %s exists, but could not be parsed\n%s" filename __LOC__
          |> failwith
      )

          
end

