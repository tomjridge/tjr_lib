(** A simple config file functor.

By default, we use the value of the env var "TJR_CONFIGS_DIR" as the
   location of config files. If this is not set, we use "/tmp".  *)

open struct
  let configs_dir = lazy (
    Sys.getenv_opt "TJR_CONFIGS_DIR" |> function
    | None -> "/tmp"
    | Some s -> s)
    
  let configs_dir () = Lazy.force configs_dir  
end

let configs_dir = configs_dir

(** Given a config type, and a filename, bind the value config to the
   Yojson-deserialized contents of the file. *)
module Make(S:sig 
    type config [@@deriving yojson]
    val default_config: config option
    val filename: string
  end) = struct
  open S

  (** Read config from file, or use default_config if it exists *)
  let config = lazy begin
    let filename = configs_dir () ^ "/" ^ filename in
    Tjr_file.file_exists filename |> function
    | false -> (
        Printf.printf 
          "No config file %s exists (%s)\n" 
          filename __FILE__;
        match default_config with
        | None -> 
          failwith @@
          Printf.sprintf 
            "No config file %s exists, and no config default defined (%s)\n" 
            filename __FILE__
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
          failwith @@
          Printf.sprintf 
            "Config file %s exists, but could not be parsed\n%s" 
            filename __LOC__
      )
      
  end          
end

