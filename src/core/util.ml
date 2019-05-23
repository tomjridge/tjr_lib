let dest_Some = function (Some x) -> x | _ -> failwith "dest_Some"

let dest_Ok = function (Ok x) -> x | _ -> failwith "dest_Ok"
