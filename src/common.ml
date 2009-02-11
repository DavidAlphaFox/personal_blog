let (|>) x f = f x
let ( |- ) f g x = g (f x)

let (!!) opt = match opt with Some v -> v | None -> raise Not_found;;

module SMap = Map.Make(struct type t = string let compare = String.compare end)
let (-->) map key = SMap.find key map
let (<--) map (key, value) = SMap.add key value map

let bool_of_int = function
  | 0 -> false
  | 1 -> true
  | _ -> invalid_arg "bool_of_int"

let bool_of_string = int_of_string |- bool_of_int

let string_of_float = Printf.sprintf "%0.17g"

let print_timestamp ts =
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    ts.Netdate.year ts.Netdate.month ts.Netdate.day ts.Netdate.hour
    ts.Netdate.minute ts.Netdate.second

(* CONFIGURATIONS *)
let database_file_name = "blog.db"
let log_filename = "blog.log"

