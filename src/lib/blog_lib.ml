open BatStd
open Utilities
open Data_mapping
open Options

let new_token data =
  let random_float = Random.float 42.0 |> Int64.of_float |> Int64.to_string in
  let now = Unix.gettimeofday () in
  let str_now = now |> Int64.of_float |> Int64.to_string in
  let value = Utilities.compute_md5 (random_float ^ str_now ^ data) in
  let t = { tok_value = value; tok_ctime = now } in
    Db.update_token t;
    t

let validate_token ?(delete = true) value =
  try
    let () = Db.select_token value |> ignore in
      if delete then Db.delete_token value;
      true
  with
    | Not_found -> false

