open BatStd

let strip s =
  Str.replace_first (Str.regexp "^[ \t\n\r]+") "" s |>
    Str.replace_first (Str.regexp "[ \t\n\r]+$") ""

let (!!) opt = match opt with Some v -> v | None -> raise Not_found

let opt_def default = function Some v -> v | None -> default

let bool_of_int = function
  | 0 -> false
  | 1 -> true
  | _ -> invalid_arg "bool_of_int"

let bool_of_string = int_of_string |- bool_of_int

let string_of_float = Printf.sprintf "%0.17g"

let fatal ?(ret_code = (-1)) f x =
  try f x
  with exn ->
    let msg = Printexc.to_string exn in
      Printf.eprintf "FATAL EXCEPTION: %s\n%!" msg;
      exit ret_code

let print_timestamp ts =
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    ts.Netdate.year ts.Netdate.month ts.Netdate.day ts.Netdate.hour
    ts.Netdate.minute ts.Netdate.second

let generate_random_pwd len =
  Random.self_init ();
  let count = ref 0 in
  let rand_int = ref 0 in
  let pwd = String.create len in
  let isalpha = function 'A'..'Z' | 'a'..'z' -> true | _ -> false in
  let isdigit = function '0'..'9' -> true | _ -> false in
  let isalnum c = if (isalpha c) || (isdigit c) then true else false in
  let valid_punct = function '.' | ',' | ':' | ';' | '-' | '+' | '_' -> true
                      | _ -> false in
  let is_valid c = if (isalnum c) || (valid_punct c) then true else false in

    while (!count < len)
    do
      rand_int := Random.int 126;
      let c = Char.chr !rand_int in
        if is_valid c
        then
          begin
            pwd.[!count] <- Char.chr !rand_int;
            count := !count + 1;
          end
    done;
    pwd

let hex_encode = Cryptokit.transform_string (Cryptokit.Hexa.encode ())
let hex_decode = Cryptokit.transform_string (Cryptokit.Hexa.decode ())

let compute_digest kernel s =
  s |> Cryptokit.hash_string (kernel ()) |>
    Cryptokit.transform_string (Cryptokit.Hexa.encode ())

let compute_sha1 = compute_digest Cryptokit.Hash.sha1
let compute_md5  = compute_digest Cryptokit.Hash.md5

let rec loop p f x = if (p x) then x else loop p f (f x)

let range ?(start = 0) ?(step = 1) stop =
  loop (fun (n,acc) -> n >= stop) (fun (n,acc) -> (n+step,n::acc)) (start,[])
    |> snd |> List.rev

let year_of_iso_time iso_str =
  let epoch = Netdate.parse_epoch iso_str in
  let ts = Netdate.create epoch in
    ts.Netdate.year

