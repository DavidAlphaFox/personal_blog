open BatPervasives;;

module CharMap = Map.Make(struct type t = char let compare = Pervasives.compare end);;

let (--) = BatChar.(--);;
let (++) = BatEnum.append;;
let alphabet = ('0' -- '9') ++  ('A' -- 'Z') ++ ('a' -- 'z') |> BatString.of_enum;;
let base = String.length alphabet;;

let idx_map = BatList.fold_right
                (uncurry CharMap.add)
                (BatEnum.combine ((BatString.enum alphabet), (0---(base - 1))) |>
                  BatList.of_enum)
                CharMap.empty
;;

let exp a b =
  let res = ref 1 in
  let i = ref 0 in
    while !i < b do
      res := !res * a;
      incr i
    done;
    !res
;;

let encode ?(padding = 0) n =
  if n < 0 then invalid_arg "Base62.encode";
  if padding > 16 then invalid_arg "Base62.encode";
  if padding < 0 then invalid_arg "Base62.encode";

  if n = 0 then "0" else begin
    let res = String.create 16 in
    let len = ref 0 in
    let n' = ref n in

    while !n' <> 0 do
      let i = !n' mod base in
        n' := !n' / base;
        res.[!len] <- alphabet.[i];
        incr len;
    done;

    let extra_zeros = ref (padding - !len) in
    while !extra_zeros > 0 do
      res.[!len] <- '0';
      incr len;
      decr extra_zeros;
    done;

    let i, j = ref 0, ref (!len - 1) in
    while !i < !j do
      let c = res.[!i] in
        res.[!i] <- res.[!j];
        res.[!j] <- c;
        incr i;
        decr j;
    done;

    String.sub res 0 !len
  end
;;

let decode s =
  let len = String.length s in
  let res = ref 0 in
  let i = ref 0 in
    try
      while !i < len do
        res := !res + (CharMap.find (s.[!i]) idx_map)*(exp base (len - 1 - !i));
        incr i
      done;
      !res
    with Not_found -> failwith "Base62.decode"
;;

assert (((encode ~padding:16 max_int |> decode) - max_int) = 0);;

