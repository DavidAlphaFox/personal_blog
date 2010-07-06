open BatStd
open BatSet
open BatMap

exception Param_not_found of string
exception Type_error of string*string*string
exception Convert_fail of string

type 'a inner_type_desc =
  | Unit
  | Int of string
  | Int32 of string
  | Int64 of string
  | Float of string
  | String of string
  | Bool of string
  | UserType of string * (string -> 'a)
  | Tuple of 'a inner_type_desc * 'a inner_type_desc
  | Tuple3 of 'a inner_type_desc * 'a inner_type_desc * 'a inner_type_desc
  | Tuple4 of 'a inner_type_desc * 'a inner_type_desc * 'a inner_type_desc * 'a inner_type_desc
  | Tuple5 of 'a inner_type_desc * 'a inner_type_desc * 'a inner_type_desc * 'a inner_type_desc * 'a inner_type_desc
  | Opt of 'a inner_type_desc
  | List of 'a inner_type_desc

type ('a, +'lst) type_desc = 'a inner_type_desc

let unit = Unit

let int n = Int n

let int32 n = Int32 n

let int64 n = Int64 n

let float n = Float n

let string n = String n

let bool n = Bool n

let user_type name of_string =
  UserType (name, of_string)

let int_of_string i =
  try int_of_string i
  with Failure "int_of_string" -> raise (Convert_fail "int")

let int32_of_string i =
  try Int32.of_string i
  with Failure "int_of_string" -> raise (Convert_fail "int32")

let int64_of_string i =
  try Int64.of_string i
  with Failure "int_of_string" -> raise (Convert_fail "int64")

let float_of_string i =
  try float_of_string i
  with Failure "float_of_string" -> raise (Convert_fail "float")

let convert_fail type_name = raise (Convert_fail type_name)

let tpl2 v1 v2 =
  Obj.magic (Tuple ((Obj.magic v1), (Obj.magic v2)))

let ( ** ) = tpl2

let tpl3 v1 v2 v3 =
  Obj.magic (Tuple3 ((Obj.magic v1), (Obj.magic v2), (Obj.magic v3)))

let tpl4 v1 v2 v3 v4 =
  Obj.magic (Tuple4 ((Obj.magic v1), (Obj.magic v2), (Obj.magic v3), (Obj.magic v4)))

let tpl5 v1 v2 v3 v4 v5 =
  Obj.magic (Tuple5 ((Obj.magic v1), (Obj.magic v2), (Obj.magic v3), (Obj.magic v4), (Obj.magic v5)))

let opt v = Obj.magic (Opt v)

let lst v = Obj.magic (List v)

let convert_or_raise name value_str converter =
  try converter value_str
  with Convert_fail type_name ->
    raise (Type_error (name, value_str, type_name))

let rec reconstruct_params_aux  (map : string StringMap.t)
                                ?(idx = None)
                                type_desc =
  let find_val n = try `Ok (StringMap.find n map) with Not_found -> `Not_found in
  let suffix = match idx with Some idx -> Printf.sprintf "-%d" idx | None -> "" in

  let reconstruct_term t =
    let reconstruct_term_aux n converter =
      let name = n ^ suffix in
        match find_val name with
          | `Ok v -> Obj.magic (convert_or_raise name v converter)
          | `Not_found -> raise (Param_not_found name) in (* reconstruct_term_aux *)

    match t with
      | Unit -> Obj.magic ()
      | Int n -> reconstruct_term_aux n int_of_string
      | Int32 n -> reconstruct_term_aux n int32_of_string
      | Int64 n -> reconstruct_term_aux n int64_of_string
      | Float n -> reconstruct_term_aux n float_of_string
      | String n -> reconstruct_term_aux n (fun x -> x)
      | Bool n -> (
          let name = n ^ suffix in
            match find_val name with
              | `Ok _ -> Obj.magic true
              | `Not_found -> Obj.magic false
        )
      | UserType (n, of_string) -> reconstruct_term_aux n of_string
      | Tuple _ | Tuple3 _ | Tuple4 _ | Tuple5 _
      | Opt _ | List _ -> failwith "Not a terminal type" in (* reconstruct_term *)

  let rec get_all_prefixes ?(set = StringSet.empty) = function
    | Unit -> set
    | Int p | Int32 p | Int64 p | Float p | String p | Bool p -> StringSet.add p set
    | UserType (p, _) -> StringSet.add p set
    | Tuple (t1, t2) -> let set = get_all_prefixes ~set t1 in
                          get_all_prefixes ~set t2
    | Tuple3 (t1, t2, t3) ->
        let set = get_all_prefixes ~set t1 in
        let set = get_all_prefixes ~set t2 in
          get_all_prefixes ~set t3
    | Tuple4 (t1, t2, t3, t4) ->
        let set = get_all_prefixes ~set t1 in
        let set = get_all_prefixes ~set t2 in
        let set = get_all_prefixes ~set t3 in
          get_all_prefixes ~set t4
    | Tuple5 (t1, t2, t3, t4, t5) ->
        let set = get_all_prefixes ~set t1 in
        let set = get_all_prefixes ~set t2 in
        let set = get_all_prefixes ~set t3 in
        let set = get_all_prefixes ~set t4 in
          get_all_prefixes ~set t5
    | Opt t -> get_all_prefixes ~set t
    (* This case is impossible because list of list doesn't even compile! *)
    | List _ -> failwith "Nested lists are not supported"
  in (* get_all_prefixes *)

  let get_all_indexes (prefix_map : StringSet.t) : IntSet.t =
    let pattern_kernnel = String.concat "\\|" (StringSet.elements prefix_map) in
    let pattern = Str.regexp ("^\\(" ^ pattern_kernnel ^ "\\)-\\([0-9]+\\)") in
      StringMap.fold
        (
          fun par_name par_value idxs ->
            if Str.string_match pattern par_name 0 then
              let found_int = Str.matched_group 2 par_name |> int_of_string in
                IntSet.add found_int idxs
            else idxs
        )
        map
        IntSet.empty
  in (* get_all_indexes *)

  let rec reconstruct_list t : 'a list =
    let idxs = t |> get_all_prefixes |> get_all_indexes in
      IntSet.fold (fun idx l -> (reconstruct_params_aux map ~idx:(Some idx) t)::l)
                  idxs [] |> List.rev
  in (* reconstruct_list *)

    match type_desc with
      | Unit | Int _ | Int32 _ | Int64 _ | Float _
      | String _ | Bool _ | UserType _ -> reconstruct_term type_desc
      | Tuple (t1, t2) -> (
          let v1 = reconstruct_params_aux map ~idx t1 in
          let v2 = reconstruct_params_aux map ~idx t2 in
            Obj.magic (Obj.magic v1, Obj.magic v2)
        )
      | Tuple3 (t1, t2, t3) -> (
          let v1 = reconstruct_params_aux map ~idx t1 in
          let v2 = reconstruct_params_aux map ~idx t2 in
          let v3 = reconstruct_params_aux map ~idx t3 in
            Obj.magic (Obj.magic v1, Obj.magic v2, Obj.magic v3)
        )
      | Tuple4 (t1, t2, t3, t4) -> (
          let v1 = reconstruct_params_aux map ~idx t1 in
          let v2 = reconstruct_params_aux map ~idx t2 in
          let v3 = reconstruct_params_aux map ~idx t3 in
          let v4 = reconstruct_params_aux map ~idx t4 in
            Obj.magic (Obj.magic v1, Obj.magic v2, Obj.magic v3, Obj.magic v4)
        )
      | Tuple5 (t1, t2, t3, t4, t5) -> (
          let v1 = reconstruct_params_aux map ~idx t1 in
          let v2 = reconstruct_params_aux map ~idx t2 in
          let v3 = reconstruct_params_aux map ~idx t3 in
          let v4 = reconstruct_params_aux map ~idx t4 in
          let v5 = reconstruct_params_aux map ~idx t5 in
            Obj.magic (Obj.magic v1, Obj.magic v2, Obj.magic v3, Obj.magic v4, Obj.magic v5)
        )
      | Opt t -> (
          try Obj.magic (Some (reconstruct_params_aux map ~idx t))
          with Param_not_found _ -> Obj.magic None
        )
      | List t -> Obj.magic (reconstruct_list t)
(* reconstruct_params_aux *)


let reconstruct_params  (map : string StringMap.t)
                        type_desc =
  reconstruct_params_aux map type_desc

let collect_params (cgi : Netcgi.cgi_activation) =
  List.fold_left
    (
      fun (param_map, file_lst) arg ->
        match arg#store with
          | `Memory -> StringMap.add arg#name arg#value param_map, file_lst
          | `File _ -> param_map, arg::file_lst
    )
    (StringMap.empty, [])
    cgi#arguments

