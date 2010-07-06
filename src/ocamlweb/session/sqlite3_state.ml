let (|>) x f = f x

module S3 = Sqlite3
module Pf = Printf

open BatMap.StringMap

let (<--) = Infix.(<--)
let (-->) = Infix.(-->)

let index_from s pos c =
  try Some (String.index_from s pos c)
  with Not_found -> None
;;

exception Session_storage_error of string

let escape_sql s =
  if String.contains s '\000' then
    raise (Session_storage_error "Invalid input string")
  else
    let start = ref 0 in
    let size = String.length s in
    let b = Buffer.create 1024 in
      try
        while true do
          let pos = String.index_from s !start '\'' in
            Buffer.add_string b (String.sub s !start (pos - !start));
            Buffer.add_string b "''";
            start := pos + 1;
        done;
        Buffer.contents b;
      with
        | Not_found -> begin
          Buffer.add_string b (String.sub s !start (size - !start));
          Buffer.contents b
        end
;;

let escape_sql_quote s = Pf.sprintf "'%s'" (escape_sql s);;

let string_of_float = Pf.sprintf "%0.17g"

type session_storage = S3.db

type sort_order =
  | No_order
  | By_ctime
  | By_ctime_desc
  | By_mtime
  | By_mtime_desc
  | By_expire
  | By_expire_desc

type session =
    {
      key : string;
      data : string;
      ctime : float;
      mtime : float;
      expire : float option;
      client_ip : string option;
      client_user_agent : string option;
    }

let raise_wrap e = raise (Session_storage_error (S3.Rc.to_string e));;

let (!!) opt = match opt with Some v -> v | None -> raise Not_found;;

let rec row_map ?(i=0) ?(map=empty) row headers =
  if i = Array.length row
  then map
  else
    let new_map = map <-- (headers.(i), row.(i)) in
      row_map ~i:(i+1) ~map:(new_map) row headers
;;

let wrap (f : string option t -> unit) =
  fun row headers ->
    let map = row_map row headers in
      f map
;;

let rec create_index res idx_names db =
  match idx_names with
    | [] -> res
    | hd::tl ->
        let query =
          Pf.sprintf "CREATE INDEX %s_idx ON sessions (%s ASC)" hd hd in
        let qres = S3.exec_not_null_no_headers db ~cb:ignore query in
          match qres with
            | S3.Rc.OK -> create_index res tl db
            | e -> S3.db_close db |> ignore; raise_wrap e
;;

let create_table db =
  let res = S3.exec_not_null_no_headers db ~cb:ignore
    "CREATE TABLE sessions
     (
        key               TEXT    PRIMARY KEY NOT NULL,
        data              TEXT                NOT NULL,
        ctime             REAL                NOT NULL,
        mtime             REAL                NOT NULL,
        expire            REAL                NULL,
        client_ip         TEXT                NULL,
        client_user_agent TEXT                NULL
     );" in
    match res with
      | S3.Rc.OK -> create_index db ["ctime"; "mtime"; "expire"] db
      | e -> S3.db_close db |> ignore; raise_wrap e
;;

let create filename =
  let db = S3.db_open filename in
    Sqlite3.busy_timeout db 10000;
    db
;;

let dispose state_mgr =
  if not (S3.db_close state_mgr)
  then raise (Session_storage_error "Sqlite3.close called with busy database: retry later")
;;

let state_of_row row =
  {
    key = !!(row-->"key");
    data = !!(row-->"data");
    ctime = !!(row-->"ctime") |> float_of_string;
    mtime = !!(row-->"mtime") |> float_of_string;
    expire = (match row-->"expire" with None -> None | Some s -> Some (float_of_string s));
    client_ip = row-->"client_ip";
    client_user_agent = row-->"client_user_agent";
  }
;;

let get state_mgr key =
  let value = ref None in
  let callback row = value := Some (state_of_row row) in

  let query = "SELECT key, data, ctime, mtime, expire, client_ip, client_user_agent
               FROM sessions WHERE key = '" ^ (escape_sql key) ^ "';" in
  let res = S3.exec state_mgr ~cb:(callback |> wrap) query in
    match res with
      | S3.Rc.OK -> !value
      | e -> raise_wrap e
;;

let del state_mgr key =
  let query =
    "DELETE FROM sessions WHERE key = '" ^ (escape_sql key) ^ "'" in
  let res = S3.exec_not_null_no_headers state_mgr ~cb:ignore query in
    match res with
      | S3.Rc.OK -> ()
      | e -> raise_wrap e
;;

let insert_query key data ctime mtime expire client_ip client_user_agent =
  let key = escape_sql_quote key in
  let data = escape_sql_quote data in
  let ctime = string_of_float ctime in
  let mtime = string_of_float mtime in
  let expire = match expire with
    | Some s -> string_of_float s
    | None -> "NULL" in
  let client_ip = match client_ip with
    | Some s -> escape_sql_quote s
    | None -> "NULL" in
  let client_user_agent = match client_user_agent with
    | Some s -> escape_sql_quote s
    | None -> "NULL" in

    Pf.sprintf "INSERT INTO sessions (key, data, ctime, mtime, expire,
                                      client_ip, client_user_agent)
                VALUES (%s, %s, %s, %s, %s, %s, %s);"
      key data ctime mtime expire client_ip client_user_agent
;;

let update_query key data ctime mtime expire client_ip client_user_agent =
  let ctime = string_of_float ctime in
  let mtime = string_of_float mtime in
  let expire = match expire with
    | None -> "NULL"
    | Some ts -> string_of_float ts in
  let client_ip = match client_ip with
    | None -> "NULL"
    | Some cl -> escape_sql_quote cl in
  let client_user_agent = match client_user_agent with
    | None -> "NULL"
    | Some cl -> escape_sql_quote cl in

    "UPDATE sessions SET
        data = " ^ (escape_sql_quote data) ^ ",
        ctime = " ^ ctime ^ ",
        mtime = " ^ mtime ^ ",
        expire = " ^ expire ^ ",
        client_ip = " ^ client_ip ^ ",
        client_user_agent = " ^ client_user_agent ^ "
     WHERE key = " ^ (escape_sql_quote key) ^ ";"
;;

let replace state_mgr client_ip client_user_agent ctime mtime expire key data =
  let q = update_query key data ctime mtime expire client_ip client_user_agent in
  let res = S3.exec_not_null_no_headers state_mgr ~cb:ignore q in
    match res with
      | S3.Rc.OK -> ()
      | e -> raise_wrap e
;;

let update state_mgr s =
  let q = insert_query s.key s.data s.ctime s.mtime s.expire s.client_ip s.client_user_agent in
  let res = S3.exec_not_null_no_headers state_mgr ~cb:ignore q in
    match res with
      | S3.Rc.OK -> ()
      | S3.Rc.CONSTRAINT ->
          replace state_mgr s.client_ip s.client_user_agent s.ctime s.mtime s.expire s.key s.data
      | e -> raise_wrap e
;;

let order_clause c =
  match c with
    | No_order -> ""
    | By_ctime -> " ORDER BY ctime ASC"
    | By_ctime_desc -> " ORDER BY ctime DESC"
    | By_mtime -> " ORDER BY mtime ASC"
    | By_mtime_desc -> " ORDER BY mtime DESC"
    | By_expire -> " ORDER BY expire"
    | By_expire_desc -> " ORDER BY expire DESC"
;;

let fold ?(order=No_order) f acc state_mgr =
  let current_acc = ref acc in
  let apply_f row = current_acc := f !current_acc (state_of_row row) in
  let res = S3.exec state_mgr ~cb:(apply_f |> wrap)
    ("SELECT * FROM sessions" ^ (order_clause order) ^ ";") in
    match res with
      | S3.Rc.OK -> !current_acc
      | e -> raise_wrap e
;;

let iter ?(order=No_order) f state_mgr =
  fold ~order:order (fun acc -> f) () state_mgr
;;

let dump ?(order=No_order) state_mgr =
  fold ~order:order (fun acc s -> s::acc) [] state_mgr |> List.rev
;;

