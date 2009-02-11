open Common
open Data_mapping

exception Error of string

let raise_wrap e = raise (Error (Sqlite3.Rc.to_string e))

let escape_sql s =
  if String.contains s '\000' then invalid_arg "escape_sql: string contains binary 0s"
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

let sql_string = escape_sql |- Printf.sprintf "'%s'"
let sql_int = string_of_int
let sql_bool = function true -> "1" | false -> "0"

let now () =
  let ts = Netdate.create ~zone:Netdate.localzone (Unix.time ()) in
    print_timestamp ts

let log f =
  let out = open_out_gen [Open_append; Open_creat; Open_binary] 0o644 log_filename in
    Printf.fprintf out "%s: " (now ());
    Printf.kfprintf (fun c -> Printf.fprintf c "\n%!"; close_out c) out f

let exec dbh ~cb query =
  log "EXEC QUERY: %s" query;
  let res = Sqlite3.exec dbh ~cb:cb query in
  let () = match res with
            | Sqlite3.Rc.OK -> ()
            | e -> log "     %s: %s" (Sqlite3.Rc.to_string e) (Sqlite3.errmsg dbh) in
    res

let db_handler = Sqlite3.db_open Common.database_file_name

let rec row_map ?(i=0) ?(map=SMap.empty) row headers =
  if i = Array.length row
  then map
  else
    let new_map = map <-- (headers.(i), row.(i)) in
      row_map ~i:(i+1) ~map:(new_map) row headers

let wrap (f : string option SMap.t -> unit) =
  fun row headers ->
    let map = row_map row headers in
      f map

let select_star ?(order_by) data_converter table =
  let order_clause = match order_by with
    | None -> "" | Some s -> "ORDER BY " ^ s in
  let query = Printf.sprintf "SELECT * FROM %s %s;" table order_clause in
  let current_acc = ref [] in
  let apply_f row = current_acc := (data_converter row)::(!current_acc) in
  let res = exec db_handler ~cb:(apply_f |> wrap) query in
    match res with
      | Sqlite3.Rc.OK -> List.rev !current_acc
      | e -> raise_wrap e

let delete table key value =
  let query = Printf.sprintf "DELETE FROM %s WHERE %s = %s;" table key value in
  let res = exec db_handler ~cb:(ignore |> wrap) query in
    match res with
      | Sqlite3.Rc.OK -> ()
      | e -> raise_wrap e

let users_list () = select_star user_of_row "users"
let posts_list () = select_star ~order_by:"post_id" post_of_row "posts"
let comments_list () = select_star comment_of_row "comments"

let delete_user email = delete "users" "emailx" (sql_string email)
let delete_post id = delete "posts" "post_id" (sql_int id)
let delete_comment id = delete "comments" "comment_id" (sql_int id)

