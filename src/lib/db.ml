TYPE_CONV_PATH "Db"

open BatStd
open BatMap
open Utilities
open Data_mapping

exception Db_Error of string

(***************************************************************)
(* L O W   L E V E L   A N D   H E L P E R   F U N C T I O N S *)
(***************************************************************)

let logf = Netplex_cenv.logf
(* let logf level = Printf.printf *)

let raise_wrap e =
  let msg = Sqlite3.Rc.to_string e in
    logf `Err "EXCEPTION: Db.Err \"%s\"" msg;
    raise (Db_Error msg)
;;

let spf = Printf.sprintf

let database_file_name = lazy (Config.sqlite3_db_path ())

let global_dbh = ref None

let close_global_dbh () =
  match !global_dbh with
    | None -> ()
    | Some dbh ->
        Sqlite3.db_close dbh |> ignore;
        global_dbh := None;
;;

at_exit close_global_dbh;;

let rec row_map ?(i=0) ?(map=StringMap.empty) row headers =
  if i = Array.length row
  then map
  else
    let new_map = map <-- (headers.(i), row.(i)) in
      row_map ~i:(i+1) ~map:(new_map) row headers

let wrap (f : string option StringMap.t -> unit) =
  fun row headers ->
    let map = row_map row headers in
      f map

let exec ~cb query =
  let dbh = match !global_dbh with
    | None -> (
        let db = fatal Sqlite3.db_open (Lazy.force database_file_name) in
          Sqlite3.busy_timeout db 1000;
          logf `Debug "OPENING SQLite3 database %s" (Lazy.force database_file_name);
          global_dbh := Some db; db
      )
    | Some dbh -> dbh in

    logf `Debug "EXEC QUERY: %s" query;
    let res = Sqlite3.exec dbh ~cb:(cb |> wrap) query in
    let () = match res with
              | Sqlite3.Rc.OK -> ()
              | e -> logf `Err "     %s: %s"
                      (Sqlite3.Rc.to_string e) (Sqlite3.errmsg dbh) in
      res
;;

let insert_or_update table ?(where) map =
  let fields = StringMap.keys map |> BatList.of_enum |> String.concat ", " in
  let values = StringMap.values map |> BatList.of_enum |> String.concat ", " in
  let simple_insert () =
    let query = spf "INSERT INTO %s (%s) VALUES (%s)" table fields values in
    let res = exec ~cb:ignore query in
      match res with
        | Sqlite3.Rc.OK -> ()
        | e -> raise_wrap e in

    match where with
      | Some key_value_l -> begin
          let count = ref 0 in
          let where = List.map (fun (k,v) -> k ^ " = " ^ v) key_value_l |>
                        String.concat " AND " in
          let query = spf "SELECT count(*) AS n FROM %s WHERE %s" table where in
            exec ~cb:(fun row -> count := (!!(row-->"n") |> int_of_string)) query |> ignore;
            if !count = 0 then simple_insert ()
            else begin
              let set = StringMap.fold (fun k v a -> (k ^ " = " ^ v)::a) map [] |>
                          String.concat ", " in
              let query = spf "UPDATE %s SET %s WHERE %s" table set where in
              let res = exec ~cb:ignore query in
                match res with
                  | Sqlite3.Rc.OK -> ()
                  | e -> raise_wrap e
            end
          end
      | None -> simple_insert ()

let select_star ?(order_by) ?(where) data_converter table =
  let order_clause = match order_by with
    | None -> "" | Some s -> "ORDER BY " ^ s in
  let where_clause = match where with
    | None -> ""
    | Some key_value_l ->
        List.map (fun (k,v) -> k ^ " = " ^ v) key_value_l |>
          String.concat " AND " |> spf "WHERE %s" in
  let query = spf "SELECT * FROM %s %s %s" table where_clause order_clause in
  let current_acc = ref [] in
  let apply_f row = current_acc := (data_converter row)::(!current_acc) in
  let res = exec ~cb:apply_f query in
    match res with
      | Sqlite3.Rc.OK -> List.rev !current_acc
      | e -> raise_wrap e

let count_star ?(where) table =
  let count = ref 0 in
  let where_clause = match where with
    | None -> ""
    | Some key_value_l -> "WHERE " ^
        (List.map (fun (k,v) -> k ^ " = " ^ v) key_value_l |> String.concat " AND ") in

  let query = spf "SELECT count(*) AS n FROM %s %s" table where_clause in
    exec ~cb:(fun row -> count := (!!(row-->"n") |> int_of_string)) query |> ignore;
    !count

let delete table key_value_l =
  let where = List.map (fun (k,v) -> k ^ " = " ^ v) key_value_l |>
                String.concat " AND " in
  let query = spf "DELETE FROM %s WHERE %s" table where in
  let res = exec ~cb:ignore query in
    match res with
      | Sqlite3.Rc.OK -> ()
      | e -> raise_wrap e

(*******************************************)
(* H I G H   L E V E L   F U N C T I O N S *)
(*******************************************)

(* Functions to select a record *)
let select_user email =
  try
    select_star ~where:[("email", sql_string email)] user_of_row "users" |>
      List.hd
  with Failure "hd" -> begin
    logf `Err "EXCEPTION: Not_found";
    raise Not_found
  end

let select_post id =
  try
    select_star ~where:[("post_id", sql_int id)] post_of_row "posts" |>
      List.hd
  with Failure "hd" -> begin
    logf `Err "EXCEPTION: Not_found";
    raise Not_found
  end

let select_post_by_year_slug year slug=
  try
    select_star ~where:[("(strftime('%Y', ctime))", sql_string (string_of_int year));
                        ("url_title", sql_string slug)] post_of_row "posts" |> List.hd
  with Failure "hd" -> begin
    logf `Err "EXCEPTION: Not_found";
    raise Not_found
  end

let select_comment id =
  try
    select_star ~where:[("comment_id", sql_int id)] comment_of_row "comments" |>
      List.hd
  with Failure "hd" -> begin
    logf `Err "EXCEPTION: Not_found";
    raise Not_found
  end

let select_comments_by_post ?(approved = `True) post_id =
  let where = match approved with
    | `True -> [("approved", sql_bool true)]
    | `False -> [("approved", sql_bool false)]
    | `All -> [] in
  let where = ("post_id", sql_int post_id)::where in
  select_star ~order_by:"comment_time" ~where comment_of_row "comments"

let select_option ?(plugin = Blog) name =
  try
    select_star ~where:[("plugin", plugin |> string_of_plugin |> sql_string);
                        ("name", sql_string name)] option_of_row "options" |>
      List.hd
  with Failure "hd" -> begin
    logf `Err "EXCEPTION: Not_found";
    raise Not_found
  end

let select_token value =
  try
    List.hd (select_star ~where:[("value", sql_string value)] token_of_row "tokens")
  with Failure "hd" -> begin
    logf `Err "EXCEPTION: Not_found";
    raise Not_found
  end

let select_url_by_short short_url =
  try
    List.hd (select_star ~where:[("id_enc", sql_string short_url)] url_of_row "short_urls")
  with Failure "hd" -> begin
    logf `Err "EXCEPTION: Not_found";
    raise Not_found
  end

let select_url_by_long long_url =
  try
    List.hd (select_star ~where:[("long_url", sql_string long_url)] url_of_row "short_urls")
  with Failure "hd" -> begin
    logf `Err "EXCEPTION: Not_found";
    raise Not_found
  end

(* Functions to update a record *)
let update_user u =
  let m = map_of_user u in
    insert_or_update "users" ~where:[("email", m-->"email")] m

let update_post p =
  let m = map_of_post p in
  insert_or_update "posts" ~where:[("post_id", m-->"post_id")] m

let update_comment c =
  let m = map_of_comment c in
  insert_or_update "comments" ~where:[("comment_id", m-->"comment_id")] m

let update_option o =
  let m = map_of_option o in
  insert_or_update "options" ~where:[ ("plugin", m-->"plugin");
                                      ("name",   m-->"name") ] m
let update_token t =
  let m = map_of_token t in
  insert_or_update "tokens" m

let update_url u =
  let m = map_of_url u in
  insert_or_update "short_urls" ~where:["id", m-->"id"] m

(* Functions to selects all records *)
let users_list () = select_star user_of_row "users"
let posts_list () = select_star ~order_by:"post_id" post_of_row "posts"
let comments_list () = select_star comment_of_row "comments"
let options_list () = select_star ~order_by:"plugin" option_of_row "options"
let token_list () = select_star ~order_by:"ctime" token_of_row "tokens"
let urls_list () = select_star url_of_row "short_urls"

(* Functions to delete a record *)
let delete_user email = delete "users" [("email", (sql_string email))]
let delete_post id = delete "posts" [("post_id", (sql_int id))]
let delete_comment id = delete "comments" [("comment_id", (sql_int id))]
let delete_option plugin key = delete "options"
  [("plugin", string_of_plugin plugin |> sql_string); ("name", (sql_string key) )]
let delete_token value = delete "tokens" [("value"), sql_string value]
let delete_url short = delete "short_urls" [("id_enc"), sql_string short]

(* Get the next integer primary key from a table *)
let next_val table field =
  let query = spf "SELECT COALESCE(MAX(%s) + 1, 1) AS next_val FROM %s" field table in
  let next_val = ref 0 in
  let res = exec ~cb:(fun row -> next_val := (!!(row-->"next_val") |> int_of_string)) query in
    match res with
      | Sqlite3.Rc.OK -> !next_val
      | e -> raise_wrap e

(* Counting functions *)
let all_posts () = count_star "posts"
let draft_posts () = count_star ~where:[("status", "0")] "posts"
let published_posts () = count_star ~where:[("status", "1")] "posts"


(*************************************)
(* S P E C I A L   F U N C T I O N S *)
(*************************************)
let complete_post_query filter_clause limit offset =
  spf "
    SELECT *,
           (
              SELECT COUNT(*)
              FROM comments
              WHERE comments.post_id = inner_t.post_id
           ) AS comments_count
    FROM (SELECT * FROM posts JOIN users ON users.email = posts.author %s) AS inner_t
    ORDER BY mtime DESC
    LIMIT %d OFFSET %d" filter_clause limit offset

let complete_post post_id =
  let where = spf " AND post_id = %s" (sql_int post_id) in
  let query = complete_post_query where (-1) 0 in
  let current_acc = ref [] in
  let apply_f row = current_acc := row::(!current_acc) in
  let res = exec ~cb:apply_f query in
    match res with
      | Sqlite3.Rc.OK -> List.hd !current_acc
      | e -> raise_wrap e

let complete_post_list ?(filter_clause = "") () : string option StringMap.t list =
  let query = complete_post_query filter_clause (-1) 0 in
  let current_acc = ref [] in
  let apply_f row = current_acc := row::(!current_acc) in
  let res = exec ~cb:apply_f query in
    match res with
      | Sqlite3.Rc.OK -> List.rev !current_acc
      | e -> raise_wrap e

let complete_post_paged_list post_status offset limit : string option StringMap.t list =
  let filter_clause = match post_status with
    | `All -> ""
    | `Draft -> "WHERE status = 0"
    | `Published -> "WHERE status = 1" in
  let query = complete_post_query filter_clause limit offset in
  let current_acc = ref [] in
  let apply_f row = current_acc := row::(!current_acc) in
  let res = exec ~cb:apply_f query in
    match res with
      | Sqlite3.Rc.OK -> List.rev !current_acc
      | e -> raise_wrap e

let posts_by_tags tag_list offset limit : string option StringMap.t list =
  let tags = "(" ^ (BatString.join ", " (List.map string_of_int tag_list)) ^ ")" in
  let query = spf "
    SELECT DISTINCT(post_id) AS post_id, author, ctime, mtime, status, title,
                                url_title, content, summary, comments, pings,
                                private, pinged, email, passwd, first_name,
                                last_name, displ_name, visual_edit, role,
                                website, comments_count
    FROM
        (
            SELECT *,
                   (
                      SELECT COUNT(*)
                      FROM comments
                      WHERE comments.post_id = inner_t.post_id
                   ) AS comments_count
            FROM  (
                    SELECT *
                    FROM posts JOIN users           ON users.email = posts.author
                               JOIN posts_tags_rel  ON posts_tags_rel.post_id = posts.post_id
                    WHERE posts_tags_rel.tag_id IN %s
                            AND
                          posts.status = 1
                  ) AS inner_t
            LIMIT %d OFFSET %d
        )
    ORDER BY mtime DESC" tags limit offset in
  let current_acc = ref [] in
  let apply_f row = current_acc := row::(!current_acc) in
  let res = exec ~cb:apply_f query in
    match res with
      | Sqlite3.Rc.OK -> List.rev !current_acc
      | e -> raise_wrap e

let float_of_query column_name q =
  let current_acc = ref 0.0 in
  let apply_f row = current_acc := !!(row-->column_name) |> Netdate.parse_epoch in
  let res = exec ~cb:apply_f q in
    match res with
      | Sqlite3.Rc.OK -> !current_acc
      | e -> raise_wrap e

let where_of_post_ids ids =
  if ids = []
  then ""
  else
    let ids = "(" ^ (BatString.join ", " (List.map string_of_int ids)) ^ ")" in
      "WHERE post_id IN " ^ ids

let last_mtime_posts_comments ?(offset = 0) ?(limit = (-1)) ?(post_ids = []) () =
  let where = where_of_post_ids post_ids in
  let query = spf "
    SELECT MAX(MAX(mtime, COALESCE(comment_time, datetime(0, 'unixepoch')))) AS last_update
    FROM (SELECT * FROM posts
          %s -- WHERE
          ORDER BY mtime DESC
          LIMIT %d
          OFFSET %d) new_posts
    LEFT OUTER JOIN
    comments
    ON (new_posts.post_id = comments.post_id)" where limit offset in
    float_of_query "last_update" query

let last_mtime_posts ?(offset = 0) ?(limit = (-1)) ?(post_ids = []) () =
  let where = where_of_post_ids post_ids in
  let query = spf "
    SELECT MAX(mtime) AS last_update
    FROM posts %s
    ORDER BY mtime DESC
    LIMIT %d
    OFFSET %d" where limit offset in
    float_of_query "last_update" query

let last_mtime_post post_id =
  let query = spf "
    SELECT MAX(comment_time) AS last_update
    FROM comments
    WHERE post_id = " ^ (string_of_int post_id) in
    float_of_query "last_update" query

(*****************************************************)
(* R E A D I N G   T H E   T A G S   D A T A B A S E *)
(*****************************************************)
let build_tags_tree () : tag list =
  let get_all_tags () =
    let query = "SELECT * FROM tags_adj ORDER BY tag_parent" in
    let current_acc = ref [] in
    let apply_f row = current_acc := row::(!current_acc) in
    let res = exec ~cb:apply_f query in
      match res with
        | Sqlite3.Rc.OK -> List.rev !current_acc
        | e -> raise_wrap e in

  let get_raw_tags () =
    List.map
      (
        fun record ->
          {
            tag_id        = !!(record-->"tag_id") |> int_of_string;
            tag_text      = !!(record-->"tag_text");
            tag_slug      = !!(record-->"tag_slug");
            tag_parent_id = (match record-->"tag_parent" with None -> None | Some id -> Some (int_of_string id));
            tag_parent    = None; (* PLACEHOLDER *)
            tag_children  = [];   (* PLACEHOLDER *)
          }
      )
      (get_all_tags ()) in

  let rec aux (raw_tags : tag list)
              (parent : tag Lazy.t option)
              (parent_id : int option) =
    let children, rest =
      List.partition (fun t -> if t.tag_parent_id = parent_id then true else false) raw_tags in

    let children, rest =
      List.fold_left
      (
        fun (new_children, rest) tag ->
          let rec new_tag = lazy (
            let complete_children, rest = aux rest (Some new_tag) (Some tag.tag_id) in
              { tag with
                  tag_parent = parent;
                  tag_children = complete_children;
              }
          ) in
            (Lazy.force new_tag)::new_children, rest
      )
      ([], rest)
      children in
      List.rev children, rest
  in (* END of aux *)
    aux (get_raw_tags ()) None None |> fst

let rec flat_tags (tree : tag list) : tag list =
  List.fold_left (fun acc tag -> acc @ [tag] @ (flat_tags tag.tag_children)) [] tree

let tag_db : Data_mapping.tag_db Lazy.t =
  lazy (
    let tree = build_tags_tree () in
    let flat = flat_tags tree in
      tree, flat
  )

let tags_table : (int * string * string) list list Lazy.t = lazy (
  let rec build_path ?(path = []) tag =
    match tag.tag_parent with
      | None -> (tag.tag_id, tag.tag_text, tag.tag_slug)::path
      | Some p ->
          let p = Lazy.force p in
            build_path ~path:((tag.tag_id, tag.tag_text, tag.tag_slug)::path) p in
  let _, flat = Lazy.force tag_db in
  let leaves = List.filter (fun t -> if List.length t.tag_children = 0 then true else false) flat in
    List.map build_path leaves
  )

let post_tags post_id : tag list =
  let query = spf "SELECT tag_id FROM posts_tags_rel WHERE post_id = %d" post_id in
  let current_acc = ref [] in
  let tag_of_row row = !!(row-->"tag_id") |> int_of_string in
  let apply_f row = current_acc := (tag_of_row row)::(!current_acc) in
  let res = exec ~cb:apply_f query in
  let tag_id_list =
    match res with
      | Sqlite3.Rc.OK -> List.rev !current_acc
      | e -> raise_wrap e in
    List.filter
      (
        fun t ->
          List.fold_left
            (fun res id -> if t.tag_id = id then true else res)
            false
            tag_id_list
      )
      (tag_db |> Lazy.force |> snd)

let rec tag_slug_path ?(path = "") (tag : tag) : string =
  match tag.tag_parent with
    | None -> "/" ^ tag.tag_slug ^ path
    | Some p ->
        let p = Lazy.force p in
          tag_slug_path ~path:("/" ^ tag.tag_slug ^ path) p

let set_post_tags post_id (tags : int list) =
  delete "posts_tags_rel" [("post_id", (sql_int post_id))];
  List.iter
  (
    fun tag_id ->
      let m = StringMap.empty<--("post_id", sql_int post_id)
                        <--("tag_id", sql_int tag_id) in
      insert_or_update "posts_tags_rel" m
  )
  tags

let get_tags_count () =
  let query = "SELECT t.tag_id, tag_text, tag_slug, COUNT(*) AS tag_count
               FROM tags t JOIN posts_tags_rel r ON (t.tag_id = r.tag_id)
               GROUP BY t.tag_id ORDER BY t.tag_id" in
  let current_acc = ref [] in
  let tag_of_row row =
    let id = !!(row-->"tag_id") |> int_of_string in
    let text = !!(row-->"tag_text") in
    let slug  = !!(row-->"tag_slug") in
    let count = !!(row-->"tag_count") |> int_of_string in
      id, text, slug, count in
  let apply_f row = current_acc := (tag_of_row row)::(!current_acc) in
  let res = exec ~cb:apply_f query in
  let l =
    match res with
      | Sqlite3.Rc.OK -> !current_acc
      | e -> raise_wrap e in
  let tags = Lazy.force tag_db |> snd in
  let path_of_id id =
    List.find (fun t -> if t.tag_id = id then true else false) tags |>
      tag_slug_path in
    List.map (fun (i, t, s, c) -> (i, t, path_of_id i, c)) l

(*******************************************************)
(* R E A D I N G   T H E   P A G E S   D A T A B A S E *)
(*******************************************************)
let get_all_records () =
  let query = "SELECT * FROM pages_complete ORDER BY parent_page, position" in
  let current_acc = ref [] in
  let apply_f row = current_acc := row::(!current_acc) in
  let res = exec ~cb:apply_f query in
    match res with
      | Sqlite3.Rc.OK -> List.rev !current_acc
      | e -> raise_wrap e

let get_raw_data () =
  List.map
    (
      fun record ->
        let id = !!(record-->"page_id") |> int_of_string in
        let parent_id = (match record-->"parent_page" with None -> None | Some id -> Some (int_of_string id)) in
        let pos = !!(record-->"position") |> int_of_string in
        let data = {
            page_mtime = !!(record-->"mtime") |> Netdate.parse_epoch;
            page_title = record-->"title";
            page_url_title = !!(record-->"url_title");
            page_template = !!(record-->"template");
            page_password = record-->"password";
          } in
        Lazy_tree.new_node ~id ~parent_id ~pos ~data
    )
    (get_all_records ())

let page_db : Data_mapping.page_db Lazy.t =
  lazy (
    let tree = get_raw_data () |> Lazy_tree.build_tree in
    let flat = Lazy_tree.flat_tree tree in
      tree, flat
  )

(*******************************************************)
(*****    M A N A G I N G   S H O R T   U R L S    *****)
(*******************************************************)
let get_short_url long_url =
  try
    let u = select_url_by_long long_url in
    let new_u = { u with su_atime = Unix.gettimeofday (); } in
      update_url new_u; u.su_enc
  with Not_found -> begin
    let next = next_val "short_urls" "id" in
    let short = Base62.encode next in
    let new_u = {
      su_id = next;
      su_enc = short;
      su_url = long_url;
      su_atime = Unix.gettimeofday ();
    } in
      update_url new_u; short
  end

let get_long_url short_url =
  try
    let u = select_url_by_short short_url in
    let new_u = { u with su_atime = Unix.gettimeofday (); } in
      update_url new_u; Some u.su_url
  with Not_found -> None

