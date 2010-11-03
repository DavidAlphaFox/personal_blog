TYPE_CONV_PATH "Data_mapping"

open BatStd
open BatMap
open Utilities
open Lazy_tree

let (-->) = StringMap.Infix.(-->)
let (<--) = StringMap.Infix.(<--)

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
let sql_string_option = function None -> "NULL" | Some s -> sql_string s
let sql_int_option = function None -> "NULL" | Some i -> sql_int i

type user_role =
  | Admin
  | Editor
  | Author
  | Contributor
  | Subscriber
      with sexp

type user =
  {
    email : string;
    passwd : string;
    first_name : string option;
    last_name : string option;
    displ_name : string option;
    visual_edit : bool;
    role : user_role;
    website : string option
  } with sexp

let role_of_string = function
  | "admin" -> Admin
  | "editor" -> Editor
  | "author" -> Author
  | "contributor" -> Contributor
  | "subscriber" -> Subscriber
  | _ -> invalid_arg "role_of_string"

let string_of_role = function
  | Admin -> "admin"
  | Editor -> "editor"
  | Author -> "author"
  | Contributor -> "contributor"
  | Subscriber -> "subscriber"

let user_of_row row =
  {
    email = !!(row-->"email");
    passwd = !!(row-->"passwd");
    first_name = row-->"first_name";
    last_name = row-->"last_name";
    displ_name = row-->"displ_name";
    visual_edit = !!(row-->"visual_edit") |> bool_of_string;
    role = !!(row-->"role") |> role_of_string;
    website = row-->"website";
  }

let map_of_user u =
  StringMap.empty<--("email", sql_string u.email)
                 <--("passwd", sql_string u.passwd)
                 <--("first_name", sql_string_option u.first_name)
                 <--("last_name", sql_string_option u.last_name)
                 <--("displ_name", sql_string_option u.displ_name)
                 <--("visual_edit", sql_bool u.visual_edit)
                 <--("role", u.role |> string_of_role |> sql_string)
                 <--("website", sql_string_option u.website)

let string_of_user = sexp_of_user |- Sexplib.Sexp.to_string_hum |- hex_encode
let print_user = string_of_user |- hex_decode |- Printf.printf "%s\n"
let user_of_string = hex_decode |- Sexplib.Sexp.of_string |- user_of_sexp

type post =
  {
    id : int;
    author : string;
    ctime : float;
    mtime : float;
    status : bool;
    title : string option;
    url_title : string option;
    post_content : string option;
    summary : string option;
    comments : bool;
    pings : bool;
    is_private : bool;
    pinged : string option
  } with sexp

let empty_post =
  {
    id = -1;
    author = "";
    ctime = Unix.gettimeofday ();
    mtime = Unix.gettimeofday ();
    status = false;
    title = Some "";
    url_title = Some "";
    post_content = Some "";
    summary = Some "";
    comments = true;
    pings = true;
    is_private = false;
    pinged = None
  }

let string_of_post = sexp_of_post |- Sexplib.Sexp.to_string_hum |- hex_encode
let print_post = string_of_post |- hex_decode |- Printf.printf "%s\n"
let post_of_string = hex_decode |- Sexplib.Sexp.of_string |- post_of_sexp

let post_of_row row =
  {
    id = !!(row-->"post_id") |> int_of_string;
    author = !!(row-->"author");
    ctime = !!(row-->"ctime") |> Netdate.parse_epoch;
    mtime = !!(row-->"mtime") |> Netdate.parse_epoch;
    status = !!(row-->"status") |> bool_of_string;
    title = row-->"title";
    url_title = row-->"url_title";
    post_content = row-->"content";
    summary = row-->"summary";
    comments = !!(row-->"comments") |> bool_of_string;
    pings = !!(row-->"pings") |> bool_of_string;
    is_private = !!(row-->"private") |> bool_of_string;
    pinged = row-->"pinged";
  }

let map_of_post p =
  StringMap.empty<--("post_id", sql_int p.id)
                 <--("author", sql_string p.author)
                 <--("ctime", p.ctime |> Netdate.create |> print_timestamp |> sql_string)
                 <--("mtime", p.mtime |> Netdate.create |> print_timestamp |> sql_string)
                 <--("status", sql_bool p.status)
                 <--("title", sql_string_option p.title)
                 <--("url_title", sql_string_option p.url_title)
                 <--("content", sql_string_option p.post_content)
                 <--("summary", sql_string_option p.summary)
                 <--("comments", sql_bool p.comments)
                 <--("pings", sql_bool p.pings)
                 <--("private", sql_bool p.is_private)
                 <--("pinged", sql_string_option p.pinged)

type page =
  {
    page_mtime : float;
    page_title : string option;
    page_url_title : string;
    page_template : string;
    page_password : string option;
  }

type page_db = (page, [`complete]) tree * (page, [`complete]) tree_node list
               (* the list of roots, actually a tree; the flat list of all pages *)

type inet_addr = Unix.inet_addr
type str = string with sexp;;
let sexp_of_inet_addr = Unix.string_of_inet_addr |- sexp_of_str
let inet_addr_of_sexp = str_of_sexp |- Unix.inet_addr_of_string

type comment =
  {
    comment_id : int;
    post_id : int;
    author_name : string;
    author_email : string;
    author_url : string option;
    author_ip : inet_addr;
    author_ua : string;
    comment_time : float;
    content : string;
    approved : bool;
    spam : bool;
  } with sexp

let comment_of_row row =
  {
    comment_id = !!(row-->"comment_id") |> int_of_string;
    post_id = !!(row-->"post_id") |> int_of_string;
    author_name = !!(row-->"author_name");
    author_email = !!(row-->"author_email");
    author_url = row-->"author_url";
    author_ip = !!(row-->"author_ip") |> Unix.inet_addr_of_string;
    author_ua = !!(row-->"author_ua");
    comment_time = !!(row-->"comment_time") |> Netdate.parse_epoch;
    content = !!(row-->"content");
    approved = !!(row-->"approved") |> bool_of_string;
    spam = !!(row-->"spam") |> bool_of_string;
  }

let map_of_comment c =
  StringMap.empty<--("comment_id", sql_int c.comment_id)
                 <--("post_id", sql_int c.post_id)
                 <--("author_name", sql_string c.author_name)
                 <--("author_email", sql_string c.author_email)
                 <--("author_url", sql_string_option c.author_url)
                 <--("author_ip", c.author_ip |> Unix.string_of_inet_addr |> sql_string)
                 <--("author_ua", sql_string c.author_ua)
                 <--("comment_time", c.comment_time |> Netdate.create |> print_timestamp |> sql_string)
                 <--("content", sql_string c.content)
                 <--("approved", sql_bool c.approved)
                 <--("spam", sql_bool c.spam)

let string_of_comment = sexp_of_comment |- Sexplib.Sexp.to_string_hum |- hex_encode
let print_comment = string_of_comment |- hex_decode |- Printf.printf "%s\n"
let comment_of_string = hex_decode |- Sexplib.Sexp.of_string |- comment_of_sexp

type plugin =
  | Blog
  | Plugin of string
    with sexp
type blog_option =
  {
    plugin : plugin;
    name : string;
    value : string
  } with sexp

let plugin_of_string = function
  | "__BLOG__" -> Blog
  | pl_name -> Plugin pl_name

let string_of_plugin = function
  | Blog -> "__BLOG__"
  | Plugin pl_name -> pl_name

let option_of_row row =
  {
    plugin = !!(row-->"plugin") |> plugin_of_string;
    name = !!(row-->"name");
    value = !!(row-->"value");
  }

let map_of_option o =
  StringMap.empty<--("plugin", o.plugin |> string_of_plugin |> sql_string)
                 <--("name", sql_string o.name)
                 <--("value", sql_string o.value)

let print_option = sexp_of_blog_option |- Sexplib.Sexp.to_string_hum |- Printf.printf "%s\n"

type tag =
  {
    tag_id        : int;
    tag_text      : string;
    tag_slug      : string;
    tag_parent_id : int option;
    tag_parent    : (tag Lazy.t) option;
    tag_children  : tag list;
  } with sexp

type tag_db = tag list * tag list (* the list of roots, actually a tree; the flat list of all tags *)

type token =
  {
    tok_value : string;
    tok_ctime : float;
  } with sexp

let string_of_token = sexp_of_token |- Sexplib.Sexp.to_string_hum |- hex_encode
let print_token = string_of_token |- hex_decode |- Printf.printf "%s\n"
let token_of_string = hex_decode |- Sexplib.Sexp.of_string |- token_of_sexp

let token_of_row row =
  {
    tok_value = !!(row-->"value");
    tok_ctime = !!(row-->"ctime") |> Netdate.parse_epoch;
  }

let map_of_token t =
  StringMap.empty<--("value", sql_string t.tok_value)
                 <--("ctime", t.tok_ctime |> Netdate.create |> print_timestamp |> sql_string)

type short_url =
  {
    su_id     : int;
    su_enc    : string;
    su_url    : string;
    su_atime  : float;
  } with sexp

let url_of_row row =
  {
    su_id = !!(row-->"id") |> int_of_string;
    su_enc = !!(row-->"id_enc");
    su_url = !!(row-->"long_url");
    su_atime = !!(row-->"atime") |> Netdate.parse_epoch;
  }

let map_of_url u =
  StringMap.empty<--("id", sql_int u.su_id)
                 <--("id_enc", sql_string u.su_enc)
                 <--("long_url", sql_string u.su_url)
                 <--("atime", u.su_atime |> Netdate.create |> print_timestamp |> sql_string)

let string_of_url = sexp_of_short_url |- Sexplib.Sexp.to_string_hum |- hex_encode
let print_comment = string_of_url |- hex_decode |- Printf.printf "%s\n"
let comment_of_string = hex_decode |- Sexplib.Sexp.of_string |- short_url_of_sexp


module Blog_session =
  struct
    type t =
      | Anonymous of int list * bool
      | Logged of user
        with sexp

    let default = Anonymous ([], false)
    let string_of_t = sexp_of_t |- Sexplib.Sexp.to_string_hum |- hex_encode

    let t_of_string s =
      try s |> hex_decode |> Sexplib.Sexp.of_string |> t_of_sexp
      with Sexplib.Conv.Of_sexp_error (_, _) -> default
  end

module Types = Signatures.Make_types(Blog_session)

