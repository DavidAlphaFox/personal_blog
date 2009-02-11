open Common

type json user_role =
    Admin
  | Editor
  | Author
  | Contributor
  | Subscriber

and user =
  {
    email : string;
    passwd : string;
    first_name : string option;
    last_name : string option;
    nick_name : string option;
    displ_name : string option;
    visual_edit : bool;
    role : user_role;
    website : string option
  }

let role_of_string = function
  | "admin" -> Admin
  | "editor" -> Editor
  | "author" -> Author
  | "contributor" -> Contributor
  | "subscriber" -> Subscriber
  | _ -> invalid_arg "role_of_string"

let user_of_row row =
  {
    email = !!(row-->"email");
    passwd = !!(row-->"passwd");
    first_name = row-->"first_name";
    last_name = row-->"last_name";
    nick_name = row-->"nick_name";
    displ_name = row-->"displ_name";
    visual_edit = !!(row-->"visual_edit") |> bool_of_string;
    role = !!(row-->"role") |> role_of_string;
    website = row-->"website";
  }

let print_user = json_of_user |- Json_io.string_of_json |- Printf.printf "%s\n"

type json post_type =
    Post
  | Page

and post =
  {
    post_id : int;
    author : string;
    ctime : float;
    mtime : float;
    post_type : post_type;
    status : bool;
    title : string option;
    url_title : string option;
    content : string option;
    summary : string option;
    comments : bool;
    pings : bool;
    is_private : bool;
    pinged : string option
  }

let print_post = json_of_post |- Json_io.string_of_json |- Printf.printf "%s\n"

let post_type_of_string = function
  | "post" -> Post
  | "page" -> Page
  | _ -> invalid_arg "post_type_of_string"

let post_of_row row =
  {
    post_id = !!(row-->"post_id") |> int_of_string;
    author = !!(row-->"author");
    ctime = !!(row-->"ctime") |> Netdate.parse_epoch;
    mtime = !!(row-->"mtime") |> Netdate.parse_epoch;
    post_type = !!(row-->"post_type") |> post_type_of_string;
    status = !!(row-->"status") |> bool_of_string;
    title = row-->"title";
    url_title = row-->"url_title";
    content = row-->"content";
    summary = row-->"summary";
    comments = !!(row-->"comments") |> bool_of_string;
    pings = !!(row-->"pings") |> bool_of_string;
    is_private = !!(row-->"private") |> bool_of_string;
    pinged = row-->"pinged";
  }

type comment =
  {
    comment_id : int;
    post_id : int;
    author_name : string;
    author_email : string;
    author_url : string option;
    author_ip : Unix.inet_addr;
    author_ua : string;
    comment_time : float;
    content : string;
    approved : bool
  }

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
  }

let print_comment c =
  let url =
    match c.author_url with None -> "null" |  Some s -> "\"" ^ s ^ "\"" in
  let ip = Unix.string_of_inet_addr c.author_ip in
    Printf.printf "{
  \"comment_id\": %d,
  \"post_id\": %d,
  \"author_name\": \"%s\",
  \"author_email\": \"%s\",
  \"author_url\": %s,
  \"author_ip\": \"%s\",
  \"author_ua\": \"%s\",
  \"comment_time\": \"%s\",
  \"content\": \"%s\",
  \"approved\": %s
}\n" c.comment_id c.post_id c.author_name c.author_email url ip c.author_ua
      (c.comment_time |> Netdate.create |> print_timestamp) c.content
      (string_of_bool c.approved)

