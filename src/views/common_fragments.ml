open BatStd
open CamlTemplate.Model
open Config
open Utilities
open Data_mapping
open Db

module X = XHTML

let (<--) = Template.Context.(<--)

let _u = Blog_template.abs_url

let email_regexp =
  Pcre.regexp ~flags:[`CASELESS; `UTF8]
    "\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}\\b"

let box _class m = Printf.sprintf "<p class=\"%s message-box\">%s</p>" _class m

let error_box m = box "error" m

let message_box m = box "message" m

let owner_name () =
  let owner = select_user (Lazy.force Options.my_email_address) in
    !!(owner.displ_name)


let admin_head page_name =
  let head_elements = [
      X.pc ""; (* Line feed *)
      X.title ((Lazy.force Options.blog_title) ^ " › " ^ page_name);
      X.meta ~a:[X.a_name "author"] ~content:"Paolo Donadeo";
      X.meta ~a:[X.a_http_equiv "content-type"] ~content:"application/xhtml+xml; charset=UTF-8";
      X.pc ""; (* Line feed *)
      X.comment ~line:true "The YUI CSS Foundation";
      X.link [X.a_rel [`stylesheet]; X.a_type "text/css"; X.a_href (_u"/static/yui/2.7.0/build/reset-fonts-grids/reset-fonts-grids.css")];
      X.link [X.a_rel [`stylesheet]; X.a_type "text/css"; X.a_href (_u"/static/yui/2.7.0/build/base/base-min.css")];
      X.pc ""; (* Line feed *)
      X.comment ~line:true "My admin style sheet";
      X.link [X.a_rel [`stylesheet]; X.a_type "text/css"; X.a_href (_u"/static/css/admin.css")];
      X.script ~a_type:"text/javascript" ~a:[X.a_src "/static/jquery/1.3.2/jquery-1.3.2.min.js"] [];
      X.script ~a_type:"text/javascript" ~a:[X.a_src "/static/js/common.js"] [];
    ] in
    Tstr (List.map X.string_of_element head_elements |> String.concat "\n")

let admin_header = lazy (
  let banner_div = X.div ~a:[X.a_id "hd"] [
    X.h1 [X.a ~a:[X.a_href (_u"/")] [X.pc (Lazy.force Options.blog_title)]];
    X.div ~a:[X.a_id "user_info"] [
      X.p [
        X.pc "Hello, ";
        X.a ~a:[X.a_title "Edit your profile"; X.a_href (_u"/TODO")] [X.pc (owner_name ())];
        X.pc " | ";
        X.a ~a:[X.a_title "Log Out"; X.a_href (_u"/login/logout")] [X.pc "Log Out"];
      ]
    ];
    X.div ~a:[X.a_id "main-actions"] [
      X.ul ~a:[X.a_class "first"]
        (X.li [X.a ~a:[X.a_href (_u"/post-edit")] [X.pc "New Post"]]) [];
      X.ul ~a:[X.a_class "inside"]
        (X.li [X.a ~a:[X.a_href (_u"/TODO")] [X.pc "New Page"]]) [
        (X.li [X.a ~a:[X.a_href (_u"/TODO")] [X.pc "Comments"]]);
        (X.li [X.a ~a:[X.a_href (_u"/post-list?post_status=draft")] [X.pc "Drafts"]])
        ]
    ]
  ] in
    Tstr (X.string_of_element banner_div)
)

let admin_menu = lazy (
  let dashboard_li =
    X.li ~a:[X.a_class "top-entry"] [
        X.h4 [X.pc "Dashboard"];
        X.ul ~a:[X.a_class "open"]
          (X.li [X.a ~a:[X.a_href (_u"/admin")] [X.pc "Dashboard"]]) [
        ]
      ] in
  let posts_li =
    X.li ~a:[X.a_class "top-entry"] [
        X.h4 [X.pc "Posts"];
        X.ul ~a:[X.a_class "open"]
          (X.li [X.a ~a:[X.a_href (_u"/post-list")] [X.pc "Posts List"]]) [
          (X.li [X.a ~a:[X.a_href (_u"/post-edit")] [X.pc "Add New"]]);
        ]
      ] in
  let comments_li =
    X.li ~a:[X.a_class "top-entry"] [
        X.h4 [X.pc "Comments"];
        X.ul ~a:[X.a_class "open"]
          (X.li [X.a ~a:[X.a_href (_u"/comments-list")] [X.pc "Comments List"]]) [
        ]
      ] in
  let menu_div = X.div ~a:[X.a_id "adminmenu"; X.a_class "yui-b"] [
    X.ul
      dashboard_li [
      posts_li;
      comments_li
      ]
  ] in
    Tstr (X.string_of_element menu_div)
)

let tvalue_of_sql_bool v =
  match v with
    | "0" -> Tbool false
    | "1" -> Tbool true
    | _ -> failwith "only 0 and 1 are allowed as bool SQL values"

let escape_string_option s =
  Tstr (X.pc (opt_def "" s) |> X.string_of_element)

let tpl_model_of_comment db_comment =
  let comment = Template.Context.create () in
    comment<--("id", Tint db_comment.comment_id);
    comment<--("post_id", Tint db_comment.post_id);
    comment<--("author_name", Tstr db_comment.author_name);
    comment<--("author_url", match db_comment.author_url with
                              | None -> Tnull
                              | Some u -> Tstr u);
    comment<--("author_ip", Tstr (Unix.string_of_inet_addr db_comment.author_ip |> sql_string));
    comment<--("author_ua", Tstr db_comment.author_ua);
    comment<--("comment_time", Tfloat db_comment.comment_time);
    comment<--("content", Tstr (db_comment.content |> Markup.parse_markup |>
                                Markup.safe_xhtml_of_markup |> X.string_of_element));
    comment<--("gravatar", Tstr (compute_md5 db_comment.author_email));
    Thash comment

let tpl_model_of_complete_post ?(process_markup = false) ?(allow_html = false) row_map =
  let (-->) = BatMap.StringMap.Infix.(-->) in
  let post_id = !!(row_map-->"post_id") |> int_of_string in
  let tags = Db.post_tags post_id in

  let tags_model =
      List.map
      (
        fun tag ->
          let tag_model = Template.Context.create () in
            tag_model<--("id", Tint tag.tag_id);
            tag_model<--("text", Tstr tag.tag_text);
            tag_model<--("slug", Tstr tag.tag_slug);
            tag_model<--("slug_path", Tstr (Db.tag_slug_path tag));
            Thash tag_model
      ) tags in

  let content =
    let db_content = row_map-->"content" in
    match process_markup, allow_html with
      | false, false -> escape_string_option db_content
      | false, true -> failwith "Invalid argument: process_markup and allow_html cannot be (false,true)"
      | true, false -> failwith "This should never happen"
      | true, true -> Tstr ((opt_def "" db_content) |> Markup.parse_markup |>
                            Markup.xhtml_of_markup |> X.unsafe_string_of_element) in

  let post = Template.Context.create () in
    post<--("post_id", Tint post_id);
    post<--("ctime", Tfloat (!!(row_map-->"ctime") |> Netdate.parse_epoch));
    post<--("mtime", Tfloat (!!(row_map-->"mtime") |> Netdate.parse_epoch));
    post<--("status", !!(row_map-->"status") |> tvalue_of_sql_bool);
    post<--("title", (row_map-->"title") |> escape_string_option);
    post<--("url_title", (row_map-->"url_title") |> escape_string_option);
    post<--("content", content);
    post<--("summary", Tstr (opt_def "" (row_map-->"summary")));
    post<--("comments", !!(row_map-->"comments") |> tvalue_of_sql_bool);
    post<--("pings", !!(row_map-->"pings") |> tvalue_of_sql_bool);
    post<--("private", !!(row_map-->"private") |> tvalue_of_sql_bool);
    post<--("email", Tstr !!(row_map-->"email"));
    post<--("first_name", (match row_map-->"first_name" with | None -> Tnull | Some s -> Tstr s));
    post<--("last_name", (match row_map-->"last_name" with | None -> Tnull | Some s -> Tstr s));
    post<--("displ_name", (match row_map-->"displ_name" with | None -> Tnull | Some s -> Tstr s));
    post<--("comments_count", Tint (!!(row_map-->"comments_count") |> int_of_string));
    post<--("permalink", Tstr (Blog_template.permalink
                                (!!(row_map-->"ctime") |> year_of_iso_time)
                                (!!(row_map-->"url_title"))
                              )
           );
    post<--("tags", Tlist tags_model);
    post<--("tags_count", Tint (List.length tags_model));
    let post_comments = select_comments_by_post post_id in
    let last_modified =
      let comments_times = List.map (fun c -> c.comment_time) post_comments in
        (!!(row_map-->"mtime") |> Netdate.parse_epoch)::comments_times |> BatList.max in
      post<--("post_comments", Tlist (List.map tpl_model_of_comment post_comments));
      Thash post, last_modified

let page_menu =
  let menu_map = lazy (
    let page_menu_aux current_page_id =
      match current_page_id with
        | None -> begin
            let roots = (Lazy.force Db.page_db |> fst) in
            let hd, tl = List.hd roots, List.tl roots in
              X.div ~a:[X.a_id "main-nav"; X.a_class "col"] [
                  X.ul (X.li ~a:[X.a_class "page_item"] [
                    X.a ~a:[X.a_rel [`bookmark]; X.a_title !!(hd.page_title); X.a_href (_u hd.page_url_title)]
                        [ X.pc !!(hd.page_title) ]
                      ] )
                    (List.map (fun li -> X.li ~a:[X.a_class "page_item"] [
                                  X.a ~a:[X.a_rel [`bookmark]; X.a_title !!(li.page_title); X.a_href (_u li.page_url_title)]
                                  [ X.pc !!(li.page_title) ]
                                ]
                              ) tl)
                ]
          end
        | Some id -> begin
            let current_page = List.find
                                (fun p -> if p.page_id = id then true else false)
                                (Lazy.force Db.page_db |> snd) in
            let page_parent_id = current_page.page_parent_id in
            let li_list = if page_parent_id = None
                          then []
                          else
                            let parent_page = Lazy.force (!!(current_page.page_parent)) in
                              [
                                X.li ~a:[X.a_class "page_item"] [
                                  X.a ~a:[X.a_title "Go up"; X.a_href (_u parent_page.page_url_title)]
                                    [ X.pc "Go up" ]
                                ]
                              ] in
            let li_list = li_list @ [
                X.li ~a:[X.a_class "page_item current_page_item"] [ X.pc !!(current_page.page_title) ]
              ] in
            let li_list = li_list @ (List.map
              (
                fun p ->
                  X.li ~a:[X.a_class "page_item"] [
                      X.a ~a:[X.a_title !!(p.page_title); X.a_href (_u p.page_url_title)] [ X.pc !!(p.page_title) ]
                    ]
              ) current_page.page_children) in
              X.div ~a:[X.a_id "main-nav"; X.a_class "col"] [
                  X.ul (List.hd li_list) (List.tl li_list)
                ]
          end
    in (* END OF page_menu_aux *)
      List.fold_left
        (fun map page -> let id = Some page.page_id in (id, page_menu_aux id)::map)
        [ (None, page_menu_aux None) ]
        (Lazy.force Db.page_db |> snd)
  ) in (* menu_map *)
    fun id -> List.assoc id (Lazy.force menu_map)

let msie_regexp = Str.regexp ".*MSIE.[567]\\."

class ['g, 'p] view_base
    (cgi : Netcgi.cgi_activation) =
  object (self)
    val mutable session = Blog_session.default
    val cgi = cgi
    val ctx = Blog_template.create_default_context ()

    method get_session () = session
    method set_session s = session <- s

    method get (par : 'g) : Signatures.http_response =
      failwith "Unexpected GET"

    method post (par : 'p) : Signatures.http_response =
      failwith "Unexpected POST"

    method private is_logged () =
      match self#get_session () with
        | Blog_session.Anonymous _ -> false
        | Blog_session.Logged _ -> true

    method private include_google_analytics () =
      ctx<--("google_analytics", Tbool (not (self#is_logged ())))

    method private old_browser_warning () =
      let is_old = Str.string_match msie_regexp
                    (cgi#environment#input_header_field "User-Agent") 0 in
      let v =
        if is_old
        then begin
          match self#get_session () with
            | Blog_session.Anonymous (l, true) -> false
            | Blog_session.Anonymous (l, false) -> begin
                self#set_session (Blog_session.Anonymous (l, true));
                true
              end
            | _ -> false
        end
        else false in
        Hashtbl.add ctx "is_old_browser" (Tbool v)
  end

