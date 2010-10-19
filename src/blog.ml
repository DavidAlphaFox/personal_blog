let () = Random.self_init ()

open BatStd
open Data_mapping
open Parameters_map

module Default_server_config = Defaults.Make_default_server_config(Types)

module URL_registry = URL_registry.Make(Types)

module Storage_conf =
  struct
    let connection_string = Config.sqlite3_sessions_db_path
  end

module Server_config =
  struct
    type ('get_param, 'post_param, 'b, 'c) url_registry =
         ('get_param, 'post_param, 'b, 'c) Types.url_registry

    let handler_404 = Default_server_config.handler_404
    let handler_500 = Default_server_config.handler_500

    open URL_registry

    let registry = empty_registry |>
      (* ROBOTS.TXT *)
      register_regexp_service "^/robots\\.txt$"
        (new Static.robots)
        unit  (* GET parameters *)
        unit  (* POST parameters *) |>

      (* SHORT URL SERVICE *)
      register_regexp_service "^/u(/(?<short_url>[0-9a-zA-Z]{1,16})?/?)?$"
        (new Short_url.main)
        (opt (string "short_url"))  (* GET parameters *)
        (string "long_url")         (* POST parameters *) |>

      (* OLD STATIC REDIRECT *)
      register_regexp_service "^/wp-content/uploads/(?<old_uri>.*)$"
        (new Dashboard.static_redirect)
        (string "old_uri")
        unit |>

      (* OLD POST URIs REDIRECT *)
      register_regexp_service "^/(?<year>\\d{4})/(\\d{2})/(?<old_slug>.*)/?$"
        (new Dashboard.old_post_redirect)
        ((int "year") ** (string "old_slug"))
        unit |>

      (* OLD HOME URL REDIRECT *)
      register_regexp_service "^/blog/?$" (new Dashboard.old_home_redirect) unit unit |>

      (* COMMENT MODERATION *)
      register_regexp_service "/manage-comment/(?<action>approve|delete|mark-spam)/(?<id>\\d+)/(?<token>[a-fA-F0-9]{32})/?$"
        (new Single_post.manage_comment)
        (tpl3 (string "action") (int "id") (string "token"))
        unit |>

      (* ADMIN *)
      register_regexp_service "^/admin/?$"
        (new Dashboard.main_view) unit unit |>
      register_regexp_service "^/login((/)|(/(?<action>logout)/?))?$"
        (new Login.login)
        (opt (string "action"))
        ((string "login") ** (string "pwd")) |>

      register_regexp_service "^/post-edit(/(?<post_id>\\d+))?/?$"
        (new Post_edit.main)
        (opt (int "post_id")) (* GET parameters *)
        (opt (int "post_id") ** ( (* POST parameters *)
         opt (string "draft_button") ** (
         opt (string "publish_button") ** (
         opt (string "delete_button") ** (
         lst (int "tag") ** (
         opt (user_type "new_ctime" Post_edit.netdate_of_string) ** (
         opt (user_type "new_mtime" Post_edit.netdate_of_string) ** (
         string "post_title" ** (
         string "new_post_slug" ** (
         string "post_content" ** (
         string "post_excerpt" ** (
         tpl3 (bool "comment_status") (bool "ping_status") (bool "private_status")
                            )))))))))))) |>

      register_regexp_service "^/post-save/?$"
        (new Post_edit.post_save)
        unit                                            (* GET parameters *)
        ((int "post_id") ** (string "post_content")) |> (* POST parameters *)

      register_regexp_service "^/post-list/?$"
        (new Post_list.main)
        (tpl3 (opt (string "post_status"))
              (opt (int "page"))
              (opt (string "message")))
        unit |>

      (* HOME PAGE *)
      register_regexp_service "^/?$"
        (new Multi_post.home_page)
        (opt (int "page"))    (* GET parameters *)
        unit                  (* POST parameters *) |>

      (* HOME PAGE RSS *)
      register_regexp_service "^/feed/?$"
        (new Feed.home)
        unit  (* GET parameters *)
        unit  (* POST parameters *) |>

      (* SPECIAL OLD RSS URL FOR COMPATIBILITY *)
      register_regexp_service "^/category/(?<facets_path>programming-languages/objective-caml)/feed/?$"
        (new Feed.facets)
        (string "facets_path")  (* GET parameters *)
        unit                    (* POST parameters *) |>

      (***********************************************************)
      (* OTHER SPECIAL CASES: THESE URLS ARE REQUESTED BY ROBOTS *)
      (***********************************************************)
      register_static_redirect "^/blog/feed/$"
                               "/feed/" |>

      register_static_redirect "^/category/passions/information-technology/computer-programming/feed/$"
                               "/facets/passions/information-technology/computer-programming/feed/" |>

      register_static_redirect "^/category/passions/information-technology/internet/feed/$"
                               "/facets/passions/information-technology/internet/feed/" |>

      register_static_redirect "^/category/type-of-text/article/feed/$"
                               "/facets/type-of-text/article/feed/" |>

      register_static_redirect "^/category/type-of-text/review/feed$"
                               "/facets/type-of-text/review/feed/" |>

      register_static_redirect "^/tag/objective-caml/feed/$"                                             
                               "/facets/programming-languages/objective-caml/feed/" |>
      (***********************************************************)

      (* FACETS RSS *)
      register_regexp_service "^/facets/(?<facets_path>[a-zA-Z0-9/-]+)/feed/?$"
        (new Feed.facets)
        (string "facets_path")  (* GET parameters *)
        unit                    (* POST parameters *) |>

      (* SINGLE POST COMMENTS RSS FEED *)
      register_regexp_service "^/post/(?<year>\\d{4,4})/(?<slug>[a-zA-Z0-9-]+)/feed/?$"
        (new Feed.comments)
        (tpl3 (int "year")
              (string "slug")
              (opt (bool "moderation")))  (* GET parameters *)
        unit                              (* POST parameters *) |>

      (* ARCHIVES PER YEAR *)
      register_regexp_service "^/(?<year>\\d{4,4})/?$"
        (new Multi_post.year_archive)
        ((int "year") ** opt (int "page"))    (* GET parameters *)
        unit                        (* POST parameters *) |>

      (* ARCHIVES PER FACETS *)
      register_regexp_service "^/facets/(?<facets_path>[a-zA-Z0-9/-]+)/?$"
        (new Multi_post.facets_archive)
        ((string "facets_path") ** opt (int "page"))  (* GET parameters *)
        unit                                          (* POST parameters *) |>

      (* POSTS *)
      register_regexp_service "^/post/(?<year>\\d{4,4})/(?<slug>[a-zA-Z0-9-]+)/?$"
        (new Single_post.main)
        (tpl3 (int "year")
              (string "slug")
              (opt (bool "moderation")))      (* GET parameters *)
        (int "post_id" ** (                  (* POST parameters *)
         opt (string "preview-comment") ** (
         opt (string "submit-comment") ** (
         tpl4 (string "author")
              (string "email")
              (opt (string "url"))
              (string "comment"))))) |>

      (* POSTS PREVIEW *)
      register_regexp_service "^/preview/(?<post_id>\\d+)/?$"
        (new Single_post.preview)
        (int "post_id") (* GET parameters *)
        unit |>         (* POST parameters *)

      register_regexp_service "^/comment-preview/?$"
        (new Single_post.comment_preview)
        unit
        (string "comment_text") |>

      (* PAGES *)
      register_predicate_service Pages.selector (new Pages.page)
        (int "page_id")                               (* GET parameters *)
        ((int "page_id") ** (string "page_password")) (* POST parameters *)

    let application_name = "My personal weblog"
    let enable_session = true
  end

module Coockie_conf =
  struct
    let max_age () = (Config.cookie_conf ()).Config.Config_file.max_age
    let name () = (Config.cookie_conf ()).Config.Config_file.name

    let make_cookie age name data =
      let domain = (Config.cookie_conf ()).Config.Config_file.domain in
      let path = (Config.cookie_conf ()).Config.Config_file.path in
        match age with
          | Signatures.Session ->
              Netcgi.Cookie.make ~domain ~path name data
          | Signatures.Max_age max_age ->
              Netcgi.Cookie.make ~max_age ~domain ~path name data

    let key_gen (cgi : Netcgi.cgi_activation) =
      let e = cgi#environment in
      let client_ip = e#cgi_remote_addr in
      let client_user_agent =
        try e#input_header_field "User-Agent"
        with Not_found -> Random.float 42.0 |> Int64.of_float |> Int64.to_string in
      let now = Unix.time () |> Int64.of_float |> Int64.to_string in
      let random_float = Random.float 42.0 |> Int64.of_float |> Int64.to_string in
        Utilities.compute_sha1 (client_ip ^ client_user_agent ^ now ^ random_float)
  end

module Session_mgr = Session_common.Make(Storage_conf)(Coockie_conf)(Sqlite3_state)(Blog_session)
module Server = Server.Make(Blog_session)(Types)(Config.Config_file)(Server_config)(Session_mgr)

Config.read_config_file (Server.get_config_filename ());;

let () = Server.main ()

