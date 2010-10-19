open BatStd
open CamlTemplate.Model
open Utilities
open Data_mapping
open Config
open Common_fragments
module X = XHTML
module U = Unix

let (<--) = Template.Context.(<--)

let spf = Printf.sprintf

let head =
  Tstr (
    List.map X.string_of_element [
        X.script ~a_type:"text/javascript" ~a:[X.a_src "/static/js/admin.js"] [];
        X.script ~a_type:"text/javascript" [
            X.cd ~prefix:"//" "
                $(document).ready(
                        function() {
                            Admin.common.init_eventi_gui();
                            Admin.post_edit_page.init_eventi_gui();
                        }
                    );"
        ]
    ] |> String.concat "\n"
  )

let escape_string_option s =
  (X.pc (opt_def "" s) |> X.string_of_element)

let string_of_post_option f post =
  match post with
    | None -> ""
    | Some p -> f p

let bool_of_post_option f post =
  match post with
    | None -> false
    | Some p -> f p

let post_title p =
  Tstr (string_of_post_option (fun p -> escape_string_option p.title) p)

let post_publish_status p =
  match p with
    | None -> Tint 0
    | Some p -> if not p.status then Tint 1 else Tint 2

let post_year p =
  let time = match p with None -> U.time () | Some p -> p.ctime in
    Tint ((time |> U.gmtime).U.tm_year + 1900)

let post_slug p =
  string_of_post_option (fun p -> escape_string_option p.url_title) p

let post_content p =
  Tstr (string_of_post_option (fun p -> escape_string_option p.post_content) p)

let post_excerpt p =
  Tstr (string_of_post_option (fun p -> escape_string_option p.summary) p)

let comment_status p = Tbool (bool_of_post_option (fun p -> p.comments) p)
let ping_status p = Tbool (bool_of_post_option (fun p -> p.pings) p)
let private_status p = Tbool (bool_of_post_option (fun p -> p.is_private) p)

let netdate_of_string s =
  try (Netdate.parse_epoch s) -. (float_of_int (60*Netdate.localzone))
  with Invalid_argument("Parse.date") -> Parameters_map.convert_fail "Netdate.float"


class ['g, 'p] main cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method private display_post post_id =
      let post_id_str, current_post, page_h2, ctime, mtime =
        try (
          match post_id with
            | None ->
                let now = Unix.gettimeofday () in
                  Tstr "", None, Tstr "Add New Post", now, now
            | Some id ->
                let post = Db.select_post id in
                let ctime = post.ctime in
                let mtime = post.mtime in
                  Tstr (string_of_int id), Some post, Tstr "Edit Post", ctime, mtime
        )
        with Not_found -> raise Http.Http404 in

      let post_tags = match post_id with
        | None -> []
        | Some id -> Db.post_tags id in

      let tags_table = Lazy.force Db.tags_table in
        let module IntSet = BatSet.IntSet in
        let tags_table_xhtml =
          let alternate = ref true in
          let checked_tags = ref IntSet.empty in
          List.map
          (
            fun (tag_path : (int*string*string) list) ->
              let alt_class = if !alternate = true then (alternate := false; " alternate")
                                                   else (alternate := true; "") in
              X.fieldset ~a:[X.a_class alt_class] (
                List.map
                (
                  fun (tag_id, tag_text, tag_slug) ->
                    let checkbox_status =
                      if  List.exists (fun t -> t.tag_id = tag_id) post_tags &&
                          not (IntSet.mem tag_id !checked_tags) then begin
                          checked_tags := IntSet.add tag_id !checked_tags;
                          [X.a_checked]
                      end
                      else if (IntSet.mem tag_id !checked_tags) then [X.a_disabled]
                      else [] in
                    X.span [
                      X.span ~a:[X.a_class "tag"] [X.pc tag_text];
                      X.input ~a:(checkbox_status@[X.a_value (string_of_int tag_id);
                                                   X.a_name (spf "tag-%d" tag_id)]) ~a_type:`checkbox ();
                      X.strong [X.pc " › "]
                    ]
                )
                tag_path
              )
          )
          tags_table in
        let tags_table_text = List.map X.string_of_element tags_table_xhtml |>
                                String.concat "\n" in

          ctx <-- ("admin_head", admin_head "Edit Post");
          ctx <-- ("head", head);
          ctx <-- ("admin_header", Lazy.force admin_header);
          ctx <-- ("admin_menu", Lazy.force admin_menu);
          ctx <-- ("page_h2", page_h2);
          (match post_id with | None -> () | Some id -> ctx <-- ("post_id", Tint id));
          ctx <-- ("ctime", Tfloat ctime);
          ctx <-- ("mtime", Tfloat mtime);
          ctx <-- ("pub_status", post_publish_status current_post);
          ctx <-- ("permalink", Tstr (Blog_template.permalink
                                      (Netdate.create ctime).Netdate.year
                                      (post_slug current_post))
                  );
          ctx <-- ("post_title", post_title current_post);
          ctx <-- ("post_year", post_year current_post);
          ctx <-- ("post_slug", Tstr (post_slug current_post));
          ctx <-- ("post_content", post_content current_post);
          ctx <-- ("post_excerpt", post_excerpt current_post);
          ctx <-- ("comment_status", comment_status current_post);
          ctx <-- ("ping_status", ping_status current_post);
          ctx <-- ("private_status", private_status current_post);
          ctx <-- ("tags_table", Tstr tags_table_text);

          let content = Template.render "admin/post_edit.tmpl" ctx in
            new Http.response ~content ~content_type cgi
      (* end of method display_post *)

    method get (post_id : 'g) =
      match session with
        | Blog_session.Anonymous _ -> new Http.response_redirect "/login" cgi
        | Blog_session.Logged u -> self#display_post post_id

    method private save_draft email post_id tags new_ctime new_mtime post_title
                              new_post_slug post_content post_excerpt comment_status
                              ping_status private_status =
      let mtime = opt_def (Unix.gettimeofday ()) new_mtime in
      let new_post = match post_id with
        | Some post_id -> (
          let old_post = Db.select_post post_id in
          let ctime = opt_def old_post.ctime new_ctime in
            { old_post with
              ctime = ctime;
              mtime = mtime;
              status = false; (* it's a draft *)
              title = Some post_title;
              url_title = Some new_post_slug;
              post_content = Some post_content;
              summary = Some post_excerpt;
              comments = comment_status;
              pings = ping_status;
              is_private = private_status;
            }
          )
        | None -> (
          let post_id = Db.next_val "posts" "post_id" in
          let ctime = opt_def (Unix.gettimeofday ()) new_ctime in
            {
              id = post_id;
              author = email;
              ctime = ctime;
              mtime = mtime;
              status = false; (* it's a draft *)
              title = Some post_title;
              url_title = Some new_post_slug;
              post_content = Some post_content;
              summary = Some post_excerpt;
              comments = comment_status;
              pings = ping_status;
              is_private = private_status;
              pinged = None;
            }
          ) in
        Db.update_post new_post;
        Db.set_post_tags new_post.id tags;
        new Http.response_redirect (spf "/post-edit/%d" new_post.id) cgi

    method private publish_draft email post_id tags new_ctime new_mtime post_title
                                 new_post_slug post_content post_excerpt comment_status
                                 ping_status private_status =
      let mtime = opt_def (Unix.gettimeofday ()) new_mtime in
      let new_post = match post_id with
        | Some post_id -> (
          let old_post = Db.select_post post_id in
          let ctime = opt_def old_post.ctime new_ctime in
            { old_post with
              ctime = ctime;
              mtime = mtime;
              status = true;
              title = Some post_title;
              url_title = Some new_post_slug;
              post_content = Some post_content;
              summary = Some post_excerpt;
              comments = comment_status;
              pings = ping_status;
              is_private = private_status;
            }
          )
        | None -> (
          let post_id = Db.next_val "posts" "post_id" in
          let ctime = opt_def (Unix.gettimeofday ()) new_ctime in
            {
              id = post_id;
              author = email;
              ctime = ctime;
              mtime = mtime;
              status = true;
              title = Some post_title;
              url_title = Some new_post_slug;
              post_content = Some post_content;
              summary = Some post_excerpt;
              comments = comment_status;
              pings = ping_status;
              is_private = private_status;
              pinged = None;
            }
          ) in
        Db.update_post new_post;
        Db.set_post_tags new_post.id tags;
        new Http.response_redirect (spf "/post-edit/%d" new_post.id) cgi

    method private delete_post post_id =
      match post_id with
        | None -> failwith "To delete a post I need an ID!"
        | Some id -> (
            Db.delete_post id;
            new Http.response_redirect "/post-list?message=message-deleted" cgi
          )

    method post ((post_id, (draft_button, (publish_button, (delete_button, (tag_list, (
                 new_ctime, (new_mtime, (post_title, (new_post_slug, (
                 post_content, (post_excerpt, (comment_status, ping_status, private_status)))))))))))) : 'p) =
      match session with
        | Blog_session.Anonymous _ -> new Http.response_redirect "/login" cgi
        | Blog_session.Logged u -> (
            match draft_button, publish_button, delete_button with
              | Some _, None, None -> self#save_draft u.email post_id tag_list new_ctime new_mtime post_title
                                                      new_post_slug post_content post_excerpt comment_status
                                                      ping_status private_status
              | None, Some _, None -> self#publish_draft u.email post_id tag_list new_ctime new_mtime post_title
                                                         new_post_slug post_content post_excerpt comment_status
                                                         ping_status private_status
              | None, None, Some _ -> self#delete_post post_id
              | _, _, _ -> failwith "No action specified!"
          )
  end

let make_json status message =
  spf " { \"status\" : \"%s\", \"message\" : \"%s\"} " status message

class ['g, 'p] post_save cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method post ((post_id, post_content) : 'p) =
      let content_type = "application/json" in
      match session with
      | Blog_session.Anonymous _ -> new Http.response_redirect "/login" cgi
      | Blog_session.Logged u -> begin
          try
            let old_post = Db.select_post post_id in
            Db.update_post { old_post with post_content = Some post_content; };
            let content = make_json "OK" "Post content saved!" in
            new Http.response ~content ~content_type cgi
          with
          | e -> begin
              let exc_str = Printexc.to_string e in
              let content = make_json "KO" "An exception occurred: \"" ^ exc_str  ^ "\"" in
              new Http.response ~content ~content_type cgi
            end
        end
  end

