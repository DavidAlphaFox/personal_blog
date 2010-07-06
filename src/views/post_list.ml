open BatStd
open CamlTemplate.Model
open Utilities
open Data_mapping
open Config
open Common_fragments
module X = XHTML
module U = Unix
module O = Options

let (<--) = Template.Context.(<--)

let spf = Printf.sprintf

let tablenav post_status max page =
  let admin_post_per_page = Options.admin_post_per_page () in
  let post_status = match post_status with "" -> "" | s -> "&amp;post_status=" ^ s in
  let inf = (page - 1)*admin_post_per_page + 1 in
  let sup = page*admin_post_per_page in
  let sup = if max > sup then sup else max in
  let last_page = max / admin_post_per_page + 1 in
  let displaying = spf "Displaying %d–%d of %d" inf sup max in

  let nav_links = [ X.span ~a:[X.a_class "displaying-num"] [X.pc displaying] ] in

  let nav_links = if page > 1 then
      (
        X.a ~a:[X.a_href (spf "/post-list?page=%d%s" (page - 1) post_status);
                X.a_class "prev page-numbers"] [X.pc "«"]
      )::nav_links
    else nav_links in

  let nav_links =
    List.fold_left
    (
      fun nav_links i ->
        if i+1 = page then
          (X.span ~a:[X.a_class "page-numbers current"] [X.pc (string_of_int (i+1))])::nav_links
        else
          (X.a ~a:[X.a_href (spf "/post-list?page=%d%s" (i+1) post_status);
                   X.a_class "page-numbers"]
            [X.pc (string_of_int (i+1))])::nav_links
    )
    nav_links
    (range last_page) in

  let nav_links = if page < last_page then
      (
        X.a ~a:[X.a_href (spf "/post-list?page=%d%s" (page + 1) post_status);
                X.a_class "next page-numbers"] [X.pc "»"]
      )::nav_links
    else nav_links in

  let div = X.div ~a:[X.a_class "tablenav"] [
    X.div ~a:[X.a_class "tablenav-pages"] (List.rev nav_links);
    X.div ~a:[X.a_class "clear"] []
  ] in
    Tstr (X.string_of_element div)

let complete_post_model ctx post_number page status =
  let admin_post_per_page = Options.admin_post_per_page () in
  let post_status = match status with
    "" -> `All | "draft" -> `Draft | "publish" -> `Published
    | _ -> failwith "invalid post status" in
  let inf = (page - 1)*admin_post_per_page + 1 in
  let data = Db.complete_post_paged_list post_status (inf - 1) admin_post_per_page in
    Tlist (List.map (fun p -> tpl_model_of_complete_post p |> fst) data)

class ['g, 'p] main cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method get ((post_status, page, message) : 'g) =
      match self#get_session () with
        | Blog_session.Anonymous _ -> new Http.response_redirect "/login" cgi
        | Blog_session.Logged u -> begin
            let post_status = opt_def "" post_status in
            let page = opt_def 1 page in
            let message = opt_def "" message in

            let all_posts, draft_posts, published_posts =
              Db.all_posts (), Db.draft_posts (), Db.published_posts () in
            let post_number = match post_status with
              "" -> all_posts | "draft" -> draft_posts
              | "publish" -> published_posts | _ -> failwith "Invalid post_status" in
              ctx <-- ("message", Tstr message);
              ctx <-- ("admin_head", admin_head "Post List");
              ctx <-- ("admin_header", Lazy.force admin_header);
              ctx <-- ("admin_menu", Lazy.force admin_menu);
              ctx <-- ("post_status", Tstr post_status);
              ctx <-- ("count_all", Tint all_posts);
              ctx <-- ("count_draft", Tint draft_posts);
              ctx <-- ("count_published", Tint published_posts);
              ctx <-- ("tablenav", tablenav post_status post_number page);
              ctx <-- ("post_list", complete_post_model ctx post_number page post_status);
              let content = Template.render "admin/post_list.tmpl" ctx in
                new Http.response ~content ~content_type cgi
          end
  end

