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
let take = BatList.take
let drop = BatList.drop
let strip = BatString.strip
let nsplit = BatString.nsplit
let join = BatString.join

let render_multi_post cgi ctx page posts title pagination_url rss_present =
  let now = Unix.gettimeofday () in
  let posts = List.filter (
      fun post ->
        let mtime = !!(post-->"mtime") |> Netdate.parse_epoch in
          if mtime < now then true else false
    ) posts in
  let page = opt_def 1 page in
  let post_per_page = Lazy.force Options.blog_post_per_page in
  let inf = (page - 1)*post_per_page in
  let posts_count = List.length posts in
  let posts = posts |> drop inf |> take post_per_page in
  let last_page = posts_count/post_per_page + (if posts_count mod post_per_page = 0 then 0 else 1) in
    if page < 1 || page > last_page then raise Http.Http404;
    let prev_page = if page > 1 then Tint (page - 1) else Tnull in
    let next_page = if page < last_page then Tint (page + 1) else Tnull in

    let e1, e2 = List.map (
        fun p -> tpl_model_of_complete_post ~process_markup:true ~allow_html:true p
      ) posts |> BatList.enum |> BatEnum.uncombine in
    let posts, last_modified = BatList.of_enum e1, BatList.of_enum e2 in
    let last_modified = BatList.max last_modified in
      ctx<--("top_menu", Tstr (X.string_of_element (page_menu None)));
      ctx<--("js_object_name", Tstr "multi_post");
      ctx<--("content_header", Tstr title);
      ctx<--("page_title", Tstr title);
      ctx<--("posts", Tlist posts);
      ctx<--("prev_page", prev_page);
      ctx<--("next_page", next_page);
      ctx<--("pagination_url", Tstr pagination_url);
      ctx<--("enable_summary", Tbool true);
      if rss_present then
        ctx<--("rss_link", Tstr (Blog_template.abs_url (pagination_url ^ "/feed/")));
      let headers = [(`Last_Modified last_modified)] in
      let content = Template.render "public/multi_post.tmpl" ctx in
        new Http.response ~content ~content_type ~cache:`Unspecified ~headers cgi
;;

class ['g, 'p] home_page cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method get (page : 'g) =
      self#old_browser_warning ();
      self#include_google_analytics ();
      let posts = Db.complete_post_list ~filter_clause:"WHERE status = '1'" () in
        render_multi_post cgi ctx page posts "Latest Articles" "" true
  end
;;

class ['g, 'p] year_archive cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method get ((year, page) : 'g) =
      self#old_browser_warning ();
      self#include_google_analytics ();
      let filter_clause = spf "WHERE strftime('%%Y', mtime) = '%d' AND status = '1'" year in
      let posts = Db.complete_post_list ~filter_clause () in
      let title = spf "Articles Written in %d" year in
      let pagination_url = spf "/%d" year in
        render_multi_post cgi ctx page posts title pagination_url false
  end
;;

let get_facets_list path =
  let rec aux (tag_list, current_parent) path =
    match path with
      | [] -> current_parent
      | hd::tl ->
          let tag = List.find (fun t -> if t.tag_slug = hd then true else false) tag_list in
            aux (tag.tag_children, Some tag) tl
  in (* aux *)
    try
      let selected = aux (Db.tag_db |> Lazy.force |> fst, None) path in
      let selected = !!selected in
        (if BatList.is_empty selected.tag_children
        then [ selected.tag_id ]
        else Db.flat_tags [selected] |> List.map (fun t -> t.tag_id)), selected.tag_text
    with Not_found -> raise Http.Http404
;;

class ['g, 'p] facets_archive cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method get ((facets_path, page) : 'g) =
      self#old_browser_warning ();
      self#include_google_analytics ();
      let path = nsplit facets_path "/" |> List.map strip in
      let tag_list, facet_text = get_facets_list path in
      let posts = Db.posts_by_tags tag_list 0 (-1) in
      let title = spf "Articles Labelled with &#147;%s&#148;" facet_text in
      let pagination_url = spf "/facets/%s" (join "/" path) in
        ctx<--("feed_title", Tstr facet_text);
        render_multi_post cgi ctx page posts title pagination_url true
  end
;;

