open BatStd
open CamlTemplate.Model
open Utilities
open Data_mapping
open Config
open Common_fragments
module X = XHTML
module U = Unix

let (<--) = Template.Context.(<--)

let take = BatList.take
let drop_while = BatList.drop_while
let last = BatList.last
let strip = BatString.strip
let nsplit = BatString.nsplit
let join = BatString.join

let content_type = "application/rss+xml; charset=UTF-8"

class ['g, 'p] feed_base (cgi : Netcgi.cgi_activation) =
  object (self)
    inherit ['g, 'p] view_base cgi

    method private render_posts_feed path posts =
      let now = Unix.gettimeofday () in
      let new_site_begins = Netdate.parse_epoch "2010-06-05 00:00:00" in

      (* Posts with mtime in the future should not appear in feeds *)
      let posts = List.filter (
          fun post ->
            let mtime = !!(post-->"mtime") |> Netdate.parse_epoch in
              if mtime < now then true else false
        ) posts in

      (* Old posts should not appear in feeds*)
      let posts =
        List.filter (fun p ->
          if (!!(p-->"mtime") |> Netdate.parse_epoch) > new_site_begins then true else false)
        posts in

      let last_build_date =
        try List.map (fun p -> !!(p-->"mtime")) posts |> BatList.max |> Netdate.parse_epoch
        with Invalid_argument "Empty List" -> new_site_begins in

      let posts = List.map (
          fun p -> tpl_model_of_complete_post ~process_markup:true ~allow_html:true p |> fst
        ) posts in

      ctx<--("facets_path", Tstr (join "/" path));
      ctx<--("last_build_date", Tfloat last_build_date);
      ctx<--("blog_title", Tstr (Lazy.force Options.blog_title));
      ctx<--("posts", Tlist posts);
      let content = Template.render "public/post_feed.tmpl" ctx in
        new Http.response ~cache:`Unspecified ~headers:[`Last_Modified last_build_date]
                          ~content ~content_type cgi
  end

class ['g, 'p] home cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] feed_base cgi

    method get (() : 'g) =
      let limit = Lazy.force Options.blog_post_in_rss in
      let posts = Db.complete_post_list ~filter_clause:"WHERE status = '1'" () |> take limit in
        self#render_posts_feed [] posts
  end
;;

class ['g, 'p] facets cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] feed_base cgi

    method get (facets_path : 'g) =
      let limit = Lazy.force Options.blog_post_in_rss in
      let path = nsplit facets_path "/" |> List.map strip |>
        drop_while ((=) "") |> List.rev |>
          drop_while ((=) "") |> List.rev in

      let tag_id_list, facet_text = Multi_post.get_facets_list path in
      let last_tag =
        let last_id = last tag_id_list in
          List.find (fun t -> t.tag_id = last_id) (Lazy.force Db.tag_db |> snd) in
      let posts = Db.posts_by_tags tag_id_list 0 (-1) |> take limit in
        ctx<--("feed_title", Tstr (last_tag.tag_text));
        self#render_posts_feed path posts
  end
;;

class ['g, 'p] comments cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] feed_base cgi

    method get ((year, slug, moderation) : 'g) =
      let post = try Db.select_post_by_year_slug year slug
        with Not_found -> raise Http.Http404 in

      let post = Db.complete_post post.id in

      let post, last_build_date = tpl_model_of_complete_post ~process_markup:true ~allow_html:true post in
        ctx<--("post", post);
        ctx<--("last_build_date", Tfloat last_build_date);

        let content = Template.render "public/post_comments_feed.tmpl" ctx in
          new Http.response ~cache:`Unspecified ~headers:[`Last_Modified last_build_date]
                            ~content ~content_type cgi
  end
;;

