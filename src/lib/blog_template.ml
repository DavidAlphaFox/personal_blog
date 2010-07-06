open BatStd
open Utilities
open Data_mapping
open Options
open CamlTemplate.Model

let spf = Printf.sprintf;;

let blog_title_tfun ~args =
  match args with
    | [] -> Tstr (Lazy.force blog_title)
    | _ -> raise (Tfun_error "Too many arguments")

let abs_url ?(protocol = `http) path_query_frag =
  let scheme = match protocol with
    | `http -> "http" | `https -> "https" in
  let syntax = Hashtbl.find Neturl.common_url_syntax "http" in
  let debug = Config.debug () in
  let host =  if debug then "localhost" else (Lazy.force Options.blog_hostname) in
    if debug
    then ((Neturl.make_url ~scheme ~host ~port:(Config.debug_port ()) syntax |>
            Neturl.string_of_url) ^ path_query_frag)
    else ((Neturl.make_url ~scheme ~host syntax |>
            Neturl.string_of_url) ^ path_query_frag)

let abs_url_tfun ~args =
  match args with
    | [ Tstr path_query_frag ] -> (
        Tstr (abs_url path_query_frag)
      )
    | [ Tstr protocol; Tstr path_query_frag ] -> (
        let protocol = match protocol with
          | "http" -> `http | "https" -> `https
          | _ -> raise (Tfun_error ("Unsupported protocol: " ^ protocol)) in
          Tstr (abs_url ~protocol path_query_frag)
      )
    | _ -> raise (Tfun_error "Invalid argument")

let permalink year url_title =
  abs_url (Printf.sprintf "/post/%d/%s" year url_title)

let post_permalink post =
  permalink ((Unix.gmtime (post.ctime)).Unix.tm_year + 1900) !!(post.url_title)

let gettimeofday_tfun ~args =
  match args with
    | [] -> Tfloat (Unix.gettimeofday ())
    | _ -> raise (Tfun_error "Invalid argument")

let format_date_tfun ~args =
  match args with
    | [ Tint zone; Tstr fmt; Tfloat ts ] -> (
        Tstr (Netdate.format ~fmt (Netdate.create ~zone ts))
      )
    | [ Tstr fmt; Tfloat ts ] -> (
        Tstr (Netdate.format ~fmt (Netdate.create ts))
      )
    | [ Tfloat ts ] -> (
        Tstr (Netdate.format ~fmt:"%Y-%m-%dT%T" (Netdate.create ts))
      )
    | _ -> raise (Tfun_error "Invalid argument")

let mk_mail_date_tfun ~args =
  match args with
    | [ Tint zone; Tfloat ts ] -> (
        Tstr (Netdate.mk_mail_date ~zone ts)
      )
    | [ Tfloat ts ] -> (
        Tstr (Netdate.mk_mail_date ~zone:Netdate.localzone ts)
      )
    | _ -> raise (Tfun_error "Invalid argument")

let len_tfun ~args =
  match args with
    | [ Tstr s ] -> Tint (String.length s)
    | [ Tlist l ] -> Tint (List.length l)
    | [ Thash h ] -> Tint (Hashtbl.length h)
    | _ -> raise (Tfun_error "Invalid argument")

let facets_tfun ~args =
  let tags = Db.get_tags_count () |> BatList.enum |> BatRandom.shuffle |> Array.to_list in
  let tags, _ =
    List.partition
      (fun (id, _, _, _) -> if id = 16 || id = 15 then false else true) tags in

(* ORDERED
  let tags = BatList.sort
              ~cmp:(
                fun (_, _, _, c1) (_, _, _, c2) ->
                  Pervasives.compare c1 c2
              ) tags |> Array.of_list in

  let even, odd =
    BatArray.fold_lefti
      (
        fun (even, odd) idx t -> if idx mod 2 = 0 then (t::even, odd) else (even, t::odd)
      ) ([], []) tags in

  let tags = (List.rev even) @ odd in
*)

  let min_font, max_font  = 75.0, 200.0 in
  let min_color, max_color = 128.0, 255.0 in
  let min, max = ref 1000, ref 0 in
    List.iter
      (
        fun (_, _, _, c) ->
          if c < !min then min := c;
          if c > !max then max := c;
      ) tags;
    let min, max = log (float_of_int !min), log (float_of_int !max) in
    let font c =
      ((log (float_of_int c)) -. min)*.(max_font -. min_font)/.(max -. min) +. min_font |> int_of_float in
    let color c =
      let color_index = 255 - (((log (float_of_int c)) -. min)*.(max_color -. min_color)/.(max -. min) +. min_color |> int_of_float) in
        spf "#%02X%02X%02X" color_index color_index color_index in
    let b = Buffer.create 1024 in
      List.iter
        (
          fun (i, t, s, c) ->
            Buffer.add_string b
              (spf "<li><a style=\"color: %s; font-size: %d%%;\" href=\"%s\" title=\"%d articles tagged with &#147;%s&#148;\">%s</a></li> "
              (color c) (font c) (abs_url ("/facets"^s)) c t t);
        ) tags;
      Tstr (Buffer.contents b)
;;

let recent_posts_tfun ~args =
  let (-->) = BatMap.StringMap.Infix.(-->) in
  let posts = Db.complete_post_list ~filter_clause:"WHERE status = '1'" () in
  let posts = BatList.take (Lazy.force Options.blog_post_per_page) posts in
  let b = Buffer.create 1024 in
    List.iter
      (
        fun post ->
          let permalink = (permalink
                                (!!(post-->"ctime") |> year_of_iso_time)
                                (!!(post-->"url_title"))
                              ) in
          Buffer.add_string b
            (spf "<li><a href=\"%s\" rel=\"bookmark\" title=\"Permanent Link to “%s”\">%s</a></li>"
            permalink !!(post-->"title") !!(post-->"title"));
      ) posts;
    Tstr (Buffer.contents b)

let escape_xml_tfun ~args =
  match args with
    | [ Tstr s ] ->
      Tstr (
        Netencoding.Html.decode ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 () s |>
          Netencoding.Html.encode ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 ()
      )
    | _ -> raise (Tfun_error "Invalid argument")

let create_default_context () =
  let ctx = Config.Template.Context.create () in
    Hashtbl.add ctx "gettimeofday" (Tfun gettimeofday_tfun);
    Hashtbl.add ctx "format_date" (Tfun format_date_tfun);
    Hashtbl.add ctx "mk_mail_date" (Tfun mk_mail_date_tfun);
    Hashtbl.add ctx "len" (Tfun len_tfun);
    Hashtbl.add ctx "abs_url" (Tfun abs_url_tfun);
    Hashtbl.add ctx "blog_title" (Tfun blog_title_tfun);
    Hashtbl.add ctx "facets" (Tfun facets_tfun);
    Hashtbl.add ctx "recent_posts" (Tfun recent_posts_tfun);
    Hashtbl.add ctx "escape_xml" (Tfun escape_xml_tfun);
    Hashtbl.add ctx "debug" (Tbool (Config.debug ()));
    Hashtbl.add ctx "current_tz" (Tint Netdate.localzone);
    ctx

