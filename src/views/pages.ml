open BatStd
open CamlTemplate.Model
open Utilities
open Data_mapping
open Config
open Common_fragments

let spf = Printf.sprintf
let (<--) = Template.Context.(<--)

let pages_regexps = lazy (
    let _, pages = Lazy.force Db.page_db in
      List.fold_left
        (fun acc page -> (Pcre.regexp ("^" ^ ((Lazy_tree.data page).page_url_title) ^ "/?$"), page)::acc)
        []
        pages |> List.rev
  )

let selector script_name =
  try
    let page =
      List.find
      (
        fun (rex, _) ->
          try ignore (Pcre.exec ~rex script_name); true
          with Not_found -> false
      )
      (Lazy.force pages_regexps) |> snd in
      Some ("", BatMap.StringMap.add "page_id" (string_of_int (Lazy_tree.id page)) BatMap.StringMap.empty)
  with
    | Not_found -> None

let in_list l x =
  List.exists (fun e -> if x = e then true else false) l

let not_in_list l x = not (in_list l x)

class ['g, 'p] page cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method private render_page ?(cache = `No_cache) ?(headers = []) page auth_ok error =
        let page_id = Lazy_tree.id page in
        let page = Lazy_tree.data page in
        ctx<--("top_menu", Tstr (X.string_of_element (page_menu (Some page_id))));
        ctx<--("page_title", Tstr !!(page.page_title));
        ctx<--("page_id", Tint page_id);
        ctx<--("page_url", Tstr page.page_url_title);
        ctx<--("js_object_name", Tstr "pages");
        ctx<--("auth_ok", Tbool auth_ok);
        ctx<--("error", Tbool error);

        let inner_context = Blog_template.create_default_context () in

        let inner_content =
          Template.render ("public/pages/" ^ page.page_template) inner_context in

        ctx<--("page_content", Tstr inner_content);

        let content = Template.render "public/page.tmpl" ctx in
          new Http.response ~cache ~headers ~content ~content_type cgi

    method get (id : 'g) =
      self#old_browser_warning ();
      self#include_google_analytics ();
      let current_page = List.find
                          (fun p -> if (Lazy_tree.id p) = id then true else false)
                          (Lazy.force Db.page_db |> snd) in
      let current_page' = Lazy_tree.data current_page in
      let headers, cache =
        match current_page'.page_password with
          | Some _ -> [], `No_cache
          | None -> [ `Last_Modified current_page'.page_mtime ], `Unspecified in
      let auth_ok =
        match current_page'.page_password, self#get_session () with
          | Some _, Blog_session.Anonymous (l, _) when (not_in_list l id) -> false
          | _, _                                                          -> true in
        self#render_page ~headers ~cache current_page auth_ok false

    method post ((id, password) : 'p) =
      let page = List.find
                  (fun p -> if (Lazy_tree.id p) = id then true else false)
                  (Lazy.force Db.page_db |> snd) in
        if (Lazy_tree.data page).page_password = Some password
        then
          let new_session =
            match self#get_session () with
              | Blog_session.Anonymous (l, msie) -> Blog_session.Anonymous (id::l, msie)
              | s -> s in
            self#set_session new_session;
            self#render_page page true false
        else self#render_page page false true
  end

