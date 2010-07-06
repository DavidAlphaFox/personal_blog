open BatStd
open CamlTemplate.Model
open Utilities
open Data_mapping
open Config
open Common_fragments
module X = XHTML
module U = Unix
module Ak = Akismet

let (<--) = Template.Context.(<--)

let spf = Printf.sprintf
let protocol_regexp = Str.regexp "^[a-zA-Z][a-zA-Z0-9]+://"

class ['g, 'p] main cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method get (short_url : 'g) =
      self#old_browser_warning ();
      self#include_google_analytics ();
      match short_url with
        | None -> begin
            ctx<--("js_object_name", Tstr "pages");
            let content = Template.render "public/short_url.tmpl" ctx in
              new Http.response ~cache:`No_cache ~headers:[] ~content ~content_type cgi
          end
        | Some url -> begin
            let url = Db.get_long_url url in
              match url with
                | Some url -> new Http.response_redirect ~status:`Moved_permanently url cgi
                | None -> raise Http.Http404
          end

    method post (long_url : 'p) =
      let long_url =
        if (Str.string_match protocol_regexp long_url 0)
        then long_url
        else "http://" ^ long_url in

        ctx<--("js_object_name", Tstr "pages");
        let short_url = Db.get_short_url long_url in
          ctx<--("short_url", Tstr (Blog_template.abs_url ("/u/" ^ short_url)));
          ctx<--("google_analytics", Tbool false);
          let content = Template.render "public/short_url.tmpl" ctx in
            new Http.response ~cache:`No_cache ~headers:[] ~content ~content_type cgi

        (*
        SELECT MIN(id + 1) AS free FROM s WHERE id + 1 NOT IN (SELECT id FROM s);
        *)
  end
