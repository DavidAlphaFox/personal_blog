open BatStd
open CamlTemplate.Model
open Data_mapping
open Config
open Common_fragments

let (<--) = Template.Context.(<--)

let head = lazy (
  let module X = XHTML in
    Tstr (
      List.map X.string_of_element [
          X.script ~a_type:"text/javascript" ~a:[X.a_src "/static/js/admin.js"] [];
          X.script ~a_type:"text/javascript" [
              X.cd ~prefix:"//" "
                  $(document).ready(
                          function() {
                              Admin.common.init_eventi_gui();
                              Admin.dashboard_page.init_eventi_gui();
                          }
                      );"
          ]
      ] |> String.concat "\n"
    )
)

let spf = Printf.sprintf


class ['g, 'p] static_redirect cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method get (old_uri : 'g) =
      let new_uri = "/static/old/" ^ old_uri in
        new Http.response_redirect
          ~content:(spf "<code>/wp-content/uploads/%s</code> permanently moved <a href=\"%s\">%s</a>" old_uri new_uri new_uri)
          ~content_type:"text/html"
          ~status:`Moved_permanently new_uri cgi
  end


class ['g, 'p] old_post_redirect cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method get ((year, old_slug) : 'g) =
      let new_uri = spf "/post/%04d/%s" year old_slug in
        new Http.response_redirect
          ~content:(spf "Permanently moved <a href=\"%s\">%s</a>" new_uri new_uri)
          ~content_type:"text/html"
          ~status:`Moved_permanently new_uri cgi
  end


class ['g, 'p] old_home_redirect cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method get (() : 'g) =
        new Http.response_redirect
          ~content:"Permanently moved to <a href=\"/\">/</a>"
          ~content_type:"text/html"
          ~status:`Moved_permanently "/" cgi
  end


class ['g, 'p] main_view cgi : ['g, 'p] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    method get (_ : 'g) =
      match self#get_session () with
        | Blog_session.Anonymous _ ->
            new Http.response_redirect "/login" cgi
        | Blog_session.Logged u ->
            ctx<--("head", Lazy.force head);
            ctx<--("admin_head", admin_head "Dashboard");
            ctx<--("admin_header", Lazy.force admin_header);
            ctx<--("admin_menu", Lazy.force admin_menu);
            let content = Template.render "admin/dashboard.tmpl" ctx in
              new Http.response ~content ~content_type cgi
  end

