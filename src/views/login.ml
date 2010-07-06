open BatStd
open CamlTemplate.Model
open Utilities
open Data_mapping
open Config
open Common_fragments

let (<--) = Template.Context.(<--)

let head =
  let module X = XHTML in
    Tstr (
      List.map X.string_of_element [
          X.script ~a_type:"text/javascript" ~a:[X.a_src "/static/js/admin.js"] [];
          X.script ~a_type:"text/javascript" [
              X.cd ~prefix:"//" "
                  $(document).ready(
                          function() {
                              Admin.common.init_eventi_gui();
                              Admin.login_page.init_eventi_gui();
                          }
                      );"
          ]
      ] |> String.concat "\n"
    )

let validate_login_form email password : Data_mapping.user option * string option =
  if email = "" then begin
    None, Some "<strong>ERROR</strong>: Please insert your email address."
  end
  else if not (Pcre.pmatch ~rex:email_regexp email) then
    None, Some "<strong>ERROR</strong>: Please type a valid email address."
  else if password = "" then
    None, Some "<strong>ERROR</strong>: A password is required."
  else begin
    let err_msg = Some "<strong>ERROR</strong>: Invalid email/password. Try again, please!" in
      try
        let u = Db.select_user email in
        let sha1 = compute_sha1 password in
          if u.passwd = sha1
          then Some u, None
          else None, err_msg
      with Not_found -> None, err_msg
  end

class ['get_param, 'post_param] login
    (cgi : Netcgi.cgi_activation) : ['get_param, 'post_param] Types.service =
  object (self)
    inherit ['g, 'p] view_base cgi

    initializer
      ctx<--("head", head);
      ctx<--("admin_head", admin_head "Login");

    method private display_login () =
      let content = Template.render "admin/login.tmpl" ctx in
        new Http.response ~content ~content_type cgi

    method get_session () = session
    method set_session s = session <- s
    method get (action : 'get_param) =
      let action = opt_def "" action in
        if action = "logout" then
          ctx<--("logout_message", Tstr (message_box "You are now logged out."));
        session <- Blog_session.Anonymous ([], false);
        self#display_login ()

    method post ((email, password) : 'post_param) =
      let user, error_message = validate_login_form email password in
        match error_message with
          | None -> begin
              session <- Blog_session.Logged !!user;
                new Http.response_redirect "/admin" cgi
            end
          | Some err -> begin
              ctx<--("error_message", Tstr (error_box err));
              self#display_login ()
            end
  end

