module Handlers =
  struct
    let handler_404 (cgi : Netcgi.cgi_activation) =
      let content = ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title>404 — Not Found</title>
    <meta content=\"application/xhtml+xml; charset=UTF-8\" http-equiv=\"content-type\" />
  </head>
  <body>
    <h1>404 — Not Found</h1>
    <p>We are sorry, but the page you requested, <strong>" ^ (cgi#url ()) ^ "</strong>, wa not found.</p>
  </body>
</html>") in
        new Http.response
          ~status:`Not_found
          ~content
          ~content_type:"application/xhtml+xml; charset=\"UTF-8\"" cgi

    let handler_500 (error : exn) (cgi : Netcgi.cgi_activation) =
      let exc_string = Printexc.to_string error in
      let backtrace = Printexc.get_backtrace () in
      let content = ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title>500 — Internal Server Error</title>
    <meta content=\"application/xhtml+xml; charset=UTF-8\" http-equiv=\"content-type\" />
  </head>
  <body>
    <h1>500 — Internal Server Error</h1>
    <p>
      <strong>Exception:</strong><br />
      <pre>" ^ exc_string ^ "\n</pre>
    </p>
    <p>
      <strong>Stack backtrace:</strong>
      <pre>" ^ backtrace ^ "\n</pre>
    </p>
  </body>
</html>") in
        new Http.response
          ~status:`Internal_server_error
          ~content
          ~content_type:"application/xhtml+xml; charset=\"UTF-8\"" cgi
  end

module Make_default_server_config
    (Types : Signatures.TYPES) :
      Signatures.SERVER_CONFIG
        with
          type ('get_param, 'post_param, 'b, 'c) url_registry =
               ('get_param, 'post_param, 'b, 'c) Types.url_registry =
  struct
    type ('get_param, 'post_param, 'b, 'c) url_registry =
         ('get_param, 'post_param, 'b, 'c) Types.url_registry

    let handler_404 = Handlers.handler_404
    let handler_500 = Handlers.handler_500
    let application_name = ""
    let registry = []
    let enable_session = true
    let parse_spec = []
  end

module Default_template_config : Signatures.TEMPLATE_CONFIG =
  struct
    let template_dir () = Filename.current_dir_name
    let template_check_interval () = 300.0
  end

