exception Http404

let render_header (env : Netcgi.cgi_environment) = function
  | `Last_Modified f ->
      env#set_multiple_output_header_field "Last-Modified" [(Netdate.mk_mail_date f)]
  | `Generic (name, values) ->
      env#set_multiple_output_header_field name values

class response
    ?(content = "")
    ?(status = `Ok)
    ?(content_type = "text/html")
    ?(cache = `No_cache)
    ?(headers = [])
    (cgi : Netcgi.cgi_activation) =
  object (self)
    val mutable content_length = String.length content

    method private out_channel = cgi#out_channel
    method cgi = cgi

    method output_string s =
      let len = String.length s in
        content_length <- content_length + len;
        self#out_channel#output_string s

    method content_length = content_length

    method rollback =
      content_length <- 0;
      self#out_channel#rollback_work

    method render ?(set_cookies : Netcgi.Cookie.t list = []) () =
      if content_length <> 0 then begin
        let env = cgi#environment in
          cgi#set_header ~status ~cache ~content_length ~set_cookies ~content_type ();
          List.iter (fun h -> render_header env h) headers;
          self#output_string content
      end;
        self#out_channel#commit_work ();

    method other_headers : [  | `Generic of string * string list
                              | `Last_Modified of float  ] list = headers

    method last_modified =
      try Some (BatList.find_map (function `Last_Modified f -> Some f | _ -> None) headers)
      with Not_found -> None
  end

class response_redirect
    ?(content = "")
    ?(content_type = "")
    ?(status = `Found)
    (redirect_url : string)
    (cgi : Netcgi.cgi_activation) =
  object (self)
    inherit response ~content ~content_type ~status cgi

    method render ?(set_cookies : Netcgi.Cookie.t list = []) () =
      let redirect_url =
        if redirect_url.[0] = '/'
          then cgi#url ~with_script_name:(`This "") ~with_path_info:(`This redirect_url) ()
          else redirect_url in
      let e = cgi#environment in
        e#set_output_header_fields [];
        e#set_status status;
        e#set_output_header_field "Location" redirect_url;
        if content_length <> 0 then self#output_string content;
        self#out_channel#commit_work ();
  end

class response_not_modified (cgi : Netcgi.cgi_activation) =
  object (self)
    inherit response ~status:`Not_modified cgi

    method render ?(set_cookies : Netcgi.Cookie.t list = []) () =
      let e = cgi#environment in
        e#set_output_header_fields [];
        e#set_status `Not_modified;
        self#out_channel#commit_work ();
  end

class response_method_not_allowed (cgi : Netcgi.cgi_activation) =
  object (self)
    inherit response ~status:`Method_not_allowed cgi

    method render ?(set_cookies : Netcgi.Cookie.t list = []) () =
      let e = cgi#environment in
        e#set_output_header_fields [];
        e#set_status `Method_not_allowed;
        self#out_channel#commit_work ();
  end

