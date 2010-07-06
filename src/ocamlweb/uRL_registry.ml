module Make
    (Types : Signatures.TYPES) =
  struct
    open Types

    let (|>) x f = f x

    let empty_registry = []

    let mem registry selector =
      List.mem_assoc selector registry

    let add selector reaction registry =
      registry @ [ (selector, reaction) ]

    let remove registry selector =
      List.remove_assoc selector registry

    let regexp_mem registry regexp =
      List.fold_left
      (
        fun found (sel, react) ->
          if found then true else begin
            match sel with
              | RegExp (str, _) -> if str = regexp then true else false
              | Predicate _ -> found
          end
      )
      false
      registry

    let regexp_add  selector reaction registry =
      let compiled = Pcre.regexp selector in
        registry @ [ (RegExp (selector, compiled), reaction) ]

    let regexp_remove registry selector =
      List.fold_left
      (
        fun new_reg ((sel, react) as el) ->
          match sel with
            | RegExp (str, _) -> if str = selector then new_reg else el::new_reg
            | Predicate _ -> el::new_reg
      )
      []
      registry |> List.rev

    let register_regexp_service regexp service get_type post_type registry =
      let registry : ('a, 'b, 'c, 'd) url_registry = Obj.magic registry in
      let registry =
        if regexp_mem registry regexp then
          regexp_remove registry regexp
        else registry in
        Obj.magic (regexp_add regexp (View (service, get_type, post_type)) registry)

    let register_predicate_service predicate service get_type post_type registry =
      let registry : ('a, 'b, 'c, 'd) url_registry = Obj.magic registry in
        Obj.magic (registry @ [ ( Predicate predicate, (View (service, get_type, post_type))) ])

    class ['g, 'p] view_base
        (cgi : Netcgi.cgi_activation) =
      object (self)
        method get_session () : session_data = raise Types.Session_not_available
        method set_session (s : Types.session_data) : unit = raise Types.Session_not_available

        method post (par : 'p) : Signatures.http_response =
          failwith "Unexpected POST"
      end

    class ['g, 'p] static_redirect new_uri cgi : ['g, 'p] Types.service =
      object (self)
        inherit ['g, 'p] view_base cgi

        method get (unit : 'g) =
          new Http.response_redirect
            ~content:(Printf.sprintf "Permanently moved <a href=\"%s\">%s</a>" new_uri new_uri)
            ~content_type:"text/html"
            ~status:`Moved_permanently new_uri cgi
      end

    let register_static_redirect regexp new_url registry =
      let registry : ('a, 'b, 'c, 'd) url_registry = Obj.magic registry in
      let registry =
        if regexp_mem registry regexp then
          regexp_remove registry regexp
        else registry in
      let service = new static_redirect new_url in
        Obj.magic (regexp_add regexp (View (service, Parameters_map.unit, Parameters_map.unit)) registry)
  end

