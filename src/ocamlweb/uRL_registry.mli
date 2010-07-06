module Make :
  functor (Types : Signatures.TYPES) ->
    sig
      val empty_registry : ('a, 'b, 'c, 'd) Types.url_registry

      val register_regexp_service :
        string ->
        (Netcgi.cgi_activation -> ('get, 'post) Types.service) ->
        ('get, 'a) Parameters_map.type_desc ->
        ('post, 'b) Parameters_map.type_desc ->
        ('c, 'd, 'e, 'f) Types.url_registry ->
        ('g, 'h, 'i, 'l) Types.url_registry

      val register_predicate_service :
        (string -> (string * string BatMap.StringMap.t) option) ->
        (Netcgi.cgi_activation -> ('get, 'post) Types.service) ->
        ('get, 'a) Parameters_map.type_desc ->
        ('post, 'b) Parameters_map.type_desc ->
        ('c, 'd, 'e, 'f) Types.url_registry ->
        ('g, 'h, 'i, 'l) Types.url_registry

      val register_static_redirect :
        string -> string ->
        ('c, 'd, 'e, 'f) Types.url_registry ->
        ('g, 'h, 'i, 'l) Types.url_registry
    end

