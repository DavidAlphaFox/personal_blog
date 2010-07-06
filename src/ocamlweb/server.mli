module Make :
  functor (Session_data : Signatures.SESSION_DATA) ->
  functor (Types : Signatures.TYPES
                with type session_data = Session_data.t) ->
  functor (Config_file : Signatures.CONFIG_FILE) ->
  functor (Config : Signatures.SERVER_CONFIG
                with type ('get_param, 'post_param, 'b, 'c) url_registry =
                          ('get_param, 'post_param, 'b, 'c) Types.url_registry) ->
  functor (Session_manager : Signatures.SESSION_MANAGER
                with type session_data = Session_data.t) ->
    sig
      val get_config_filename : unit -> string
      val main : unit -> unit
    end

