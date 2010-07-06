open BatStd
open BatMap

let (<--) = StringMap.Infix.(<--)
let logf = Netplex_cenv.logf

let add_map map_to_add map =
  StringMap.fold (fun k v map -> map <-- (k, v)) map_to_add map

module Make
    (Session_data : Signatures.SESSION_DATA)
    (Types : Signatures.TYPES
                with type session_data = Session_data.t)
    (Config_file : Signatures.CONFIG_FILE)
    (Config : Signatures.SERVER_CONFIG
                with type ('get_param, 'post_param, 'b, 'c) url_registry =
                            ('get_param, 'post_param, 'b, 'c) Types.url_registry)
    (Session_manager : Signatures.SESSION_MANAGER
                with type session_data = Session_data.t) =
  struct
    open Types

    exception Loop_break

    let session_mngr_present = lazy (Session_manager.create_instance ())

    let rec request_dispatcher  ?(registry = Config.registry)
                                ?(script_name = None)
                                (cgi : Netcgi.cgi_activation) =
      let () = Lazy.force session_mngr_present in
      let script_name =
        match script_name with
          | Some url -> url
          | None -> if cgi#environment#cgi_script_name <> ""
                    then cgi#environment#cgi_script_name
                    else cgi#environment#cgi_path_info in
      let reaction =
        let found = ref None in
        try
          let () = List.iter
            (
              fun (selector, reaction) ->
                match selector with
                  | RegExp (str, rex) -> begin
                      try
                        let res = Pcre.exec ~rex script_name in
                        let first, last = (Pcre.get_substring_ofs res 0) in
                        let remaining = String.sub script_name last ((String.length script_name) - last) in
                        let names = Pcre.names rex in
                        let map = Array.fold_left
                          (
                            fun map name ->
                              try
                                let value = Pcre.get_named_substring rex name res in
                                  StringMap.add name value map
                              with Not_found -> map
                          )
                          StringMap.empty names in
                          found := Some (reaction, remaining, map);
                          raise Loop_break
                      with
                        | Not_found -> ()
                    end
                  | Predicate p ->
                      match p script_name with
                        | None -> ()
                        | Some (remaining, map) -> (
                            found := Some (reaction, remaining, map);
                            raise Loop_break
                          )
            ) Config.registry in None
        with
          | Loop_break -> !found in

        try
          match reaction with
            | Some (reaction, remaining, url_param_map) -> begin
                match reaction with
                  | View (service, get_type_desc, post_type_desc) -> (
                      let service = service cgi in
                      let request_method = cgi#request_method in
                      let type_desc, handler = match request_method with
                        | `GET -> get_type_desc, service#get
                        | `POST -> post_type_desc, service#post
                        | _ -> failwith "Unsupported request method" in
                      (* Session handling *)
                      let session = Session_manager.load_session cgi in
                      let session_data = session.Session_manager.data in

                      (* Parameters *)
                      let cgi_args, cgi_file = Parameters_map.collect_params cgi in
                      let cgi_args = add_map cgi_args url_param_map in
                      let handler_arg = Parameters_map.reconstruct_params cgi_args type_desc in

                      (* Page handler called *)
                      (try service#set_session session_data
                      with Types.Session_not_available -> ());
                      let response = handler handler_arg in
                      let if_mod_since =
                        let headers = cgi#environment#input_header in
                          try Some (Nethttp.Header.get_if_modified_since headers)
                          with Not_found -> None in
                      let last_mod = response#last_modified in
                        match request_method, if_mod_since, last_mod with
                          | `GET, Some ims, Some lm when lm <= ims ->
                              (new Http.response_not_modified cgi)#render ();
                          | _, _, _ ->
                              let new_session =
                                try { session with Session_manager.data = service#get_session () }
                                with Types.Session_not_available -> session in
                              let cookies = Session_manager.store_session cgi new_session in
                                response#render ~set_cookies:cookies ();
                    )
                  | URL_registry registry ->
                      request_dispatcher ~registry ~script_name:(Some remaining) cgi
              end
            | None -> (* Resource not found *)
                let response = Config.handler_404 cgi in
                  response#render ()
        with
          | Http.Http404 ->
              let response = Config.handler_404 cgi in
                response#render ()
          | Failure "Unsupported request method" ->
              (* This is a workaround, I don't know why Ocamlnet raises this exception *)
              (new Http.response_method_not_allowed cgi)#render ()
          | error -> (* Internal server error *)
              let response = Config.handler_500 error cgi in
                logf `Err "EXCEPTION: %s" (Printexc.to_string error);
                logf `Err "BACKTRACE: %s" (Printexc.get_backtrace ());
                response#render ()

    let request_dispatcher' (cgi : Netcgi1_compat.Netcgi_types.cgi_activation) =
      let cgi' = Netcgi1_compat.Netcgi_types.of_compat_activation cgi in
        request_dispatcher cgi'

    let netplex_dynamic_handler =
       {
         Nethttpd_services.dyn_handler = (fun _ -> request_dispatcher');
         dyn_activation = Nethttpd_services.std_activation `Std_activation_buffered;
         dyn_uri = None;
         dyn_translator = (fun _ -> "");
         dyn_accept_all_conditionals = false;
       }

    let nethttpd_factory () =
      Nethttpd_plex.nethttpd_factory
        ~handlers:[(Config.application_name, netplex_dynamic_handler)] ()

    let netcgi_factory () =
      let buffered_transactional_optype () =
        let buffered _ ch = new Netchannels.buffered_trans_channel ch in
          `Transactional buffered in
      let output_type = buffered_transactional_optype () in
      let netcgi_config = { Netcgi.default_config with
          Netcgi.default_exn_handler = false;
          Netcgi.permitted_http_methods = [ `GET; `POST ];
       } in
        Netcgi_plex.factory
          ~config:netcgi_config
          ~output_type
          (fun _ cgi -> request_dispatcher cgi)

    let usage_message = "usage: " ^ Sys.argv.(0) ^ " [options]
Options are:
  -conf <file>   Read this configuration file
  -pid <file>    Write this PID file
  -fg            Start in the foreground and do not run as daemon
  --config-file  Application level configuration file (MANDATORY)
  -help          Display this list of options
  --help         Display this list of options"

    let get_config_filename () =
      let argv = Array.copy Sys.argv in
        for i = 0 to (Array.length argv - 1)
          do
            if argv.(i) = "-help" || argv.(i) = "--help" then begin
              Printf.eprintf "%s\n%!" usage_message;
              exit 0;
            end
          done;
        try
          let name = ref None in
            for i = 0 to (Array.length argv - 1)
              do
                if argv.(i) = "--config-file"
                then name := Some (argv.(i + 1))
              done;
            match !name with
              | None -> failwith "MISSING_OPTION"
              | Some fn -> fn
        with
          | Failure("MISSING_OPTION")
          | Invalid_argument("index out of bounds") -> begin
              Printf.eprintf "%s\n%!" usage_message;
              exit (-1)
            end

    let main () =
      Printexc.record_backtrace true;
      Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

      let conf = get_config_filename () |> Config_file.read_config_file in
      let config_filename = Config_file.get_netplex_config conf in
      let pidfile = Some (Config_file.get_pid_filename conf) in
      let foreground = Config_file.get_foreground conf in

      let defaults = Netplex_main.create ~config_filename ~pidfile ~foreground () in

      let (opt_list, cmdline_cfg) = Netplex_main.args ~defaults () in

        Arg.parse
          (Arg.align (opt_list @ ["--config-file",
                                  Arg.String (fun s -> ()),
                                  " Application level configuration file (MANDATORY)"]))
          (fun s -> raise (Arg.Bad ("Unknown option: " ^ s)))
          ("usage: " ^ Sys.argv.(0) ^ " [options]\nOptions are:");

        Netplex_main.startup
          (Netplex_mp.mp ())
          Netplex_log.logger_factories
          Netplex_workload.workload_manager_factories
          [ netcgi_factory (); nethttpd_factory () ]
          cmdline_cfg
  end

