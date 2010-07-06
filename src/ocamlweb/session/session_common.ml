open Signatures

module Make
    (Storage_config : STORAGE_CONFIGURATION)
    (Coockie_conf : COOCKIE_CONFIGURATION)
    (Storage : SESSION_STORAGE)
    (Data : SESSION_DATA) :
      SESSION_MANAGER
        with  type session_storage = Storage.session_storage and
              type session_data = Data.t =
  struct
    type session_storage = Storage.session_storage
    type session_data = Data.t
    type session =
        {
          key : string;
          data : session_data;
          ctime : float;
          mtime : float;
          expire : float option;
          client_ip : string option;
          client_user_agent : string option;
        }

    let store = ref None

    let create_instance () =
      match !store with
        | Some _ -> ()
        | None -> store := Some (Storage.create (Storage_config.connection_string ()))

    let get_store () =
      match !store with
        | Some store -> store
        | None -> failwith "Session manager: the session storage is not available"

    let get_key cgi = "ciao"

    let get_data key =
      let s = Storage.get (get_store ()) key in
        match s with
          | None -> Data.default
          | Some s -> Data.t_of_string s.Storage.data

    let load_session cgi =
      let e = cgi#environment in
      let current_key =
        try Netcgi.Cookie.value (e#cookie (Coockie_conf.name ()))
        with Not_found -> Coockie_conf.key_gen cgi in
      let store = get_store () in
      let session = Storage.get store current_key in
        match session with
          | None ->
            let current_key = Coockie_conf.key_gen cgi in
            let now = Unix.time () in
            let expire =
              match Coockie_conf.max_age () with
                | Session -> None;
                | Max_age s -> Some (now +. (float_of_int s)) in
              {
                key = current_key;
                data = Data.default;
                ctime = now;
                mtime = now;
                expire = expire;
                client_ip = Some e#cgi_remote_addr;
                client_user_agent =
                  try Some (e#input_header_field "User-Agent")
                  with Not_found -> None;
              }
          | Some session ->
              {
                key = session.Storage.key;
                data = Data.t_of_string session.Storage.data;
                ctime = session.Storage.ctime;
                mtime = session.Storage.mtime;
                expire = session.Storage.expire;
                client_ip = session.Storage.client_ip;
                client_user_agent = session.Storage.client_user_agent;
              }

    let store_session cgi session =
      let store = get_store () in
      let now = Unix.time () in
      let expire =
        match Coockie_conf.max_age () with
          | Session -> None;
          | Max_age s -> Some (now +. (float_of_int s)) in
      let key = session.key in
      let session =
        {
          Storage.key = key;
          Storage.data = Data.string_of_t session.data;
          Storage.ctime = session.ctime;
          Storage.mtime = now;
          Storage.expire = expire;
          Storage.client_ip = session.client_ip;
          Storage.client_user_agent = session.client_user_agent;
        } in
        Storage.update store session;
        [ Coockie_conf.make_cookie (Coockie_conf.max_age ()) (Coockie_conf.name ()) key ]
  end

