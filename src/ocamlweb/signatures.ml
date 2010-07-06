TYPE_CONV_PATH "Signatures"

open BatMap

class type http_response =
  object
    val mutable content_length : int
    method cgi : Netcgi.cgi_activation
    method private commit : unit -> unit
    method content_length : int
    method private out_channel : Netchannels.trans_out_obj_channel
    method output_string : string -> unit
    method render : ?set_cookies:Netcgi.Cookie.t list -> unit -> unit
    method rollback : unit -> unit
    method other_headers : [  | `Generic of string * string list
                              | `Last_Modified of float  ] list
    method last_modified : float option
  end

module type SESSION_DATA =
  sig
    type t
    val default : t
    val string_of_t : t -> string
    val t_of_string : string -> t
  end

module type TYPES =
  sig
    exception Session_not_available

    type session_data

    class type ['get_param, 'post_param] service =
      object
        method get_session : unit -> session_data
        method set_session : session_data -> unit
        method get : 'get_param -> http_response
        method post :'post_param -> http_response
      end

    type selector =
      | RegExp of string * Pcre.regexp
      | Predicate of (string -> (string * string StringMap.t) option)
          (* None means no match; [Some (u, map)]
             means match and u is the remaining
             part of the URL, map is a map of parameters *)

    type ('get_param, 'post_param, 'b, 'c) url_reaction =
      | View of (Netcgi.cgi_activation -> ('get_param, 'post_param) service) *
                ('get_param, 'b) Parameters_map.type_desc *
                ('post_param, 'c) Parameters_map.type_desc
      | URL_registry of ('get_param, 'post_param, 'b, 'c) url_registry

    and ('get_param, 'post_param, 'b, 'c) url_registry =
          (selector * ('get_param, 'post_param, 'b, 'c) url_reaction) list
  end

module Make_types
    (Session_data : SESSION_DATA) :
      TYPES
        with  type session_data = Session_data.t =
  struct
    exception Session_not_available

    type session_data = Session_data.t

    class type ['get_param, 'post_param] service =
      object
        method get_session : unit -> session_data
        method set_session : session_data -> unit
        method get : 'get_param -> http_response
        method post :'post_param -> http_response
      end

    type selector =
      | RegExp of string * Pcre.regexp
      | Predicate of (string -> (string * string StringMap.t) option)
          (* None means no match; [Some (u, map)]
             means match and u is the remaining
             part of the URL, map is a map of parameters *)

    type ('get_param, 'post_param, 'b, 'c) url_reaction =
      | View of (Netcgi.cgi_activation -> ('get_param, 'post_param) service) *
                ('get_param, 'b) Parameters_map.type_desc *
                ('post_param, 'c) Parameters_map.type_desc
      | URL_registry of ('get_param, 'post_param, 'b, 'c) url_registry

    and ('get_param, 'post_param, 'b, 'c) url_registry =
          (selector * ('get_param, 'post_param, 'b, 'c) url_reaction) list
  end

module type CONFIG_FILE =
  sig
    type t
    val read_config_file : string -> t
    val get_netplex_config : t -> string
    val get_pid_filename : t -> string
    val get_foreground : t -> bool
  end

module type SERVER_CONFIG =
  sig
    type ('get_param, 'post_param, 'b, 'c) url_registry

    val handler_404 : Netcgi.cgi_activation -> http_response
    val handler_500 : exn -> Netcgi.cgi_activation -> http_response
    val application_name : string
    val registry : (unit, unit, 'b, 'c) url_registry
    val enable_session : bool
  end

module type TEMPLATE_CONFIG =
  sig
    val template_dir : unit -> string
    val template_check_interval : unit -> float
  end

module type SESSION_STORAGE =
  sig
    exception Session_storage_error of string

    type session_storage

    type sort_order =
      | No_order        (** No order *)
      | By_ctime        (** Order by creation time, ascending *)
      | By_ctime_desc   (** Order by creation time, descending *)
      | By_mtime        (** Order by modification time, ascending *)
      | By_mtime_desc   (** Order by modification time, descending *)
      | By_expire       (** Order by expire time, ascending *)
      | By_expire_desc  (** Order by expire time, descending *)
    (** Enumeration used in iterations to sort pairs by ctime or mtime. *)

    type session =
        {
          key : string;
          data : string;
          ctime : float;
          mtime : float;
          expire : float option;
          client_ip : string option;
          client_user_agent : string option;
        }

    val create : string -> session_storage
    val dispose : session_storage -> unit
    val get : session_storage -> string -> session option
    val del : session_storage -> string -> unit
    val update : session_storage -> session -> unit
    val fold : ?order:sort_order -> ('a -> session -> 'a) -> 'a -> session_storage -> 'a
    val iter : ?order:sort_order -> (session -> unit) -> session_storage -> unit
    val dump : ?order:sort_order -> session_storage -> session list
  end

module type SESSION_MANAGER =
  sig
    type session_storage
    type session_data
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

    val create_instance : unit -> unit
    val load_session : Netcgi.cgi_activation -> session
    val store_session : Netcgi.cgi_activation -> session -> Netcgi.Cookie.t list
  end

module type STORAGE_CONFIGURATION =
  sig
    val connection_string : unit -> string
  end

type cookie_age =
  | Session
  | Max_age of int with sexp

module type COOCKIE_CONFIGURATION =
  sig
    val max_age : unit -> cookie_age
    val name : unit -> string

    val make_cookie : cookie_age -> string -> string -> Netcgi_common.Cookie.t
    (*                age           name      data       *)

    val key_gen : Netcgi.cgi_activation -> string
  end

