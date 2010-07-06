(** Dummy backend to store sessions
  *
  * This actually does nothing!
  *)

exception Session_storage_error of string
(** NEVER raised *)

type session_storage

type sort_order =
  | No_order
  | By_ctime
  | By_ctime_desc
  | By_mtime
  | By_mtime_desc
  | By_expire
  | By_expire_desc

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
val fold : ?order:sort_order -> ('a -> session -> 'a) -> 'a ->
  session_storage -> 'a
val iter : ?order:sort_order -> (session -> unit) -> session_storage -> unit
val dump : ?order:sort_order -> session_storage -> session list

