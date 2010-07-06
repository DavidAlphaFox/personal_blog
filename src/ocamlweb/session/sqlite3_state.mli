(** SQLite3 backend to store sessions
  *
  * This module provides a simple and effective interface to store sessions into
  * a SQLite3 database. Session are stored as simple key/value pairs, both of
  * which are strings, plus ctime, mtime, expire time and optionally the client
  * IP and user agent informations.
  *)

exception Session_storage_error of string
(** Raised when
  *   - the db schema is incorrect or the db is busy;
  *   - in case of errors in the query issued to SQLite3;
  *   - Raised when keys or values contains ['\000'] characters, which are invalid
  *     for SQLite3.
  *)

type session_storage
(** The type of the connection handler. *)

type sort_order =
  | No_order        (** No order *)
  | By_ctime        (** Order by creation time, ascending *)
  | By_ctime_desc   (** Order by creation time, descending *)
  | By_mtime        (** Order by modification time, ascending *)
  | By_mtime_desc   (** Order by modification time, descending *)
  | By_expire       (** Order by expire time, ascending *)
  | By_expire_desc  (** Order by expire time, descending *)
(** Enumeration used in iterations to sort pairs by ctime, mtime or expire time. *)

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
(** This record represents a session. *)

val create : string -> session_storage
(** [connect "filename"] connects to the SQLite3 DB and
  * creates it if it doesn't exist. *)

val create_table : session_storage -> session_storage
(** [create_table session_storage] creates a table suitable for storing
  * sessions. *)

val dispose : session_storage -> unit
(** Disconnect the database. *)

val get : session_storage -> string -> session option
(** [retrieve handler "key"] returns [Some session] if a binding with "key"
  * exists in the database, or [None] if such binding doesn't exist. *)

val del : session_storage -> string -> unit
(** Removes an entry, given the key. If the key is not present, no errors are
  * issued. *)

val update : session_storage -> session -> unit

val fold : ?order:sort_order -> ('a -> session -> 'a) -> 'a ->
  session_storage -> 'a
(** Fold over the key/value collection in the desired order. [~order] defaults
  * to [No_order]. *)

val iter : ?order:sort_order -> (session -> unit) -> session_storage -> unit
(** Iter over the key/value collection in the desired order. [~order] defaults
  * to [No_order]. *)

val dump : ?order:sort_order -> session_storage -> session list
(** Dump the entire collection of sessions. [~order] defaults to [No_order]. *)

