exception Session_storage_error of string

type session_storage = unit

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

let create filename = ()
let dispose state_mgr = ()
let get state_mgr key = None
let del state_mgr key = ()
let update state_mgr s = ()

let fold ?(order=No_order) f acc state_mgr = acc
let iter ?(order=No_order) f state_mgr = ()
let dump ?(order=No_order) state_mgr = []

