TYPE_CONV_PATH "Config"

open BatStd
open Utilities

module Config_file =
  struct
    type cookie_conf =
      {
        max_age : Signatures.cookie_age;
        name : string;
        domain : string;
        path : string;
      } with sexp

    type t =
      {
        netplex_config : string;
        pid_file : string;
        foreground : bool;
        doc_root : string;
        debug : bool;
        debug_port : int;
        template_check_interval : float;
        cookie_conf : cookie_conf;
      } with sexp

    let read_config_file filename =
      let config_str = BatFile.with_file_in filename BatIO.read_all in
        strip config_str |> Sexplib.Sexp.of_string |> t_of_sexp

    let get_netplex_config conf = conf.netplex_config

    let get_pid_filename conf = conf.pid_file

    let get_foreground conf =  conf.foreground
  end

open Config_file

let global_conf : t option ref = ref None

module Template_config =
  struct
    let template_dir () =
      match !global_conf with
        | None -> "/" | Some c -> c.doc_root ^ "templates/"
    let template_check_interval () =
      match !global_conf with
        | None -> 0.0 | Some c -> c.template_check_interval
  end

module Template = Template.Make(Template_config)

let conf () =
  match !global_conf with
    | Some c -> c
    | None -> raise Not_found

let read_config_file name =
  global_conf := Some (read_config_file name)

let doc_root ()                 = (conf ()).doc_root
let template_dir ()             = doc_root () ^ "templates/"
let sqlite3_db_path ()          = doc_root () ^ "blog.db"
let sqlite3_sessions_db_path () = doc_root () ^ "blog_sessions.db"
let debug ()                    = (conf ()).debug
let debug_port ()               = (conf ()).debug_port
let content_type                = "text/html; charset=UTF-8"
let cookie_conf ()              = (conf ()).cookie_conf

