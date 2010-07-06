open BatStd

let () = Ssl.init ()

let (>>=) = Lwt.(>>=)

let set_unix_event_system = lazy (
  Lwt_equeue.set_event_system ((Netplex_cenv.self_cont ())#event_system)
)

let fst4 (a, b, c, d) = a
let snd4 (a, b, c, d) = b
let trd4 (a, b, c, d) = c
let fth4 (a, b, c, d) = d

let create_message from_header to_header more_headers subject body attachment =
  let attachments =
    match attachment with
      | None -> []
      | Some f -> [ Netsendmail.wrap_attachment
                      ~in_charset:`Enc_utf8
                      ~out_charset:`Enc_utf8
                      ~content_type:("application/octet-stream", [])
                      ~content_disposition:("attachment",
                        ["filename", Filename.basename f |> Mimestring.mk_param])
                      (new Netmime.file_mime_body f) ] in

  let headers, body = Netsendmail.compose ~from_addr:from_header
      ~in_charset:`Enc_utf8 ~out_charset:`Enc_utf8
      ~to_addrs:[to_header] ~subject:subject ~attachments:attachments body in
    List.iter (fun (header, content) -> headers#update_field header content) more_headers;
    let b = Buffer.create 1024 in
      Netmime.write_mime_message (new Netchannels.output_buffer b) (headers, body);
      Buffer.contents b

module type SMTP_CLIENT =
  sig
    exception SMTP_error of int * string list

    type t (* The type of an SMTP client *)

    val create : ?hostname:string -> ?port:int -> unit -> t Lwt.t
    val set_debug_level : t -> int -> t Lwt.t
    val ehlo : ?host:string -> t -> unit Lwt.t
    val starttls : t -> unit Lwt.t
    val login : t -> string -> string -> unit Lwt.t
    val mail : t -> string -> unit Lwt.t
    val rcpt : t -> string -> unit Lwt.t
    val data : t -> string -> unit Lwt.t
    val quit : t -> unit Lwt.t
    val force_close : t -> unit Lwt.t
  end

module SMTP_client : SMTP_CLIENT =
  struct
    exception SMTP_error of int * string list

    type socket = Lwt_unix.file_descr * Lwt_ssl.socket * Lwt_chan.in_channel * Lwt_chan.out_channel

    type t = {
      mutable channel : socket;
      mutable debug_level : int;
    }

    let resolve name =
      try Unix.inet_addr_of_string name
      with Failure _ ->
            let h = Unix.gethostbyname name in
            h.Unix.h_addr_list.(0)

    let socket_connect host port =
      let s = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Lwt_unix.connect s (Unix.ADDR_INET((resolve host), port)) >>=
          fun () -> Lwt.return s

    let crlf_regexp = Str.regexp "\r\n"

    let new_line_regexp = Str.regexp "\\(\r\n\\|\r\\|\n\\)"

    let input_line client : string Lwt.t =
      Lwt_chan.input_line (trd4 client.channel) >>= fun l ->
      Lwt.return (Utilities.strip l)

    let output_string client s : unit Lwt.t =
      Lwt_chan.output_string (fth4 client.channel) s >>= fun () ->
      Lwt_chan.flush (fth4 client.channel)

    let rec read ?(acc = []) client : (int * string list) Lwt.t =
      input_line client     >>= (fun l ->
      if client.debug_level > 0 then Netplex_cenv.logf `Debug "SMTP SERVER: %s" l;
      Lwt.return l)         >>= (fun l ->
      if l.[3] = '-'
      then read ~acc:(l::acc) client
      else begin
        Lwt.return (int_of_string (String.sub l 0 3) , List.rev (l::acc))
      end)

    let handle_reply client : unit Lwt.t =
      read client >>= fun (code, msg) ->
        match code/100 with
          | 2 | 3 -> Lwt.return ()
          | _ -> Lwt.fail (SMTP_error (code, msg))

    let smtp_cmd client cmd =
      if client.debug_level > 0 then Netplex_cenv.logf `Debug "SMTP CLIENT: %s" cmd;
      output_string client cmd      >>= fun () ->
      output_string client "\r\n"   >>= fun () ->
      handle_reply client

    let create ?(hostname = "localhost") ?(port = 25) () =
      let socket_th = socket_connect hostname port in
        socket_th >>= (fun socket ->
            let ssl_socket = Lwt_ssl.plain socket in
              Lwt.return (socket, ssl_socket)) >>= (fun (socket, ssl_socket) ->
          let inc = Lwt_chan.make_in_channel (Lwt_ssl.read ssl_socket) in
          let outc = Lwt_chan.make_out_channel (Lwt_ssl.write ssl_socket) in
          let new_client = { channel = (socket, ssl_socket, inc, outc); debug_level = 0; } in
            (handle_reply new_client)             >>= (fun () ->
            Lwt.return new_client))

    let set_debug_level client n =
      client.debug_level <- n;
      Lwt.return client

    let ehlo ?host client : unit Lwt.t =
      smtp_cmd client ("EHLO " ^ (
        match host with
          | None -> (Unix.gethostbyname (Unix.gethostname ())).Unix.h_name
          | Some s -> s
        ))

    let starttls c =
      let ssl_context = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
        smtp_cmd c "STARTTLS" >>= fun () ->
        Lwt_ssl.ssl_connect (fst4 c.channel) ssl_context >>= fun ssl_sock ->
          let inc = Lwt_chan.make_in_channel (Lwt_ssl.read ssl_sock) in
          let outc = Lwt_chan.make_out_channel (Lwt_ssl.write ssl_sock) in
            Lwt.return (c.channel <- (fst4 c.channel, ssl_sock, inc, outc))

    let login c user password =
      let encoded_login =
        Netencoding.Base64.encode
          (Printf.sprintf "%s\000%s\000%s" user user password) in
        smtp_cmd c ("AUTH PLAIN " ^ encoded_login)

    let mail c addr = smtp_cmd c (Printf.sprintf "MAIL FROM:<%s>" addr)

    let rcpt c addr = smtp_cmd c (Printf.sprintf "RCPT TO:<%s>" addr)

    let data c email_string =
      let lines = email_string |> Str.global_replace new_line_regexp "\r\n" |>
                    Str.split crlf_regexp in

      smtp_cmd c "DATA" >>= fun () -> (
        Lwt_util.fold_left
        (
          fun th line ->
            output_string c ( if String.length line > 0 && line.[0] = '.'
                              then ("." ^ line ^ "\r\n")
                              else line^"\r\n" )
        )
        ()
        lines) >>= fun () ->
        smtp_cmd c "."

    let quit c =
      smtp_cmd c "QUIT" >>= (fun () -> Lwt.return (Lwt_ssl.close (snd4 c.channel)))

    let force_close c =
      Lwt.return (Lwt_ssl.close (snd4 c.channel))
  end

open SMTP_client

let sendmail from_header to_header subject body ?(attachment) ?(more_headers = []) () : unit Lwt.t =
  Lazy.force set_unix_event_system;
  let email_as_string = create_message from_header to_header more_headers subject body attachment in
  let client = create ~hostname:"smtp.gmail.com" ~port:587 () in
    Lwt.catch (fun () ->
      client                                                          >>= fun c ->
      (if Config.debug () then set_debug_level c 1 else Lwt.return c) >>= fun c ->
      ehlo ~host:"localhost" c                                        >>= fun () ->
      starttls c                                                      >>= fun () ->
      ehlo ~host:"localhost" c                                        >>= fun () ->
      login c (snd from_header) (Lazy.force Options.smtp_pwd)         >>= fun () ->
      mail c (snd from_header)                                        >>= fun () ->
      rcpt c "p.donadeo@gmail.com"                                    >>= fun () ->
      data c email_as_string                                          >>= fun () ->
      quit c
    ) (fun exn ->
      match exn with
        | SMTP_error (code, msgs) -> (
            Netplex_cenv.logf `Err "Sendmail.sendmail: SMTP_error, code = %d" code;
            List.iter (fun msg -> Netplex_cenv.logf `Err "    %s" msg) msgs;
            client >>= force_close
          )
        | e -> (
            Netplex_cenv.logf `Err "Sendmail.sendmail: unexpected exception";
            Netplex_cenv.logf `Err "    %s" (Printexc.to_string e);
            client >>= force_close
          )
    )
;;

