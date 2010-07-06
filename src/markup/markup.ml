open Printf
open Markup_types

let (|>) x f = f x

let is_white c =
  match c with
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false

let rec trim_left s =
  let len = String.length s in
    if len > 0
    then if is_white s.[0] then trim_left (String.sub s 1 (len - 1)) else s
    else s

let rec trim_right s =
  let len = String.length s in
    if len > 0
    then
      if is_white s.[len - 1] then trim_right (String.sub s 0 (len - 1))
      else s
    else s

let trim_white_space s = s |> trim_left |> trim_right

let parse_markup str =
  Markup_lexer.reset_lexer ();
  let str = trim_white_space str in
  let tokens = Markup_lexer.tokens_of_markup str in
    Markup_parser.parse_input tokens |> fst

let xhtml_of_text = function
  | Text s -> XHTML.pc s
  | Emph s -> XHTML.em [XHTML.pc s]
  | Bold s -> XHTML.strong [XHTML.pc s]
  | Monospaced s -> XHTML.code [XHTML.pc s]
  | Underline s ->
      XHTML.em  ~a:[XHTML.a_style "font-style:normal; text-decoration:underline;"]
                [XHTML.pc s]
  | Link (url, content) ->
      let content = match content with None -> url | Some c -> c in
        XHTML.a ~a:[XHTML.a_href url; XHTML.a_rel [`nofollow]] [XHTML.pc content]

let xhtml_of_normal p =
  XHTML.p (List.map xhtml_of_text p)

let xhtml_of_normal_pre content =
  XHTML.pre [ XHTML.pc content ]

let xhtml_of_pre content _type =
  match _type with
    | Normal_pre -> xhtml_of_normal_pre content
    | Html_pre -> XHTML.pc_unsafe content

let xhtml_of_paragraph p =
  match p with
    | Normal p -> xhtml_of_normal p
    | Pre (content, _type) -> xhtml_of_pre content _type
    | Syntax_error ->
        XHTML.p ~a:[XHTML.a_style "color: red;"]
                [XHTML.pc "Syntax error in this paragraph"]

let xhtml_of_safe_paragraph p =
  match p with
    | Normal p -> xhtml_of_normal p
    | Pre (content, Normal_pre) -> xhtml_of_normal_pre content
    | Pre (content, Html_pre) -> failwith "xhtml_of_safe_paragraph: this should never happen!"
    | Syntax_error ->
        XHTML.p ~a:[XHTML.a_style "color: red;"]
                [XHTML.pc "Syntax error in this paragraph"]

let xhtml_of_markup ast =
  XHTML.div (List.map xhtml_of_paragraph ast)

let safe_xhtml_of_markup ast =
  let sanitized =
    List.map (fun p -> match p with Pre (_, Html_pre) -> Normal [Text ""] | _ -> p)
    ast in
  XHTML.div (List.map xhtml_of_safe_paragraph sanitized)

