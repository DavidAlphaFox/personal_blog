{
open Lexing
open Markup_types
open Markup_parser

let debug = false

let pf s =
  if debug then Printf.printf "LEXER: emitting %s\n%!" s
  else ignore s

let buf         = Buffer.create     1024
let add_string  = Buffer.add_string buf
let add_char    = Buffer.add_char   buf
let contents () = Buffer.contents   buf
let reset ()    = Buffer.reset      buf
let flush ()    =
  let s = contents () in
    reset ();
    s

let () = reset ()

let lex_queue = Queue.create ()
let push token = Queue.push token lex_queue
let pop () = Queue.pop lex_queue
let reset_lexer () =
  reset ();
  Queue.clear lex_queue

let string_of_token = function
  | TEXT t -> "TEXT = '" ^ t ^ "'"
  | BLANK_LINE -> "BLANK_LINE"
  | BOLD_MARK -> "BOLD_MARK"
  | EMPH_MARK -> "EMPH_MARK"
  | END_OF_FILE -> "END_OF_FILE"
  | UNDERLINE_MARK -> "UNDERLINE_MARK"
  | MONO_MARK -> "MONO_MARK"
  | LINK (url, content) -> begin
      let content = match content with None -> url | Some c -> c in
        ("LINK TO: '" ^ url ^ "'; TITLE = '" ^ content ^ "'")
      end
  | PREFORMATTED (text, _type) -> begin
      match _type with
        | Normal_pre -> "PREFORMATTED STANDARD: '" ^ text ^ "'"
        | Html_pre -> "PREFORMATTED: HTML: '" ^ text ^ "'"
    end
  | INDENT n -> ("INDENT of " ^ (string_of_int n) ^ " spaces")

let push_mark m =
  let content = flush () in
    if String.length content <> 0 then begin
      push (TEXT content);
    end;
    push m;
    let t = pop () in
      pf (string_of_token t);
      t

let push_text txt = push_mark (TEXT txt)

let push_link url content = push_mark (LINK (url, content))

}

let nl = "\r\n" | "\n" | "\r"
let not_nl = [^ '\n' '\r']
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0' - '9']
let name_char = letter | digit | '.' | '-' | '_' | ':'
let name = 	(letter | '_' | ':') (name_char)*
let	space = (' ' | '\t')
let preformatted_types = "" | "html"
let not_a_pipe = [^ '|']+
let not_square_bracket = [^ ']']+

rule token = parse
  | "**" { push_mark BOLD_MARK }
  | "//" { push_mark EMPH_MARK }
  | "__" { push_mark UNDERLINE_MARK }
  | "''" { push_mark MONO_MARK }

  | "\\**"  { push_text "**"}
  | "\\//"  { push_text "//"}
  | "\\__"  { push_text "__"}
  | "\\''"  { push_text "''"}

  | "[[" (not_a_pipe as url) ("|" (not_square_bracket as text))? "]]"
    { push_link url text }

  | space* "{{{" (preformatted_types? as p_type) space* nl
    {
      let _type = match p_type with "html" -> Html_pre | _ -> Normal_pre in
      let content = flush () in
        if String.length content <> 0 then begin
          push (TEXT content);
          push BLANK_LINE;
        end;
        preformatted _type lexbuf;
        let t = pop () in
          pf (string_of_token t);
          t
    }

  | nl (space+ as indent) not_nl+
    { push_mark (INDENT (String.length indent)) }

  | nl (space* nl)+ { push_mark BLANK_LINE }
  | nl
    {
      add_char '\n';
      token lexbuf
    }
  | eof { push_mark END_OF_FILE }
  | _
    {
      add_char (Lexing.lexeme_char lexbuf 0);
      token lexbuf
    }

and preformatted pre_type = parse
  | nl space* "}}}" space* nl+
    {
      let content = flush () in
        if String.length content <> 0 then begin
          push (PREFORMATTED (content, pre_type));
          push BLANK_LINE
        end
    }
  | nl space* "}}}" space* eof
    {
      let content = flush () in
        if String.length content <> 0 then begin
          push (PREFORMATTED (content, pre_type));
        end
    }
  | eof
    {
      let content = flush () in
        if String.length content <> 0 then begin
          push (PREFORMATTED (content, pre_type));
        end
    }
  | _
    {
      add_char (Lexing.lexeme_char lexbuf 0);
      preformatted pre_type lexbuf
    }

{
let tokens_of_markup str =
  reset_lexer ();
  let lexbuf = Lexing.from_string str in
  let rec aux acc =
    let lexeme = token lexbuf in
      match lexeme with
        | Markup_parser.END_OF_FILE -> Markup_parser.END_OF_FILE :: acc
        | l -> aux (l::acc) in
    aux [] |> List.rev
}

