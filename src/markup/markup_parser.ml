let (|>) x f = f x
let (|-) f g x = g (f x)

type ('a, 'b) parser = 'a -> 'b * 'a

type token =
  | TEXT of string
  | BLANK_LINE
  | BOLD_MARK
  | EMPH_MARK
  | UNDERLINE_MARK
  | MONO_MARK
  | LINK of (string * string option)
  | END_OF_FILE
  | PREFORMATTED of (string * Markup_types.preformatted_type)
  | INDENT of int

exception StopParse of token list
exception ParseError

let parse_error _ = raise ParseError

let ( ||| ) (p1 : ('a, 'b) parser) (p2 : ('a, 'c) parser) : ('a, 'd) parser =
  fun s -> try p1 s with StopParse _ -> p2 s

let ( ++ ) (p1 : ('a, 'b) parser) (p2 : ('a, 'c) parser) : ('a, 'b * 'c) parser =
  fun s ->
    let e1, s = p1 s in
    let e2, s = p2 s in
      (e1, e2), s

let rec star (p : ('a, 'b) parser) s =
  try
    let e, s = p s in
    let es, s = star p s in
      e::es, s
  with StopParse _ ->
    [], s

let optional (p : ('a, 'b) parser) s =
  try
    let res, s = p s in
      [res], s
  with StopParse _ -> [], s

let plus (p : ('a, 'b) parser) =
  p ++ (star p)

let ( >| ) p k i =
  let e, s = p i in
    k e, s

let some p = function
  | h::t when p h -> h, t
  | s -> raise (StopParse s)

let a x = some (( = ) x)
let not_a x = some (( <> ) x)

let trash_all_while (p : ('a, 'b) parser) s =
  let _, s = star p s in
    [], s

let is_link = function LINK _ -> true | _ -> false

let is_text = function TEXT _ -> true | _ -> false

let is_preformatted = function PREFORMATTED (_, _) -> true | _ -> false

let text_from text =
  match text with
    | TEXT t -> t
    | _ -> failwith "text_from: not a TEXT fragment"

let text_from_2 (_, text) =
  match text with
    | TEXT t -> t
    | _ -> failwith "text_from_2: not a TEXT fragment"

let text_from_3 ((_, text), _) =
  match text with
    | TEXT t -> t
    | _ -> failwith "text_from_3: not a TEXT fragment"

let pref_from p =
  match p with
    | PREFORMATTED (content, _type) -> (Markup_types.Pre (content, _type))
    | _ -> failwith "pref_from: not a PREFORMATTED fragment"

let rec parse_input : (token list, Markup_types.paragraph list) parser =
  fun s ->
    (((optional (paragraph ++ (((a BLANK_LINE) ++ (paragraph)) |> star))) ++ (a END_OF_FILE))
      >|
    (fun (opt_pars, eof) ->
      match opt_pars with
        | [] -> []
        | (fst_par, [])::_ -> [ fst_par ]
        | (fst_par, par_list)::_ -> [ fst_par ] @ (List.map (fun (_, p) -> p) par_list)
    )) s

and paragraph =
  fun s ->  try
              (
                (normal_paragraph >| fun x -> Markup_types.Normal x) |||
                (some is_preformatted >| pref_from)
              ) s
            with ParseError ->
              let _, s' = trash_all_while (not_a BLANK_LINE) s in
                Markup_types.Syntax_error, s'

and normal_paragraph =
  fun s -> ((plus text_fragment) >| (fun (t, tl) -> t::tl)) s

and text_fragment : (token list, Markup_types.text) parser =
  fun s -> (bold ||| emph ||| underline ||| monospaced |||
            link ||| text_frag) s

and bold =
  ((a BOLD_MARK ++ some is_text ++ a BOLD_MARK) >|
    (text_from_3 |- (fun t -> Markup_types.Bold t))) |||
  ((a BOLD_MARK) >| parse_error)

and emph =
  fun s ->
  (
    ((a EMPH_MARK ++ some is_text ++ a EMPH_MARK) >|
      (text_from_3 |- (fun t -> Markup_types.Emph t))) |||
    (
      ((a EMPH_MARK) ++ (star text_frag)) >|
      (
        fun (_, t_list) ->
          let str_list = List.map (
              fun t -> match t with
                | Markup_types.Text t -> t
                | _ -> failwith "emph: impossible"
            ) t_list in
          Markup_types.Text ("//" ^ (String.concat "" str_list))
      )
    ) |||
    ((a EMPH_MARK) >| parse_error)
  ) s

and underline =
  ((a UNDERLINE_MARK ++ some is_text ++ a UNDERLINE_MARK) >|
    (text_from_3 |- (fun t -> Markup_types.Underline t))) |||
  ((a UNDERLINE_MARK) >| parse_error)

and monospaced =
  ((a MONO_MARK ++ some is_text ++ a MONO_MARK) >|
    (text_from_3 |- (fun t -> Markup_types.Monospaced t))) |||
  ((a MONO_MARK) >| parse_error)

and link =
  some (is_link) >| (
      function LINK (u, c) -> Markup_types.Link (u, c)
        | _ -> failwith "link: not a LINK fragment"
    )

and text_frag =
  some is_text >| (text_from |- (fun t -> Markup_types.Text t))

