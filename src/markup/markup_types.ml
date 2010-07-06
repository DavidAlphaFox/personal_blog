type preformatted_type =
  | Normal_pre
  | Html_pre

type paragraph =
  | Normal of text list
  | Pre of string * preformatted_type
  | Syntax_error

and text =
  | Text of string
  | Emph of string
  | Bold of string
  | Monospaced of string
  | Underline of string
  | Link of string * string option

