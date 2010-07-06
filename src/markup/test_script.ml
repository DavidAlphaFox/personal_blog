open Markup_types;;

let (|>) x f = f x;;

let trim_white_space s =
  let is_white c =
    match c with
      | ' ' | '\t' | '\r' | '\n' -> true
      | _ -> false in

  let rec trim_left s =
    let len = String.length s in
      if len > 0
      then if is_white s.[0] then trim_left (String.sub s 1 (len - 1)) else s
      else s in

  let rec trim_right s =
    let len = String.length s in
      if len > 0
      then
        if is_white s.[len - 1] then trim_right (String.sub s 0 (len - 1))
        else s
      else s in

    s |> trim_left |> trim_right
;;

let test_lexer_parser n str expected =
  Printf.printf "\n===============================================\n";
  try
    let res = Markup.parse_markup str in
      if res = expected then
        Printf.printf "Test %d PASSED\n%!" n
      else
        failwith (Printf.sprintf "Test %d FAILED\n%!" n);
  with
    | Parsing.Parse_error ->
        failwith (Printf.sprintf "Test %d FAILED FOR Parsing.Parse_error\n%!" n);
;;

(* TEST 1 *)
test_lexer_parser 1 "**Ciao bau miao**
Seconda linea
//questo va in corsivo//, mentre questo no!

cippirimerlo!\n" [Normal [Bold "Ciao bau miao";
                          Text "\nSeconda linea\n";
                          Emph "questo va in corsivo";
                          Text ", mentre questo no!"];
                  Normal [Text "cippirimerlo!"]
                 ];;

(* TEST 2 *)
test_lexer_parser 2 "" [];;

(* TEST 3 *)
test_lexer_parser 3 "\n\n\n" [];;

(* TEST 4 *)
test_lexer_parser 4 "Ciao" [Normal [Text "Ciao"]];;

(* TEST 5 *)
test_lexer_parser 5 "AAAA\n\n\nBBBB" [Normal [Text "AAAA"]; Normal [Text "BBBB"]];;

(* TEST 6 *)
test_lexer_parser 6 "Ciao\n\n\n" [Normal [Text "Ciao"]];;

(* TEST 7 *)
test_lexer_parser 7 "\n\n\nCippirimerlo\n\n\n" [Normal [Text "Cippirimerlo"]];;

(* TEST 8 *)
test_lexer_parser 8 "Questo è **neretto**!\n\n\nE questo \\** invece no\n\nQuesto \\//**non** è corsivo!"
                    [
                      Normal [Text "Questo è "; Bold "neretto"; Text "!"];
                      Normal [Text "E questo "; Text "**"; Text " invece no"];
                      Normal [Text "Questo "; Text "//"; Bold "non"; Text " è corsivo!"]
                    ];;

(* TEST 9 *)
test_lexer_parser 9 "Legale, con **neretto**, ciao\n\nQuesto è un ** input illegale\n\nCiao //corsivo// miao"
                    [
                      Normal [Text "Legale, con "; Bold "neretto"; Text ", ciao"];
                      Syntax_error;
                      Normal [Text "Ciao "; Emph "corsivo"; Text " miao"]
                    ];;

(* TEST 10 *)
test_lexer_parser 10 "Legale, con __sottolineato__, ciao\n\nQuesto è un __ input illegale\n\nCiao //corsivo// miao"
                    [
                      Normal [Text "Legale, con "; Underline "sottolineato"; Text ", ciao"];
                      Syntax_error;
                      Normal [Text "Ciao "; Emph "corsivo"; Text " miao"]
                    ];;

(* TEST 11 *)
test_lexer_parser 11 "Legale, con ''monospaced'', ciao\n\nQuesto è un '' input illegale\n\nCiao //corsivo// miao"
                    [
                      Normal [Text "Legale, con "; Monospaced "monospaced"; Text ", ciao"];
                      Syntax_error;
                      Normal [Text "Ciao "; Emph "corsivo"; Text " miao"]
                    ];;

(* TEST 12 *)
test_lexer_parser 12
  "\nPrimo paragrafo\n\n   {{{html   \nParagrafo preformattato\n\nSeconda riga\n }}}\n\nCiao"
  [
    Normal [Text "Primo paragrafo"];
    Pre ("Paragrafo preformattato\n\nSeconda riga", Html_pre);
    Normal [Text "Ciao"]
  ];;

(* TEST 13 *)
test_lexer_parser 13
  "\nPrimo paragrafo\n\n{{{\nParagrafo preformattato\n\nSeconda riga\n}}}\n\nCiao"
  [
    Normal [Text "Primo paragrafo"];
    Pre ("Paragrafo preformattato\n\nSeconda riga", Normal_pre);
    Normal [Text "Ciao"]
  ];;

(* TEST 14 *)
test_lexer_parser 14
  "\nPrimo paragrafo\n\n{{{\nParagrafo preformattato\n\nSeconda riga\n\n\nCiao"
  [
    Normal [Text "Primo paragrafo"];
    Pre ("Paragrafo preformattato\n\nSeconda riga\n\n\nCiao", Normal_pre)
  ];;

(* TEST 15 *)
test_lexer_parser 15
                 "Go here: http://www.donadeo.net to see what a blog!"
  [Normal [ Text "Go here: http:"; Text "//www.donadeo.net to see what a blog!" ]];;

(* TEST 16 *)
test_lexer_parser 16
                 "Go here: http:\\//www.donadeo.net to see what a blog!"
  [Normal [ Text "Go here: http:"; Text "//"; Text "www.donadeo.net to see what a blog!" ]];;

(* TEST 17 *)
test_lexer_parser 17
                 "Go [[http://www.donadeo.net|here]] to see what a blog!"
  [Normal [ Text "Go "; Link ("http://www.donadeo.net", Some "here"); Text " to see what a blog!"]];;

(* TEST 18 *)
test_lexer_parser 18
                 "Go [[http://www.donadeo.net]] to see what a blog!"
  [Normal [ Text "Go "; Link ("http://www.donadeo.net", None); Text " to see what a blog!"]];;

(* TEST 19 *)
test_lexer_parser 19
  "Ciao **neretto**\n\n{{{\npreformattato\n}}}\n"
  [ Normal [Text "Ciao "; Bold "neretto"];
    Pre ("preformattato", Normal_pre) ]

