#use "topfind";;
#require "netstring";;
#require "str";;
#load "xHTML.cmo";;

open XHTML;;

(* to use tidy:
   tidy -o test_tidy.html -w 120 -i -ascii test.html *)

let t =
  table
    ~thead:(thead (tr ~a:[a_style "font-style: italic;"] (td [pc"animale"]) [td [pc"verso"]]) [])
    [
      tr (td [pc"gatto"]) [td [pc"miao"]];
      tr (td [pc"cane"]) [td [pc"bau"]];
    ]
;;

let h =
  html ~a:[a_id "body"]
    (head ~a:[a_id "head"] (title"Titolo") [
        script ~a:[a_src"/static/js/my_script.js"] ~a_type:"text/javascript" [];
        script ~a_type:"text/javascript" [(cd ~prefix:"// "
"tinyMCE.init({
    theme : \"advanced\",
    mode : \"none\",
    plugins : \"bbcode\",
    theme_advanced_buttons1 : \"bold,italic,underline,undo,redo,link,unlink,image,forecolor,styleselect,removeformat,cleanup,code\",
    theme_advanced_buttons2 : \"\",
    theme_advanced_buttons3 : \"\",
    theme_advanced_toolbar_location : \"bottom\",
    theme_advanced_toolbar_align : \"center\",
    theme_advanced_styles : \"Code=codeStyle;Quote=quoteStyle\",
    content_css : \"css/bbcode.css\",
    entity_encoding : \"raw\",
    add_unload_trigger : false,
    remove_linebreaks : false,
    inline_styles : false,
    convert_fonts_to_spans : false
});")];
        comment "This is a comment!";
      ])
    (body ~a:[a_id "mio_body"] [
        h1 [ pc"Titolo H1" ];
        comment "This is a comment!";
        p  [ pc"Cippa" ];
        p  [ pc"pippo |&|" ];         (* testo renderizzato: una singola ampersand *)
        p  [ pc"pippo |&amp;|" ];     (* testo renderizzato: una singola ampersand *)
        p  [ pc"pippo |&amp;amp;|" ]; (* testo renderizzato: la stringa letterale "&amp;" *)
        p  [ pc"Japanese text: &raquo; 文 書 の"];
        p  [ pc"Japanese text: » &#25991; &#26360; &#12398;"];
        div[ pc "Div:"; a ~a:[a_href"http://www.google.com"] [pc"Google"] ];
        p  [ pc "<b>This is a bold text</b>"];
        p  [ b [pc "This is a bold text"] ];
        form ~action:"http://www.pippo.com/api/" [
            div [ textarea ~rows:25 ~cols:80 "<Type here>"; ]
        ];
        p  [ pc_unsafe "<b>This is a bold text</b>"];
        t;
        hr [];
        p []
     ])
;;

let doc = unsafe_string_of_element ~out_enc:`Enc_usascii h;;
let out = open_out "test.html";;
output_string out doc;;
close_out out;;

