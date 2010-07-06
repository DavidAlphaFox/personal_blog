open BatStd
open Utilities
open Data_mapping


(*******************)
(* OPTIONS FROM DB *)
(*******************)
let blog_title = lazy (
  (Db.select_option "blog_title").value |>
    Netencoding.Html.decode ~in_enc:`Enc_utf8
                            ~out_enc:`Enc_utf8 ~entity_base:`Html ()
)

let tagline = lazy ((Db.select_option "tagline").value)

let my_name = lazy ((Db.select_option "my_name").value)

let my_email_address = lazy ((Db.select_option "my_email_address").value)

let akismet_key = lazy ((Db.select_option "akismet_key").value)

let admin_post_per_page () = (Db.select_option "admin_post_per_page").value |> int_of_string

let smtp_pwd = lazy ((Db.select_option "smtp_pwd").value)

let blog_post_per_page = lazy ((Db.select_option "blog_post_per_page").value |> int_of_string)

let blog_post_in_rss = lazy ((Db.select_option "blog_post_in_rss").value |> int_of_string)

let blog_hostname = lazy ((Db.select_option "blog_hostname").value)

