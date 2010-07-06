#use "topfind";;

#directory "markup/";;
#directory "lib/";;
#directory "ocamlweb/";;
#directory "/usr/local/lib/ocaml/3.11.1/camltemplate/";;
#directory "views/";;

#require "batteries";;
#require "sexplib";;
#require "sqlite3";;
#require "netstring";;
#require "cryptokit";;
#require "str";;
#require "netplex";;
#require "netcgi2-plex";;
#require "nethttpd";;
#require "netclient";;

#require "equeue";;
#require "lwt-equeue.ssl";;

#load "ocamlweb/signatures.d.cmo";;
#load "/usr/local/lib/ocaml/3.11.1/camltemplate/camltemplate.cma";;
#load "ocamlweb/template.d.cmo";;
#load "lib/utilities.d.cmo";;
#load "config.d.cmo";;
#load "lib/data_mapping.d.cmo";;
#load "lib/base62.d.cmo";;
#load "lib/db.d.cmo";;

#load "lib/options.d.cmo";;
#load "lib/sendmail.d.cmo";;
#load "lib/blog_template.d.cmo";;
#load "lib/blog_lib.d.cmo";;
#load "lib/xHTML.d.cmo";;
#load "lib/akismet.d.cmo";;

#load "markup/markup_types.d.cmo";;
#load "markup/markup_parser.d.cmo";;
#load "markup/markup_lexer.d.cmo";;
#load "markup/markup.d.cmo";;

#load "ocamlweb/http.d.cmo";;
#load "ocamlweb/defaults.d.cmo";;
#load "ocamlweb/parameters_map.d.cmo";;
#load "ocamlweb/server.d.cmo";;
#load "ocamlweb/session/session_common.d.cmo";;
#load "ocamlweb/session/sqlite3_state.d.cmo";;
#load "ocamlweb/signatures.d.cmo";;
#load "ocamlweb/template.d.cmo";;
#load "ocamlweb/uRL_registry.d.cmo";;

#load "views/common_fragments.d.cmo";;
#load "views/post_list.d.cmo";;
#load "views/single_post.d.cmo";;
#load "views/multi_post.d.cmo";;
#load "views/pages.d.cmo";;
#load "views/post_edit.d.cmo";;
#load "views/dashboard.d.cmo";;
#load "views/login.d.cmo";;

Config.read_config_file "/home/paolo/Documenti/Home_page/blog.git/src/blog_devel.conf";;

open Data_mapping;;
open Db;;

