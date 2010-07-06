open BatMap

exception Param_not_found of string
exception Type_error of string*string*string (* param_name, value_as_str, type_name *)

type ('a, +'lst) type_desc

val unit : (unit, [> `scalar ]) type_desc
val int : string -> (int, [> `scalar ]) type_desc
val float : string -> (float, [> `scalar ]) type_desc
val string : string -> (string, [> `scalar ]) type_desc
val bool : string -> (bool, [> `scalar ]) type_desc
val user_type : string -> (string -> 'a) -> ('a, [> `scalar ]) type_desc

val convert_fail : string -> 'a

val ( ** ): ('a, [< `scalar | `list ]) type_desc -> ('b, [< `scalar | `list]) type_desc ->
            (('a*'b), [> `scalar ]) type_desc
val tpl2: ('a, [< `scalar | `list ]) type_desc -> ('b, [< `scalar | `list ]) type_desc ->
          (('a*'b), [> `scalar ]) type_desc
val tpl3: ('a, [< `scalar | `list ]) type_desc -> ('b, [< `scalar | `list ]) type_desc -> ('c, [< `scalar | `list ]) type_desc ->
          (('a*'b*'c), [> `scalar ]) type_desc
val tpl4 : ('a, [< `scalar | `list ]) type_desc -> ('b, [< `scalar | `list ]) type_desc -> ('c, [< `scalar | `list ]) type_desc -> ('d, [< `scalar | `list ]) type_desc ->
           (('a*'b*'c*'d), [> `scalar ]) type_desc
val tpl5 : ('a, [< `scalar | `list ]) type_desc -> ('b, [< `scalar | `list ]) type_desc -> ('c, [< `scalar | `list ]) type_desc -> ('d, [< `scalar | `list ]) type_desc -> ('e, [< `scalar | `list ]) type_desc ->
           (('a*'b*'c*'d*'e), [> `scalar ]) type_desc
val opt : ('a, [< `scalar ]) type_desc -> ('a option, [> `scalar ]) type_desc
val lst : ('a, [< `scalar ]) type_desc -> ('a list, [> `list ]) type_desc

val collect_params :
  Netcgi.cgi_activation ->
  string StringMap.t * Netcgi.cgi_argument list

val reconstruct_params : string StringMap.t -> ('a, [< `scalar | `list ]) type_desc -> 'a

