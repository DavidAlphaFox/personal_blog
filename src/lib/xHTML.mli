module Element_tags :
  sig
    type cdata = [ `cdata ]
    type pcdata = [ `pcdata ]
    type comment = [ `comment ]
    type blocktext = [ `pre | `hr | `blockquote | `address ]
    type heading = [ `h1 | `h2 | `h3 | `h4 | `h5 | `h6 ]
    type lists = [ `ul | `ol | `dl ]
    type block = [ `p | heading | `div | lists |
                       blocktext | `fieldset | `table ]
    type misc_inline = [ `ins | `del | `script ]
    type misc = [ `noscript | misc_inline ]
    type _Block = [ block | `form | misc | comment ]
    type special_pre = [ `br | `span | `bdo | `map ]
    type special = [ special_pre | `e_object | `img ]
    type fontstyle = [ `tt | `i | `b | `big | `small ]
    type phrase = [ `em | `strong | `dfn | `code | `q | `samp | `kbd |
                    `var | `cite | `abbr | `acronym | `sub | `sup ]
    type inline_forms = [ `input | `select | `textarea | `label | `button ]
    type inline = [ `a | special | fontstyle | phrase | inline_forms ]
    type _Flow = [ cdata | pcdata | block | `form | inline | misc | comment ]
    type _Inline = [ cdata | pcdata | inline | misc_inline | comment ]
    type pre_content = [ cdata | pcdata | `a | fontstyle | phrase | special_pre |
                          misc_inline | inline_forms ]
    type a_content = [ cdata | pcdata | special | fontstyle | phrase |
                       inline_forms | misc_inline ]
    type form_content = [ block | misc ]
    type button_content = [ cdata | pcdata | `p | heading | `div | lists |
                            blocktext | `table | special | fontstyle |
                            phrase | misc ]
    type cellhalign = [ `align | `char | `charoff ]
    type cellvalign = [ `valign ]
  end

module Attribute_tags :
  sig
    type coreattrs = [ `id | `a_class | `style | `title ]
    type i18n = [ `lang | `xml_lang | `dir ]
    type events = [ `onclick | `ondblclick | `onmousedown | `onmouseup |
                        `onmouseover | `onmousemove | `onmouseout |
                        `onkeypress | `onkeydown | `onkeyup ]
    type attrs = [ coreattrs | i18n | events ]
    type head_misc = [ `script | `style | `meta | `link | `e_object ]
    type focus = [ `accesskey | `tabindex | `onfocus | `onblur ]
  end

open Element_tags
open Attribute_tags

type (+'tag) attrib
type (+'safe, +'tag) element

type id = string

type input_type =
  [
    | `text
    | `password
    | `checkbox
    | `radio
    | `submit
    | `reset
    | `file
    | `hidden
    | `image
    | `button
  ]

type direction =
  [
    | `ltr
    | `rtl
  ]

type button_type =
  [
    | `button
    | `submit
    | `reset
  ]

type param_value_type =
  [
    | `data
    | `ref
    | `p_object
  ]

type form_method =
  [
    | `get
    | `post
  ]

type link_type =
  [
    | `alternate
    | `stylesheet
    | `start
    | `next
    | `prev
    | `contents
    | `index
    | `glossary
    | `copyright
    | `chapter
    | `section
    | `subsection
    | `appendix
    | `help
    | `bookmark
    | `nofollow
  ]

type media_desc =
  [
    | `screen
    | `tty
    | `tv
    | `projection
    | `handheld
    | `print
    | `braille
    | `aural
    | `all
  ]

type shape =
  [
    | `rect
    | `circle
    | `poly
    | `default
  ]

type length =
  [
    | `px of int
    | `per of int
  ]

type multi_length =
  [
    | length
    | `rel of int
  ]

type table_frame =
  [
    | `void
    | `above
    | `below
    | `hsides
    | `lhs
    | `rhs
    | `vsides
    | `box
    | `border
  ]

type table_rules =
  [
    | `none
    | `groups
    | `rows
    | `cols
    | `all
  ]

type halign =
  [
    | `left
    | `center
    | `right
    | `justify
    | `char
  ]

type valign =
  [
    | `top
    | `middle
    | `bottom
    | `baseline
  ]

type cell_scope =
  [
    | `row
    | `col
    | `rowgroup
    | `colgroup
  ]

(* TODO
 *
 * CONTROLLARE tutti i tipi esplicitamente "string"
 *)

val pc : string -> ([> `safe ], [> `pcdata ]) element
val pc_unsafe : string -> ([> `unsafe ], [> `pcdata ]) element
val cd : ?prefix:string -> ?suffix:string -> string -> ([> `safe ], [> `cdata ]) element
val comment : ?line:bool -> string -> ([> `safe ], [> `comment ]) element

(* XHTML 1.0 Strict elements *)
val html : ?a:[< i18n | `id ] attrib list ->
  ('safety, [< `head ]) element ->
  ('safety, [< `body ]) element ->
  ('safety, [> `html ]) element

val head : ?a:[< i18n | `id | `profile ] attrib list ->
  ([< `safe ], [< `title ]) element ->
  ('safety, [< head_misc | `base | comment ]) element list ->
  ('safety, [> `head ]) element

val title : ?a:[< i18n | `id ] attrib list ->
  string ->
  ([> `safe ], [> `title ]) element

val base : ?id:id ->
  href:string ->
  ([> `safe ], [> `base ]) element

val meta : ?a:[< i18n | `id | `http_equiv | `name | `scheme ] attrib list ->
  content:string ->
  ([> `safe ], [> `meta ]) element

val link : [< attrs | `charset | `href | `hreflang | `a_type | `rel | `rev | `media] attrib list ->
  ([> `safe ], [> `link ]) element

val style : ?a:[< i18n | `id | `media | `title] attrib list ->
  a_type:string ->
  ('safety, [< pcdata | cdata ]) element ->
  ('safety, [> `style ]) element

val script : ?a:[< `id | `charset | `src | `defer ] attrib list ->
  a_type:string ->
  ('safety, [< pcdata | cdata ]) element list ->
  ('safety, [> `script ]) element

val noscript : ?a:[< attrs ] attrib list ->
  ('safety, [< _Block ]) element list ->
  ('safety, [> `noscript ]) element

val body : ?a:[< attrs | `onload | `onunload ] attrib list ->
  ('safety, [< _Block ]) element list ->
  ('safety, [> `body ]) element

val div : ?a:[< attrs ] attrib list ->
  ('safety, [< _Flow ]) element list ->
  ('safety, [> `div ]) element

val p : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `p ]) element

val h1 : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `h1 ]) element

val h2 : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `h2 ]) element

val h3 : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `h3 ]) element

val h4 : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `h4 ]) element

val h5 : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `h5 ]) element

val h6 : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `h6 ]) element

val ul : ?a:[< attrs ] attrib list ->
  ('safety, [< `li ]) element ->
  ('safety, [< `li ]) element list ->
  ('safety, [> `ul ]) element

val ol : ?a:[< attrs ] attrib list ->
  ('safety, [< `li ]) element ->
  ('safety, [< `li ]) element list ->
  ('safety, [> `ol ]) element

val li : ?a:[< attrs ] attrib list ->
  ('safety, [< _Flow ]) element list ->
  ('safety, [> `li ]) element

val dl : ?a:[< attrs ] attrib list ->
  ('safety, [< `dt | `dd ]) element ->
  ('safety, [< `dt | `dd ]) element list ->
  ('safety, [> `dl ]) element

val dt : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `dt ]) element

val dd : ?a:[< attrs ] attrib list ->
  ('safety, [< _Flow ]) element list ->
  ('safety, [> `dd ]) element

val address : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `address ]) element

val hr : [< attrs ] attrib list ->
  ([> `safe ], [> `hr ]) element

val pre : ?a:[< attrs ] attrib list ->
  ('safety, [< pre_content ]) element list ->
  ('safety, [> `pre ]) element

val blockquote : ?a:[< attrs | `cite ] attrib list ->
  ('safety, [< _Block ]) element list ->
  ('safety, [> `blockquote ]) element

val ins : ?a:[< attrs | `cite | `datetime ] attrib list ->
  ('safety, [< _Flow ]) element list ->
  ('safety, [> `ins ]) element

val del : ?a:[< attrs | `cite | `datetime ] attrib list ->
  ('safety, [< _Flow ]) element list ->
  ('safety, [> `del ]) element

val a : ?a:[< attrs | focus | `charset | `a_type | `name | `href |
            `hreflang | `rel | `rev | `shape | `coords ] attrib list ->
  ('safety, [< a_content ]) element list ->
  ('safety, [> `a ]) element

val span : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `span ]) element

val bdo : ?a:[< coreattrs | events | `lang | `xml_lang ] attrib list ->
  dir:direction ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `bdo ]) element

val br : [< coreattrs ] attrib list ->
  ([> `safe ], [> `br ]) element

val em : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `em ]) element

val strong : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `strong ]) element

val dfn : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `dfn ]) element

val code : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `code ]) element

val samp : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `samp ]) element

val kbd : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `kbd ]) element

val var : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `var ]) element

val cite : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `site ]) element

val abbr : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `abbr ]) element

val acronym : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `acronym ]) element

val q : ?a:[< attrs | `cite ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `q ]) element

val sub : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `sub ]) element

val sup : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `sup ]) element

val tt : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `tt ]) element

val i : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `i ]) element

val b : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `b ]) element

val big : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `big ]) element

val small : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `small ]) element

val e_object :  ?a:[< attrs | `declare | `classid | `codebase | `data |
                    `a_type | `codetype | `archive | `standby | `height |
                    `width | `usemap | `name | `tabindex ] attrib list ->
  ('safety, [< cdata | pcdata | `param | block | `form | inline | misc | comment ]) element list ->
  ('safety, [> `e_object ]) element

val param : [< `id | `name | `value | `a_type | `valuetype ] attrib list ->
  ([> `safe ], [> `param ]) element

val img : ?a:[< attrs | `longdesc | `height | `width | `usemap | `ismap ] attrib list ->
  src:string ->
  alt:string ->
  unit ->
  ([> `safe ], [> `img ]) element

val map : id:id ->
  ?a:[< i18n | events | `a_class | `style | `title | `name ] attrib list ->
  ('safety, [< _Block | `area ]) element ->
  ('safety, [< _Block | `area ]) element list ->
  ('safety, [> `map ]) element

val area : ?a:[< attrs | focus | `shape | `coords | `href | `nohref ] attrib list ->
  alt:string ->
  unit ->
  ([> `safe ], [> `area ]) element

val form : ?a:[< attrs | `a_method | `enctype | `onsubmit | `onreset |
               `accept | `accept_charset ] attrib list ->
  action:string ->
  ('safety, [< form_content | comment ]) element list ->
  ('safety, [> `form ]) element

val label : ?a:[< attrs | `a_for | `accesskey | `onfocus | `onblur ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `label ]) element

val input : ?a:[< attrs | focus | `name | `value | `checked | `disabled |
                `readonly | `size | `maxlength | `src | `alt | `usemap |
                `onselect | `onchange | `accept ] attrib list ->
  a_type:input_type ->
  unit ->
  ([> `safe ], [> `input ]) element

val select : ?a:[< attrs | `name | `size | `multiple | `disabled |
                 `tabindex | `onfocus | `onblur | `onchange ] attrib list ->
  ([< `safe ], [ `optgroup | `option ]) element ->
  ([< `safe ], [ `optgroup | `option ]) element list ->
  ([> `safe ], [> `select ]) element

val optgroup : ?a:[< attrs | `disabled ] attrib list ->
  label:string ->
  ([< `safe ], [< `option ]) element ->
  ([< `safe ], [< `option ]) element list ->
  ([> `safe ], [> `optgroup ]) element

val option : ?a:[< attrs | `select | `disabled | `label | `value ] attrib list ->
  string ->
  ([> `safe ], [> `option ]) element

val textarea : ?a:[< attrs | focus | `name | `disabled | `readonly |
                   `onselect | `onchange ] attrib list ->
  rows:int ->
  cols:int ->
  string ->
  ([> `safe ], [> `textarea ]) element

val fieldset : ?a:[< attrs ] attrib list ->
  ('safety, [< cdata | pcdata | `legend | block | `form | inline | misc ]) element list ->
  ('safety, [> `fieldset ]) element

val legend : ?a:[< attrs | `accesskey ] attrib list ->
  ('safety, [< _Inline ]) element ->
  ('safety, [> `legend ]) element

val button : ?a:[< attrs | focus | `name | `value | `disabled ] attrib list ->
  ?a_type:button_type ->
  ('safety, [< button_content ]) element list ->
  ('safety, [> `button ]) element

val table_complete : ?a:[< attrs | `summary | `width | `border | `frame |
                         `rules | `cellspacing | `cellpadding ] attrib list ->
  ?caption:(('safety, [< `caption ]) element) ->
  ?cols:[< `Columns of (('safety, [< `col ]) element list)
         | `Colgroups of (('safety, [< `colgroup ]) element list) ] ->
  ?thead:('safety, [< `thead ]) element ->
  ?tfoot:('safety, [< `tfoot ]) element ->
  [< `Tbody of (('safety, [< `tbody ]) element * ('safety, [< `tbody ]) element list)
   | `Tr    of (('safety, [< `tr ]) element *    ('safety, [< `tr ]) element list) ] ->
  ('safety, [> `table ]) element

val table : ?a:[< attrs | `summary | `width | `border | `frame |
                `rules | `cellspacing | `cellpadding ] attrib list ->
  ?caption:(('safety, [< `caption ]) element) ->
  ?thead:('safety, [< `thead ]) element ->
  ?tfoot:('safety, [< `tfoot ]) element ->
  ('safety, [< `tr ]) element list ->
  ('safety, [> `table ]) element

val caption : ?a:[< attrs ] attrib list ->
  ('safety, [< _Inline ]) element list ->
  ('safety, [> `caption ]) element

val thead : ?a:[< attrs | cellhalign | cellvalign ] attrib list ->
  ('safety, [< `tr ]) element ->
  ('safety, [< `tr ]) element list ->
  ('safety, [> `thead ]) element

val tfoot : ?a:[< attrs | cellhalign | cellvalign ] attrib list ->
  ('safety, [< `tr ]) element ->
  ('safety, [< `tr ]) element list ->
  ('safety, [> `tfoot ]) element

val tbody : ?a:[< attrs | cellhalign | cellvalign ] attrib list ->
  ('safety, [< `tr ]) element ->
  ('safety, [< `tr ]) element list ->
  ('safety, [> `tbody ]) element

val colgroup : ?a:[< attrs | `span | `col_width | cellhalign | cellvalign ] attrib list ->
  ([< `safe ], [< `col ]) element list ->
  ([> `safe ], [> `colgroup ]) element

val colgroups : ([< `safe ], [< `colgroup ]) element list ->
  [ `Colgroups of (([> `safe ], [< `colgroup ]) element list) ]

val col : [< attrs | `span | `col_width | cellhalign | cellvalign ] attrib list ->
  ([> `safe ], [> `col ]) element

val cols : ([< `safe ], [< `col ]) element list ->
  [ `Columns of (([> `safe ], [> `col ]) element list) ]

val tr : ?a:[< attrs | cellhalign | cellvalign ] attrib list ->
  ('safety, [< `th | `td]) element ->
  ('safety, [< `th | `td ]) element list ->
  ('safety, [> `tr ]) element

val th : ?a:[< attrs | `abbr | `axis | `headers | `scope | `rowspan
             | `colspan | cellhalign | cellvalign ] attrib list ->
  ('safety, [< _Flow ]) element list ->
  ('safety, [> `th ]) element

val td : ?a:[< attrs | `abbr | `axis | `headers | `scope | `rowspan
             | `colspan | cellhalign | cellvalign ] attrib list ->
  ('safety, [< _Flow ]) element list ->
  ('safety, [> `td ]) element


(* Attributes *)
val a_lang : string -> [> `lang ] attrib
val a_xml_lang : string -> [> `xml_lang ] attrib
val a_dir : direction -> [> `dir ] attrib

val a_id : id -> [> `id ] attrib
val a_class : string -> [> `a_class ] attrib
val a_style : string -> [> `style ] attrib
val a_title : string -> [> `title ] attrib

val a_onclick : string -> [> `onclick ] attrib
val a_ondblclick : string -> [> `ondblclick ] attrib
val a_onmousedown : string -> [> `onmousedown ] attrib
val a_onmouseup : string -> [> `onmouseup ] attrib
val a_onmouseover : string -> [> `onmouseover ] attrib
val a_onmousemove : string -> [> `onmousemove ] attrib
val a_onmouseout : string -> [> `onmouseout ] attrib
val a_onkeypress : string -> [> `onkeypress ] attrib
val a_onkeydown : string -> [> `onkeydown ] attrib
val a_onkeyup : string -> [> `onkeyup ] attrib

val a_accesskey : char -> [> `accesskey ] attrib
val a_tabindex : int -> [> `tabindex ] attrib
val a_onfocus : string -> [> `onfocus ] attrib
val a_onblur : string -> [> `onblur ] attrib

val a_profile : string -> [> `profile ] attrib

val a_http_equiv : string -> [> `http_equiv ] attrib
val a_name : string -> [> `name ] attrib
val a_scheme : string -> [> `scheme ] attrib

val a_charset : Netconversion.encoding -> [> `charset ] attrib
val a_href : string -> [> `href ] attrib
val a_hreflang : string -> [> `hreflang ] attrib
val a_type : string -> [> `a_type ] attrib
val a_rel : link_type list -> [> `rel ] attrib
val a_rev : link_type list -> [> `rev ] attrib
val a_media : media_desc list -> [> `media ] attrib

val a_src : string -> [> `src ] attrib
val a_defer : [> `defer ] attrib

val a_onload : string -> [> `onload ] attrib
val a_onunload  : string -> [> `onunload ] attrib

val a_cite : string -> [> `cite ] attrib
val a_datetime : string -> [> `datetime ] attrib

val a_shape : shape -> [> `shape ] attrib
val a_coords : length list -> [> `coords ] attrib

val a_declare : [> `declare ] attrib
val a_classid : string -> [> `classid ] attrib
val a_codebase : string -> [> `codebase ] attrib
val a_data : string -> [> `data ] attrib
val a_codetype : string -> [> `codetype ] attrib
val a_archive : string list -> [> `archive ] attrib
val a_standby : string -> [> `standby ] attrib
val a_height : length -> [> `height ] attrib
val a_width : length -> [> `width ] attrib
val a_usemap : string -> [> `usemap ] attrib

val a_value : string -> [> `value ] attrib
val a_valuetype : param_value_type -> [> `valuetype ] attrib

val a_longdesc : string -> [> `longdesc ] attrib
val a_ismap : [> `ismap ] attrib

val a_nohref : [> `nohref ] attrib

val a_method : form_method -> [> `a_method ] attrib
val a_enctype : string -> [> `enctype ] attrib
val a_onsubmit : string -> [> `onsubmit ] attrib
val a_onreset : string -> [> `onreset ] attrib
val a_accept : string list -> [> `accept ] attrib
val a_accept_charset : Netconversion.encoding list -> [> `accept_charset ] attrib

val a_for : id -> [> `a_for ] attrib

val a_checked : [> `checked ] attrib
val a_disabled : [> `disabled ] attrib
val a_readonly : [> `readonly ] attrib
val a_size : int -> [> `size ] attrib
val a_maxlength : int -> [> `maxlength ] attrib
val a_alt : string -> [> `alt ] attrib
val a_onselect : string -> [> `onselect ] attrib
val a_onchange : string -> [> `onchange ] attrib

val a_multiple : [> `multiple ] attrib

val a_label : string -> [> `label ] attrib

val a_summary : string -> [> `summary ] attrib
val a_border : int -> [> `border ] attrib
val a_frame : table_frame -> [> `frame ] attrib
val a_rules : table_rules -> [> `rules ] attrib
val a_cellspacing : length -> [> `cellspacing ] attrib
val a_cellpadding : length -> [> `cellpadding ] attrib

val a_span : int -> [> `span ] attrib
val a_col_width : multi_length -> [> `col_width ] attrib
val a_align : halign -> [> `align ] attrib
val a_char : char -> [> `char ] attrib
val a_charoff : length -> [> `charoff ] attrib
val a_valign : valign -> [> `valign ] attrib

val a_abbr : string -> [> `abbr ] attrib
val a_axis : string -> [> `axis ] attrib
val a_headers : id list -> [> `headers ] attrib
val a_scope : cell_scope -> [> `scope ] attrib
val a_rowspan : int -> [> `rowspan ] attrib
val a_colspan : int -> [> `colspan ] attrib

val unsafe_string_of_element : ?in_enc:Netconversion.encoding ->
                               ?out_enc:Netconversion.encoding ->
                               ('safety, 'tag) element -> string

val string_of_element : ?in_enc:Netconversion.encoding ->
                        ?out_enc:Netconversion.encoding ->
                        ([< `safe ], 'tag) element -> string

