let (|>) x f = f x

exception Invalid_datetime of string
exception Empty_table
exception Malformed_comment of string

module Element_tags =
  struct
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

module Attribute_tags =
  struct
    type coreattrs = [ `id | `a_class | `style | `title ]
    type i18n = [ `lang | `xml_lang | `dir ]
    type events = [ `onclick | `ondblclick | `onmousedown | `onmouseup |
                        `onmouseover | `onmousemove | `onmouseout |
                        `onkeypress | `onkeydown | `onkeyup ]
    type attrs = [ coreattrs | i18n | events ]
    type head_misc = [ `script | `style | `meta | `link | `e_object ]
    type focus = [ `accesskey | `tabindex | `onfocus | `onblur ]
  end

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

type el_name = string
type attr_name = string

type attr_value =
  | Attr_Id of id
  | Attr_IdRef of id
  | Attr_Uri of string (* TODO: codificare le URI in modo safe? *)
  | Attr_UriList of string list
  | Attr_LanguageCode of string
  | Attr_Direction of direction
  | Attr_CDATA of string
  | Attr_Character of char
  | Attr_StyleSheet of string
  | Attr_Text of string
  | Attr_Script of string
  | Attr_Charset of Netconversion.encoding
  | Attr_Charsets of Netconversion.encoding list
  | Attr_ContentType of string
  | Attr_ContentTypes of string list
  | Attr_LinkTypes of link_type list
  | Attr_MediaDesc of media_desc list
  | Attr_Datetime of string
  | Attr_Shape of shape
  | Attr_Coords of length list
  | Attr_Length of length
  | Attr_Number of int
  | Attr_Param_value_type of param_value_type
  | Attr_Form_method of form_method
  | Attr_InputType of input_type
  | Attr_Button_type of button_type
  | Attr_Pixels of int
  | Attr_TFrame of table_frame
  | Attr_TRules of table_rules
  | Attr_MultiLength of multi_length
  | Attr_CellHAlign of halign
  | Attr_CellVAlign of valign
  | Attr_IdRefs of id list (* space separated list of IDs *)
  | Attr_Scope of cell_scope
  | Attr_Constant of string

type inner_attrib = attr_name * attr_value
type inner_element =
  | Element of el_name * inner_attrib list * inner_element list
  | Empty_element of el_name * inner_attrib list * inner_element list
  | PCData of string * bool  (* to be escaped if bool = true *)
  | CData of string * string * string (* prefix, cdata, suffix *) (* NOT to be escaped *)
  | Comment of string * bool

type 'a attrib = inner_attrib
type ('a, 'b) element = inner_element

let string_of_direction = function
  | `Ltr -> "lrt"
  | `Rtl -> "rtl"

let string_of_link_type = function
  | `alternate -> "alternate"
  | `stylesheet -> "stylesheet"
  | `start -> "start"
  | `next -> "next"
  | `prev -> "prev"
  | `contents -> "contents"
  | `index -> "index"
  | `glossary -> "glossary"
  | `copyright -> "copyright"
  | `chapter -> "chapter"
  | `section -> "section"
  | `subsection -> "subsection"
  | `appendix -> "appendix"
  | `help -> "help"
  | `bookmark -> "bookmark"
  | `nofollow -> "nofollow"

let string_of_media_desc = function
  | `screen -> "screen"
  | `tty -> "tty"
  | `tv -> "tv"
  | `projection -> "projection"
  | `handheld -> "handheld"
  | `print -> "print"
  | `braille -> "braille"
  | `aural -> "aural"
  | `all -> "all"

let string_of_shape = function
  | `rect -> "rect"
  | `circle -> "circle"
  | `poly -> "poly"
  | `default -> "default"

let string_of_length = function
  | `px p -> (string_of_int p) ^ "px"
  | `per p -> (string_of_int p) ^ "%"

let string_of_param_value_type = function
  | `data -> "data"
  | `ref -> "ref"
  | `p_object -> "object"

let string_of_input_type = function
  | `text -> "text"
  | `password -> "password"
  | `checkbox -> "checkbox"
  | `radio -> "radio"
  | `submit -> "submit"
  | `reset -> "reset"
  | `file -> "file"
  | `hidden -> "hidden"
  | `image -> "image"
  | `button -> "button"

let string_of_button_type = function
  | `button -> "button"
  | `submit -> "submit"
  | `reset -> "reset"

let string_of_table_frame = function
  | `void -> "void"
  | `above -> "above"
  | `below -> "below"
  | `hsides -> "hsides"
  | `lhs -> "lhs"
  | `rhs -> "rhs"
  | `vsides -> "vsides"
  | `box -> "box"
  | `border -> "border"

let string_of_table_rules = function
  | `none -> "none"
  | `groups -> "groups"
  | `rows -> "rows"
  | `cols -> "cols"
  | `all -> "all"

let string_of_multi_length = function
  | `px p -> (string_of_int p) ^ "px"
  | `per p -> (string_of_int p) ^ "%"
  | `rel rel -> Printf.sprintf "%d*" rel

let string_of_halign = function
  | `left -> "left"
  | `center -> "center"
  | `right -> "right"
  | `justify -> "justify"
  | `char -> "char"

let string_of_valign = function
  | `top -> "top"
  | `middle -> "middle"
  | `bottom -> "bottom"
  | `baseline -> "baseline"

let string_of_cell_scope = function
  | `row -> "row"
  | `col -> "col"
  | `rowgroup -> "rowgroup"
  | `colgroup -> "colgroup"

let cd ?(prefix = "") ?(suffix = "") s = CData (prefix, s, suffix)
let pc s = PCData (s, true)
let pc_unsafe s = PCData (s, false)
let comment ?(line = false) c = Comment (c, line)

let html ?(a = []) head body =
  let xmlns_value = Attr_Uri "http://www.w3.org/1999/xhtml" in
    Element ("html", ("xmlns", xmlns_value)::a, [head; body])

let head ?(a = []) title elts = Element ("head", a, title::elts)

let title ?(a = []) title = Element ("title", a, [pc title])

let base ?id ~href =
  let a_href = "href", Attr_Uri href in
    match id with
      | None -> Empty_element ("base", [a_href], [])
      | Some id -> Empty_element ("base", [("id", Attr_Id id); a_href], [])

let meta ?(a = []) ~content =
  Empty_element ("meta", ("content", Attr_CDATA content)::a, [])

let link attrs = Empty_element ("link", attrs, [])

let style ?(a = []) ~a_type content =
  Element ("style", ("type", Attr_ContentType a_type)::a, [content])

let script ?(a = []) ~a_type content =
  Element ("script", ("type", Attr_ContentType a_type)::a, content)

let noscript ?(a = []) elts = Element ("noscript", a, elts)
let body ?(a = []) elts = Element ("body", a, elts)
let div ?(a = []) elts = Element ("div", a, elts)
let p ?(a = []) elts = Element ("p", a, elts)
let h1 ?(a = []) elts = Element ("h1", a, elts)
let h2 ?(a = []) elts = Element ("h2", a, elts)
let h3 ?(a = []) elts = Element ("h3", a, elts)
let h4 ?(a = []) elts = Element ("h4", a, elts)
let h5 ?(a = []) elts = Element ("h5", a, elts)
let h6 ?(a = []) elts = Element ("h6", a, elts)
let ul ?(a = []) li lis = Element ("ul", a, li::lis)
let ol ?(a = []) li lis = Element ("ol", a, li::lis)
let li ?(a = []) elts = Element ("li", a, elts)
let dl ?(a = []) el elts = Element ("dl", a, el::elts)
let dt ?(a = []) elts = Element ("dt", a, elts)
let dd ?(a = []) elts = Element ("dd", a, elts)
let address ?(a = []) elts = Element ("address", a, elts)
let hr attrs = Empty_element ("hr", attrs, [])
let pre ?(a = []) elts = Element ("pre", a, elts)
let blockquote ?(a = []) elts = Element ("blockquote", a, elts)
let ins ?(a = []) elts = Element ("ins", a, elts)
let del ?(a = []) elts = Element ("del", a, elts)
let a ?(a = []) elts = Element ("a", a, elts)
let span ?(a = []) elts = Element ("span", a, elts)
let bdo ?(a = []) ~dir elts =
  Element ("bdo", ("dir", Attr_Direction dir)::a, elts)
let br attrs = Empty_element ("br", attrs, [])
let em ?(a = []) elts = Element ("em", a, elts)
let strong ?(a = []) elts = Element ("strong", a, elts)
let dfn ?(a = []) elts = Element ("dfn", a, elts)
let code ?(a = []) elts = Element ("code", a, elts)
let samp ?(a = []) elts = Element ("samp", a, elts)
let kbd ?(a = []) elts = Element ("kbd", a, elts)
let var ?(a = []) elts = Element ("var", a, elts)
let cite ?(a = []) elts = Element ("cite", a, elts)
let abbr ?(a = []) elts = Element ("abbr", a, elts)
let acronym ?(a = []) elts = Element ("acronym", a, elts)
let q ?(a = []) elts = Element ("q", a, elts)
let sub ?(a = []) elts = Element ("sub", a, elts)
let sup ?(a = []) elts = Element ("sup", a, elts)
let tt ?(a = []) elts = Element ("tt", a, elts)
let i ?(a = []) elts = Element ("i", a, elts)
let b ?(a = []) elts = Element ("b", a, elts)
let big ?(a = []) elts = Element ("big", a, elts)
let small ?(a = []) elts = Element ("small", a, elts)
let e_object ?(a = []) elts = Element ("object", a, elts)
let param attrs = Empty_element ("param", attrs, [])
let img ?(a = []) ~src ~alt () =
  let src, alt = ("src", Attr_Uri src), ("alt", Attr_Text alt) in
    Empty_element ("img", src::(alt::a), [])
let map ~id ?(a = []) elt elts =
  Element ("map", ("id", Attr_Id id)::a, elt::elts)
let area ?(a = []) ~alt () = Empty_element ("area", ("alt", Attr_Text alt)::a, [])
let form ?(a = []) ~action elts =
  Element ("form", ("action", Attr_Uri action)::a, elts)
let label ?(a = []) elts = Element ("label", a, elts)
let input ?(a = []) ~a_type () =
  Empty_element ("input", ("type", Attr_InputType a_type)::a, [])
let select ?(a = []) elt elts = Element ("select", a, elt::elts)
let optgroup ?(a = []) ~label opt opts =
  Element ("optgroup", ("label", Attr_Text label)::a, opt::opts)
let option ?(a = []) content =
  Element ("option", a, [pc content])
let textarea ?(a = []) ~rows ~cols content =
  Element ( "textarea",
            ("rows", Attr_Number rows)::("cols", Attr_Number cols)::a,
            [pc content] )
let fieldset ?(a = []) elts = Element ("fieldset", a, elts)
let legend ?(a = []) content = Element ("legend", a, [content])
let button ?(a = []) ?(a_type) elts =
  let attributes = match a_type with
    | None -> a
    | Some a_type -> ("type", Attr_Button_type a_type)::a in
    Element ("button", attributes, elts)

let table_complete ?(a = []) ?caption ?(cols) ?thead ?tfoot rows =
  let inner = match rows with
    | `Tbody (tbody, other_tbodys) -> tbody::other_tbodys
    | `Tr (tr, other_trs) -> tr::other_trs in
  let inner = match tfoot with None -> inner | Some f -> f::inner in
  let inner = match thead with None -> inner | Some h -> h::inner in
  let inner = match cols with
    | None -> inner
    | Some (`Columns c) -> c@inner
    | Some (`Colgroups cg) -> cg@inner in
  let inner = match caption with None -> inner | Some c -> c::inner in
    Element ("table", a, inner)

let table ?(a = []) ?caption ?thead ?tfoot trs =
  match trs with
    | [] -> raise Empty_table
    | rows ->
      let inner = match tfoot with None -> rows | Some f -> f::rows in
      let inner = match thead with None -> inner | Some h -> h::inner in
      let inner = match caption with None -> inner | Some c -> c::inner in
        Element ("table", a, inner)

let caption ?(a = []) elts = Element ("caption", a, elts)
let thead ?(a = []) tr trs = Element ("thead", a, tr::trs)
let tfoot ?(a = []) tr trs = Element ("tfoot", a, tr::trs)
let tbody ?(a = []) tr trs = Element ("tbody", a, tr::trs)
let colgroup ?(a = []) cols = Element ("colgroup", a, cols)
let colgroups elts = `Colgroups elts
let col attrs = Empty_element ("col", attrs, [])
let cols elts = `Columns elts
let tr ?(a = []) td tds = Element ("tr", a, td::tds)
let th ?(a = []) elts = Element ("th", a, elts)
let td ?(a = []) elts = Element ("td", a, elts)


(* Attributes *)
module Attributes_i18n =
  struct
    let a_lang l = ("lang", Attr_LanguageCode l)
    let a_xml_lang l = ("xml:lang", Attr_LanguageCode l)
    let a_dir d = ("dir", Attr_Direction d)
  end

module Attributes_coreattrs =
  struct
    let a_id id = ("id", Attr_Id id)
    let a_class c = ("class", Attr_CDATA c)
    let a_style s = ("style", Attr_StyleSheet s)
    let a_title t = ("title", Attr_Text t)
  end

module Attributes_events =
  struct
    let a_onclick s = ("onclick", Attr_Script s)
    let a_ondblclick s = ("ondblclick", Attr_Script s)
    let a_onmousedown s = ("onmousedown", Attr_Script s)
    let a_onmouseup s = ("onmouseup", Attr_Script s)
    let a_onmouseover s = ("onmouseover", Attr_Script s)
    let a_onmousemove s = ("onmousemove", Attr_Script s)
    let a_onmouseout s = ("onmouseout", Attr_Script s)
    let a_onkeypress s = ("onkeypress", Attr_Script s)
    let a_onkeydown s = ("onkeydown", Attr_Script s)
    let a_onkeyup s = ("onkeyup", Attr_Script s)
  end

module Attributes_focus =
  struct
    let a_accesskey c = ("accesskey", Attr_Character c)
    let a_tabindex n = ("tabindex", Attr_Number n)
    let a_onfocus s = ("onfocus", Attr_Script s)
    let a_onblur s = ("onblur", Attr_Script s)
  end

let a_lang = Attributes_i18n.a_lang
let a_xml_lang = Attributes_i18n.a_xml_lang
let a_dir = Attributes_i18n.a_dir

let a_id = Attributes_coreattrs.a_id
let a_class = Attributes_coreattrs.a_class
let a_style = Attributes_coreattrs.a_style
let a_title = Attributes_coreattrs.a_title

let a_onclick = Attributes_events.a_onclick
let a_ondblclick = Attributes_events.a_ondblclick
let a_onmousedown = Attributes_events.a_onmousedown
let a_onmouseup = Attributes_events.a_onmouseup
let a_onmouseover = Attributes_events.a_onmouseover
let a_onmousemove = Attributes_events.a_onmousemove
let a_onmouseout = Attributes_events.a_onmouseout
let a_onkeypress = Attributes_events.a_onkeypress
let a_onkeydown = Attributes_events.a_onkeydown
let a_onkeyup = Attributes_events.a_onkeyup

let a_accesskey = Attributes_focus.a_accesskey
let a_tabindex = Attributes_focus.a_tabindex
let a_onfocus = Attributes_focus.a_onfocus
let a_onblur = Attributes_focus.a_onblur

let a_profile p = ("profile", Attr_Uri p)

let a_http_equiv content = ("http-equiv", Attr_CDATA content)
let a_name content = ("name", Attr_CDATA content)
let a_scheme content = ("scheme", Attr_CDATA content)

let a_charset ch = ("charset", Attr_Charset ch)
let a_href p = ("href", Attr_Uri p)
let a_hreflang l = ("hreflang", Attr_LanguageCode l)
let a_type t = ("type", Attr_ContentType t)
let a_rel rels = ("rel", Attr_LinkTypes rels)
let a_rev rels = ("rev", Attr_LinkTypes rels)
let a_media media = ("media", Attr_MediaDesc media)

let a_src uri = ("src", Attr_Uri uri)
let a_defer = ("defer", Attr_Constant "defer")

let a_onload s = ("onload", Attr_Script s)
let a_onunload s = ("onunload", Attr_Script s)

let a_cite uri = ("cite", Attr_Uri uri)

let datetime_regexp =
  let d = "[0-9]" in
    Printf.sprintf
      "^\\(%s%s%s%s\\)-\\(%s%s\\)-\\(%s%s\\)T\\(%s%s\\):\\(%s%s\\):\\(%s%s\\)\\(Z\\|\\(\\(\\+\\|-\\)\\(%s%s\\):\\(%s%s\\)\\)\\)$"
      d d d d d d d d d d d d d d d d d d |> Str.regexp

let a_datetime dt =
  if Str.string_match datetime_regexp dt 0 = false then
    raise (Invalid_datetime ("Incorrect format for string \"" ^ dt ^ "\""));

  let year = Str.matched_group 1 dt |> int_of_string in
    if year < 1970 then
      raise (Invalid_datetime ("Invalid year: " ^ (Str.matched_group 1 dt)));

  let month = Str.matched_group 2 dt |> int_of_string in
    if month < 1 || month > 12 then
      raise (Invalid_datetime ("Invalid month: " ^ (Str.matched_group 2 dt)));

  let day = Str.matched_group 3 dt |> int_of_string in
    if day < 1 || day > 31 then
      raise (Invalid_datetime ("Invalid day: " ^ (Str.matched_group 3 dt)));

  let hours = Str.matched_group 4 dt |> int_of_string in
    if hours < 0 || hours > 23 then
      raise (Invalid_datetime ("Invalid hours: " ^ (Str.matched_group 5 dt)));

  let minutes = Str.matched_group 5 dt |> int_of_string in
    if minutes < 0 || minutes > 59 then
      raise (Invalid_datetime ("Invalid minutes: " ^ (Str.matched_group 5 dt)));

  let seconds = Str.matched_group 6 dt |> int_of_string in
    if seconds < 0 || seconds > 59 then
      raise (Invalid_datetime ("Invalid seconds: " ^ (Str.matched_group 6 dt)));

  let tz = Str.matched_group 7 dt in
    if tz <> "Z" then begin
      let tz_hours_s = Str.matched_group 10 dt in
      let tz_min_s = Str.matched_group 11 dt in
      let tz_hours = int_of_string tz_hours_s in
      let tz_min = int_of_string tz_min_s in
        if tz_hours < 0 || tz_hours > 23 then
          raise (Invalid_datetime ("Invalid TZ hour: " ^ tz_hours_s));
        if tz_min < 0 || tz_min > 59 then
          raise (Invalid_datetime ("Invalid TZ minute: " ^ tz_min_s));
    end;
    ("datetime", Attr_Datetime dt)

let a_shape s = ("shape", Attr_Shape s)
let a_coords coo_l = ("coords", Attr_Coords coo_l)

let a_declare = ("declare", Attr_Constant "declare")
let a_classid uri = ("classid", Attr_Uri uri)
let a_codebase uri = ("codebase", Attr_Uri uri)
let a_data uri = ("data", Attr_Uri uri)
let a_codetype t = ("codetype", Attr_ContentType t)
let a_archive l = ("archive", Attr_UriList l)
let a_standby s = ("standby", Attr_Text s)
let a_height l = ("height", Attr_Length l)
let a_width w = ("width", Attr_Length w)
let a_usemap u = ("usemap", Attr_Uri u)

let a_value v = ("value", Attr_CDATA v)
let a_valuetype vt = ("valuetype", Attr_Param_value_type vt)

let a_longdesc desc = ("longdesc", Attr_Uri desc)
let a_ismap = ("ismap", Attr_Constant "ismap")

let a_nohref = ("nohref", Attr_Constant "nohref")

let a_method m = ("method", Attr_Form_method m)
let a_enctype enc = ("enctype", Attr_ContentType enc)
let a_onsubmit s = ("onsubmit", Attr_Script s)
let a_onreset s = ("onreset", Attr_Script s)
let a_accept l = ("accept", Attr_ContentTypes l)
let a_accept_charset l = ("accept-charset", Attr_Charsets l)

let a_for id = ("for", Attr_IdRef id)

let a_checked = ("checked", Attr_Constant "checked")
let a_disabled = ("disabled", Attr_Constant "disabled")
let a_readonly = ("readonly", Attr_Constant "readonly")
let a_size n = ("size", Attr_Number n)
let a_maxlength n = ("maxlength", Attr_Number n)
let a_alt str = ("alt", Attr_CDATA str)
let a_onselect script = ("onselect", Attr_Script script)
let a_onchange script = ("onchange", Attr_Script script)

let a_multiple = ("multiple", Attr_Constant "multiple")

let a_label text = ("label", Attr_Text text)

let a_summary text = ("summary", Attr_Text text)
let a_border pixels = ("border", Attr_Pixels pixels)
let a_frame frame_type = ("frame", Attr_TFrame frame_type)
let a_rules rules = ("rules", Attr_TRules rules)
let a_cellspacing space = ("cellspacing", Attr_Length space)
let a_cellpadding space = ("cellpadding", Attr_Length space)

let a_span n = ("span", Attr_Number n)
let a_col_width w = ("width", Attr_MultiLength w)
let a_align a = ("align", Attr_CellHAlign a)
let a_char c = ("char", Attr_Character c)
let a_charoff l = ("charoff", Attr_Length l)
let a_valign a = ("valign", Attr_CellVAlign a)

let a_abbr text = ("abbr", Attr_Text text)
let a_axis text = ("axis", Attr_CDATA text)
let a_headers idrefs = ("headers", Attr_IdRefs idrefs)
let a_scope scope = ("scope", Attr_Scope scope)
let a_rowspan n = ("rowspan", Attr_Number n)
let a_colspan n = ("colspan", Attr_Number n)

let cat = String.concat

let rec render_value v =
  match v with
    | Attr_Id id -> id
    | Attr_Uri uri -> uri
    | Attr_UriList uri_l ->
        List.map (fun u -> Attr_Uri u) uri_l |> List.map render_value |> cat " "
    | Attr_LanguageCode lang -> lang
    | Attr_Direction dir -> (match dir with `ltr -> "ltr" | `rtl -> "rtl")
    | Attr_CDATA cd -> cd (* TODO!!! *)
    | Attr_Character c -> Printf.sprintf "%c" c
    | Attr_StyleSheet style_sheet -> render_value (Attr_CDATA style_sheet)
    | Attr_Text text -> render_value (Attr_CDATA text)
    | Attr_Script script -> render_value (Attr_CDATA script)
    | Attr_Charset enc -> Netconversion.string_of_encoding enc
    | Attr_Charsets enc_l -> List.map (fun c -> Attr_Charset c) enc_l |>
                              List.map render_value |> cat " "
    | Attr_ContentType ct -> render_value (Attr_CDATA ct)
    | Attr_ContentTypes ct_l -> List.map (fun c -> Attr_ContentType c) ct_l |>
                                  List.map render_value |> cat ","
    | Attr_LinkTypes link_t_list ->
        List.map string_of_link_type link_t_list |> cat " "
    | Attr_IdRef id -> id
    | Attr_IdRefs id_list -> List.map (fun id -> Attr_IdRef id) id_list |>
                              List.map render_value |> cat " "
    | Attr_MediaDesc md -> List.map string_of_media_desc md |> cat ","
    | Attr_Datetime dt -> dt
    | Attr_Shape s -> string_of_shape s
    | Attr_Coords len_l -> List.map string_of_length len_l |> cat ","
    | Attr_Length l -> string_of_length l
    | Attr_Number n -> string_of_int n
    | Attr_Param_value_type param -> string_of_param_value_type param
    | Attr_Form_method m -> (match m with `get -> "GET" | `post -> "POST")
    | Attr_InputType it -> string_of_input_type it
    | Attr_Button_type bt -> string_of_button_type bt
    | Attr_Pixels p -> Printf.sprintf "%dpx" p
    | Attr_TFrame t_frame -> string_of_table_frame t_frame
    | Attr_TRules t_rules -> string_of_table_rules t_rules
    | Attr_MultiLength m_length -> string_of_multi_length m_length
    | Attr_CellHAlign ha -> string_of_halign ha
    | Attr_CellVAlign va -> string_of_valign va
    | Attr_Constant c -> c
    | Attr_Scope c_scope -> string_of_cell_scope c_scope

let rec render_attribute (attr_name, attr_value) =
  let value_string = render_value attr_value in
    Printf.sprintf " %s=\"%s\"" attr_name value_string

let malformed_comment_regexp = Str.regexp ".*--.*"

let rec render_element empty_model in_enc out_enc name attrs children =
  let spf = Printf.sprintf in
  let b = Buffer.create 1024 in
  let add = Buffer.add_string b in
    if name = "html" then
      add ("<?xml version=\"1.0\" encoding=\"" ^ (Netconversion.string_of_encoding out_enc) ^ "\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n");
    add (spf "<%s" name); (* open element *)
    let () = match attrs, children with
      | [], [] -> add (if empty_model then " />" else (spf "></%s>" name))
      | [], c -> begin
          add ">";
          List.iter (fun ch -> add (unsafe_string_of_element ~in_enc:in_enc ~out_enc:out_enc ch)) c;
          add (spf "</%s>" name);
        end
      | attrs, [] -> begin
          List.iter (fun a -> add (render_attribute a)) attrs;
          add (if empty_model then " />" else (spf "></%s>" name));
        end
      | attrs, children -> begin
          List.iter (fun a -> add (render_attribute a)) attrs;
          add ">";
          List.iter (fun ch -> add (unsafe_string_of_element ~in_enc:in_enc ~out_enc:out_enc ch)) children;
          add (spf "</%s>" name);
        end in
      Buffer.contents b

and unsafe_string_of_element ?(in_enc = `Enc_utf8) ?(out_enc = `Enc_utf8) e =
  match e with
    | Element (name, attrs, children) ->
        render_element false in_enc out_enc name attrs children
    | Empty_element (name, attrs, children) ->
        render_element true in_enc out_enc name attrs children
    | PCData (pc, escape) ->
        if escape = true then
          Netencoding.Html.decode ~in_enc:in_enc ~out_enc:`Enc_utf8 () pc |>
            Netencoding.Html.encode ~in_enc:`Enc_utf8 ~out_enc:out_enc   ()
        else pc
    | CData (pre, cd, suff) ->
        pre ^ "<![CDATA[" ^ suff ^  "\n" ^
          (* Removes the "]]>" delimiters from CDATA section content *)
          (Str.global_replace (Str.regexp_string "]]>") "" cd) ^
        "\n" ^ pre ^ "]]>" ^ suff ^ "\n"
    | Comment (c, line) ->
        if Str.string_match malformed_comment_regexp c 0 = true
        then raise (Malformed_comment c)
        else begin
          if line then "<!-- " ^ c ^ " -->"
          else "<!--\n" ^ c ^ "\n-->"
        end

let rec string_of_element ?(in_enc = `Enc_utf8) ?(out_enc = `Enc_utf8) e =
  (* TODO check da fare su idref, idrefs label/for *)
  unsafe_string_of_element ~in_enc:in_enc ~out_enc:out_enc e

