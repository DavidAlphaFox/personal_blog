open OUnit
open Parameters_map

let int_scalar () =
  let map = of_assoc [("i", "42")] in
  let p = reconstruct_params map (int "i") in
    assert_equal p 42
;;

let float_scalar () =
  let map = of_assoc [("f", "42")] in
  let p = reconstruct_params map (float "f") in
    assert_equal p 42.0
;;

let optional_int () =
  let map = of_assoc [("i", "42")] in
  let p = reconstruct_params map (opt (int "i")) in
    assert_equal p (Some 42)
;;

let missing_int () =
  let map = of_assoc [("f", "42")] in
  let p = reconstruct_params map (opt (int "i")) in
    assert_equal p (None)
;;

let tuple_with_option () =
  let map = of_assoc [("i", "42"); ("f", "3.14"); ("opt_i", "101")] in
  let p = reconstruct_params map (int "i" ** (float "f" ** (opt (int "opt_i")))) in
    assert_equal p (42, (3.14, Some 101))
;;

let tuple_with_missing_option () =
  let map = of_assoc [("i", "42"); ("f", "3.14");] in
  let p = reconstruct_params map (int "i" ** (float "f" ** (opt (int "opt_i")))) in
    assert_equal p (42, (3.14, None))
;;

let tpl3_with_missing_option () =
  let map = of_assoc [("i", "42"); ("f", "3.14");] in
  let p = reconstruct_params map (tpl3 (int "i") (float "f") (opt (int "opt_i"))) in
    assert_equal p (42, 3.14, None)
;;

let int_list () =
  let map = of_assoc [("pre-10", "42"); ("pre-12", "420"); ("pre-27", "4200")] in
  let p = reconstruct_params map (lst (int "pre")) in
    assert_equal p [42; 420; 4200]
;;

let float_and_int_list () =
  let map = of_assoc [("pre-10", "42"); ("float_par", "3.14"); ("pre-12", "420"); ("pre-27", "4200")] in
  let p = reconstruct_params map (float "float_par" ** lst (int "pre")) in
    assert_equal p (3.14, [42; 420; 4200])
;;

let int_float_list () =
  let map = of_assoc [("int-1", "42"); ("int-2", "43"); ("int-4", "44");
                      ("float-1", "3.14"); ("float-2", "6.28"); ("float-4", "42.01")] in
  let p = reconstruct_params map (lst (int "int" ** float "float")) in
    assert_equal p [(42, 3.14); (43, 6.28); (44, 42.01)]
;;

let int_option_float_list () =
  let map = of_assoc [("int-1", "42"); ("int-2", "43"); ("int-4", "44");
                      ("float-1", "3.14"); ("float-2", "6.28"); ("float-4", "42.01");
                      ("float-42", "10.42")] in
  let p = reconstruct_params map (lst (opt (int "int") ** float "float")) in
    assert_equal p [(Some 42, 3.14); (Some 43, 6.28); (Some 44, 42.01); (None, 10.42)]
;;

let complex_tuple_list () =
  let map = of_assoc  [
                        ("str-20", "hallo20"); ("int-20", "20"); ("fl-20", "20.20");
                        ("str-1", "hallo1");   ("int-1", "1");   ("fl-1", "1.1");
                        ("str-2", "hallo2");   ("int-2", "2")    (* None *)      ;
                        ("str-3", "hallo3");              (* None *)
                        ("str-4", "hallo4");   (* Missing *)     ("fl-4", "4.4"); (* -> ("hallo4", None) *)
                        (*                    ("int-5", "5");   ("fl-5", "5.5");*)
                        ("str-10", "hallo10"); (* Missing *)     ("fl-10", "10.10");
                      ] in
  let p = reconstruct_params map (lst ((string "str") ** (opt ((int "int") ** opt (float "fl"))))) in
    assert_equal p [
                      ("hallo1", Some (1, Some 1.100000));
                      ("hallo2", Some (2, None));
                      ("hallo3", None);
                      ("hallo4", None);
                      ("hallo10", None);
                      ("hallo20", Some (20, Some 20.200000));
                   ]
;;

let tuple_with_complex_tuple_list () =
  let map = of_assoc [
                      ("login", "paolo");
                      ("str-20", "hallo20"); ("int-20", "20"); ("fl-20", "20.20");
                      ("str-1", "hallo1");   ("int-1", "1");   ("fl-1", "1.1");
                      ("str-2", "hallo2");   ("int-2", "2")    (* None *)      ;
                      ("str-3", "hallo3");              (* None *)
                      ("str-4", "hallo4");   (* Missing *)     ("fl-4", "4.4"); (* -> ("hallo4", None) *)
                      (*                    ("int-5", "5");   ("fl-5", "5.5");*)
                      ("str-10", "hallo10"); (* Missing *)     ("fl-10", "10.10");
                     ] in
  let p = reconstruct_params map
    ((bool "permanent") ** ((string "login") ** lst ((string "str") ** (opt ((int "int") ** opt (float "fl")))))) in
    assert_equal p (false, ("paolo", ( [
                                          ("hallo1", Some (1, Some 1.100000));
                                          ("hallo2", Some (2, None));
                                          ("hallo3", None);
                                          ("hallo4", None);
                                          ("hallo10", None);
                                          ("hallo20", Some (20, Some 20.200000));
                                       ] )))
;;

let tuple_with_complex_tuple_list2 () =
  let map = of_assoc [
                      ("permanent", "foo bar");
                      ("login", "paolo");
                      ("str-20", "hallo20"); ("int-20", "20"); ("fl-20", "20.20");
                      ("str-1", "hallo1");   ("int-1", "1");   ("fl-1", "1.1");
                      ("str-2", "hallo2");   ("int-2", "2")    (* None *)      ;
                      ("str-3", "hallo3");              (* None *)
                      ("str-4", "hallo4");   (* Missing *)     ("fl-4", "4.4"); (* -> ("hallo4", None) *)
                      (*                    ("int-5", "5");   ("fl-5", "5.5");*)
                      ("str-10", "hallo10"); (* Missing *)     ("fl-10", "10.10");
                     ] in
  let p = reconstruct_params map
    ((bool "permanent") ** ((string "login") ** lst ((string "str") ** (opt ((int "int") ** opt (float "fl")))))) in
    assert_equal p (true, ("paolo", (  [
                                          ("hallo1", Some (1, Some 1.100000));
                                          ("hallo2", Some (2, None));
                                          ("hallo3", None);
                                          ("hallo4", None);
                                          ("hallo10", None);
                                          ("hallo20", Some (20, Some 20.200000));
                                       ] )))
;;

let bool_inside_a_list () =
  let map = of_assoc [
                        ("str-1", "hallo 1"); ("ckbox-1", "bau");
                        ("str-2", "hallo 2"); (* false *)
                        (*(* ERROR *)          ("ckbox-3", "bau");*)
                     ] in
  let p = reconstruct_params map (lst ((string "str") ** (bool "ckbox"))) in
    assert_equal p [ ("hallo 1", true); ("hallo 2", false); ]
;;

let bool_inside_a_list_missing () =
  let map = of_assoc [
                        ("str-1", "hallo 1"); ("ckbox-1", "bau");
                        ("str-2", "hallo 2"); (* false *)
                        (* ERROR *)          ("ckbox-3", "bau");
                     ] in
  let f () = reconstruct_params map (lst ((string "str") ** (bool "ckbox"))) in
    assert_raises (Parameters_map.Param_not_found("str-3")) f
;;

let complex_nested_tpl3 () =
  let map = of_assoc [
                       ("permanent", "foo bar");
                       ("login", "paolo");
                       ("str-20", "hallo20"); ("int-20", "20"); ("fl-20", "20.20");
                       ("str-1", "hallo1");   ("int-1", "1");   ("fl-1", "1.1");
                       ("str-2", "hallo2");   ("int-2", "2")    (* None *)      ; ] in
  let p = reconstruct_params map
     (
       tpl3 (bool "permanent") (string "login") (
         lst (
           tpl3 (string "str") (int "int") (opt (float "fl"))
         )
       )
     ) in
    assert_equal p (true, "paolo", [
                                      ("hallo1", 1, Some 1.100000);
                                      ("hallo2", 2, None);
                                      ("hallo20", 20, Some 20.200000);
                                   ])

;;

type gender =
  | Male
  | Female
  | Unknown;;

let gender_of_string = function (* can be produced via Sexplib! *)
  | "Male" -> Male
  | "Female" -> Female
  | "Unknown" -> Unknown
  | v -> convert_fail "gender"
;;

let string_of_gender = function (* can be produced via Sexplib! *)
  | Male -> "Male"
  | Female -> "Female"
  | Unknown -> "Unknown"
;;

let user_type_test () =
  let map = of_assoc [
                       ("gndr-5", "Male");
                       ("gndr-100", "Female");
                       ("gndr-1", "Unknown");
                       ("gndr-6", "Female");
                     ] in
  let p = reconstruct_params map (lst (user_type "gndr" gender_of_string)) in
    assert_equal p [ Unknown; Male; Female; Female; ]
;;


let suite = "Parameters reconstruction" >::: [
    "Int scalar" >:: int_scalar;
    "Float scalar" >:: float_scalar;
    "Optional int option" >:: optional_int;
    "Missing int option" >:: missing_int;
    "Tuple with option (present)" >:: tuple_with_option;
    "Tuple with option (missing)" >:: tuple_with_missing_option;
    "3 element tuple with option (missing)" >:: tuple_with_missing_option;
    "Int list" >:: int_list;
    "float * int list" >:: float_and_int_list;
    "(int * float) list" >:: int_float_list;
    "(int option * float) list" >:: int_option_float_list;
    "List of complex tuple (with opt)" >:: complex_tuple_list;
    "List of complex tuple (with opt) inside nested tuples" >:: tuple_with_complex_tuple_list;
    "List of complex tuple (with opt) inside nested tuples (2)" >:: tuple_with_complex_tuple_list2;
    "(string * bool) list" >:: bool_inside_a_list;
    "(string * bool) list (a string is missing!)" >:: bool_inside_a_list_missing;
    "Complex nested tpl3" >:: complex_nested_tpl3;
    "Simple user type test" >:: user_type_test;
  ]
;;

OUnit.run_test_tt_main suite;;

