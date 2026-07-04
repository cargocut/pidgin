(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open Pidgin.Repr

let dump repr = repr |> Format.asprintf "%a" Pidgin_pp.repr |> print_endline

let%expect_test "dump null" =
  dump (null ());
  [%expect {| null |}]
;;

let%expect_test "dump true" =
  dump (bool true);
  [%expect {| true |}]
;;

let%expect_test "dump false" =
  dump (bool false);
  [%expect {| false |}]
;;

let%expect_test "dump 42" =
  dump (int 42);
  [%expect {| 42 |}]
;;

let%expect_test "dump 42.36" =
  dump (float 42.36);
  [%expect {| 42.36 |}]
;;

let%expect_test "dump empty list" =
  dump (string "");
  [%expect {| "" |}]
;;

let%expect_test "dump Hello World" =
  dump (string "Hello World");
  [%expect {| "Hello World" |}]
;;

let%expect_test "dump an empty list" =
  dump (list []);
  [%expect {| [] |}]
;;

let%expect_test "dump a list of int" =
  dump (list_of int (List.init 99 Fun.id));
  [%expect
    {|
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
     21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
     40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58,
     59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77,
     78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96,
     97, 98]
    |}]
;;

let%expect_test "dump an empty record" =
  dump (record []);
  [%expect {| {} |}]
;;

let%expect_test "dump a record" =
  dump
    (record
       [ "nickname", string "mspwn"
       ; "gender", string "male"
       ; "email", string "msp@domain.com"
       ; "level", int 10
       ]);
  [%expect
    {|
    {"nickname": "mspwn", "gender": "male", "email": "msp@domain.com",
     "level": 10}
    |}]
;;

let%expect_test "dump an other record" =
  dump
    (record
       [ "nickname", string "mspwn"
       ; "gender", string "male"
       ; "email", string "msp@domain.com"
       ; ( "other"
         , record
             [ "nickname", string "mspwn"
             ; "gender", string "male"
             ; "email", string "msp@domain.com"
             ; "level", int 10
             ; ( "more_nesting"
               , record
                   [ "nickname", string "mspwn"
                   ; "gender", string "male"
                   ; "email", string "msp@domain.com"
                   ; "level", int 10
                   ] )
             ] )
       ; "level", int 10
       ]);
  [%expect
    {|
    {"nickname": "mspwn", "gender": "male", "email": "msp@domain.com",
     "other":
      {"nickname": "mspwn", "gender": "male", "email": "msp@domain.com",
       "level": 10,
       "more_nesting":
        {"nickname": "mspwn", "gender": "male", "email": "msp@domain.com",
         "level": 10}}, "level": 10}
    |}]
;;
