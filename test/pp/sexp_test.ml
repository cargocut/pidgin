(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open Pidgin.Sexp

let dump sexp = sexp |> Format.asprintf "%a" Pidgin_pp.sexp |> print_endline

let%expect_test "dump a simple atom" =
  atom "Hello World" |> dump;
  [%expect {| Hello\ World |}]
;;

let%expect_test "dump an empty node" =
  node [] |> dump;
  [%expect {| () |}]
;;

let%expect_test "dump a node" =
  node
    [ node [ atom "foo"; node [ atom "1" ] ]
    ; node [ atom "bar"; atom "2" ]
    ; node
        [ atom "baz"
        ; node
            [ node [ atom "flag"; atom "true" ]
            ; node [ atom "message"; atom "Hello World" ]
            ]
        ]
    ]
  |> dump;
  [%expect {| ((foo (1)) (bar 2) (baz ((flag true) (message Hello\ World)))) |}]
;;

let%expect_test "dump a node" =
  node
    [ node [ atom "foo"; node [ atom "1" ] ]
    ; node [ atom "bar"; atom "2" ]
    ; node [ atom "a_complicated_node"; atom "2" ]
    ; node
        [ atom "baz"
        ; node
            [ node [ atom "flag"; atom "true" ]
            ; node [ atom "message"; atom "Hello World" ]
            ; node [ atom "message2"; atom "Hello World" ]
            ; node
                [ atom "message3"
                ; atom "Hello World"
                ; atom "key"
                ; atom "value"
                ; node (List.init 7 (fun i -> atom @@ string_of_int i))
                ]
            ]
        ]
    ]
  |> dump;
  [%expect
    {|
    ((foo (1))
     (bar 2)
     (a_complicated_node 2)
     (baz
      ((flag true)
       (message Hello\ World)
       (message2 Hello\ World)
       (message3 Hello\ World key value (0 1 2 3 4 5 6)))))
    |}]
;;
