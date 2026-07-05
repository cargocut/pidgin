(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open Pidgin.Sexp

let dump sexp = sexp |> Format.asprintf "%a" Pidgin_pp.csexp |> print_endline

let%expect_test "dump a simple atom" =
  atom "Hello World" |> dump;
  [%expect {| 11:Hello World |}]
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
  [%expect
    {| ((3:foo(1:1))(3:bar1:2)(3:baz((4:flag4:true)(7:message11:Hello World)))) |}]
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
    {| ((3:foo(1:1))(3:bar1:2)(18:a_complicated_node1:2)(3:baz((4:flag4:true)(7:message11:Hello World)(8:message211:Hello World)(8:message311:Hello World3:key5:value(1:01:11:21:31:41:51:6))))) |}]
;;
