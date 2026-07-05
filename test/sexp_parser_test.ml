(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let n = Sexp.node
  let a = Sexp.atom
  let kv k v = n [ a k; v ]

  let parse_empty =
    test_case "Parse a simple atom" `Quick (fun () ->
      let str = "" in
      let expected = Ok (n [])
      and computed = Sexp.from_string str in
      check Test_lib.Testable.sexp_parsed "should be equal" expected computed)
  ;;

  let parse_simple_atom =
    test_case "Parse a simple atom" `Quick (fun () ->
      let str = "foo" in
      let expected = Ok (a "foo")
      and computed = Sexp.from_string str in
      check Test_lib.Testable.sexp_parsed "should be equal" expected computed)
  ;;

  let parse_simple_node0 =
    test_case "Parse a simple node" `Quick (fun () ->
      let str = "foo bar baz (foo bar (bar baz (foo)))" in
      let expected =
        Ok
          (n
             [ a "foo"
             ; a "bar"
             ; a "baz"
             ; n [ a "foo"; a "bar"; n [ a "bar"; a "baz"; n [ a "foo" ] ] ]
             ])
      and computed = Sexp.from_string str in
      check Test_lib.Testable.sexp_parsed "should be equal" expected computed)
  ;;

  let parse_simple_node1 =
    test_case "Parse a simple node" `Quick (fun () ->
      let str = "(foo bar baz (foo bar (bar baz (foo))))" in
      let expected =
        Ok
          (n
             [ a "foo"
             ; a "bar"
             ; a "baz"
             ; n [ a "foo"; a "bar"; n [ a "bar"; a "baz"; n [ a "foo" ] ] ]
             ])
      and computed = Sexp.from_string str in
      check Test_lib.Testable.sexp_parsed "should be equal" expected computed)
  ;;

  let parse_simple_node2 =
    test_case "Parse a simple node" `Quick (fun () ->
      let str =
        "(name msp) (email msp@mail.com) (age 30) (lang haskell) \
         (social_account ((twitter msp) (github mspwn) (mastodon \
         msp@cargocut.org) (gitlab mspwn) (bsky msp.org)))"
      in
      let expected =
        Ok
          (n
             [ kv "name" (a "msp")
             ; kv "email" (a "msp@mail.com")
             ; kv "age" (a "30")
             ; kv "lang" (a "haskell")
             ; kv
                 "social_account"
                 (n
                    [ kv "twitter" (a "msp")
                    ; kv "github" (a "mspwn")
                    ; kv "mastodon" (a "msp@cargocut.org")
                    ; kv "gitlab" (a "mspwn")
                    ; kv "bsky" (a "msp.org")
                    ])
             ])
      and computed = Sexp.from_string str in
      check Test_lib.Testable.sexp_parsed "should be equal" expected computed)
  ;;

  let parse_simple_node3 =
    test_case "Parse a simple node" `Quick (fun () ->
      let str =
        "(name msp) (email msp@mail.com) (age 30) (lang haskell) \
         (social_account ((twitter msp) (github mspwn) (mastodon \
         msp@cargocut.org) (gitlab mspwn) (bsky msp.org))) (bio I\\ am\\ a\\ \
         functional\\ programmer\\ based\\ in\\ Italy)"
      in
      let expected =
        Ok
          (n
             [ kv "name" (a "msp")
             ; kv "email" (a "msp@mail.com")
             ; kv "age" (a "30")
             ; kv "lang" (a "haskell")
             ; kv
                 "social_account"
                 (n
                    [ kv "twitter" (a "msp")
                    ; kv "github" (a "mspwn")
                    ; kv "mastodon" (a "msp@cargocut.org")
                    ; kv "gitlab" (a "mspwn")
                    ; kv "bsky" (a "msp.org")
                    ])
             ; kv "bio" (a "I am a functional programmer based in Italy")
             ])
      and computed = Sexp.from_string str in
      check Test_lib.Testable.sexp_parsed "should be equal" expected computed)
  ;;

  let parse_simple_node_with_complicated_escape_rules =
    test_case "Parse a simple node" `Quick (fun () ->
      let str =
        "(name msp) (email msp@mail.com) (age 30) (lang haskell) \
         (social_account ((twitter msp) (github mspwn) (mastodon \
         msp@cargocut.org) (gitlab mspwn) (bsky msp.org))) (bio I\\ am\\ a\\ \
         functional\\ programmer\\ based\\ in\\ Italy\\\n\
         test\\ \\(a\\ sub\\ node\\))"
      in
      let expected =
        Ok
          (n
             [ kv "name" (a "msp")
             ; kv "email" (a "msp@mail.com")
             ; kv "age" (a "30")
             ; kv "lang" (a "haskell")
             ; kv
                 "social_account"
                 (n
                    [ kv "twitter" (a "msp")
                    ; kv "github" (a "mspwn")
                    ; kv "mastodon" (a "msp@cargocut.org")
                    ; kv "gitlab" (a "mspwn")
                    ; kv "bsky" (a "msp.org")
                    ])
             ; kv
                 "bio"
                 (a
                    "I am a functional programmer based in Italy\n\
                     test (a sub node)")
             ])
      and computed = Sexp.from_string str in
      check Test_lib.Testable.sexp_parsed "should be equal" expected computed)
  ;;

  let parse_simple_node_with_indent =
    test_case "Parse a simple node" `Quick (fun () ->
      let str =
        {sexp|
(name msp)
(email msp@mail.com)


(age 30)
(lang haskell)


(social_account
  ((twitter msp)
   (github mspwn) (mastodon msp@cargocut.org)
           (gitlab mspwn) (bsky msp.org)))

|sexp}
      in
      let expected =
        Ok
          (n
             [ kv "name" (a "msp")
             ; kv "email" (a "msp@mail.com")
             ; kv "age" (a "30")
             ; kv "lang" (a "haskell")
             ; kv
                 "social_account"
                 (n
                    [ kv "twitter" (a "msp")
                    ; kv "github" (a "mspwn")
                    ; kv "mastodon" (a "msp@cargocut.org")
                    ; kv "gitlab" (a "mspwn")
                    ; kv "bsky" (a "msp.org")
                    ])
             ])
      and computed = Sexp.from_string str in
      check Test_lib.Testable.sexp_parsed "should be equal" expected computed)
  ;;

  let parse_invalid_expr0 =
    test_case "Parse an invalid expr" `Quick (fun () ->
      let str = "(foo" in
      let expected = Sexp.Non_terminated_node 3 |> Result.error
      and computed = Sexp.from_string str in
      check Test_lib.Testable.sexp_parsed "should be equal" expected computed)
  ;;

  let parse_invalid_expr1 =
    test_case "Parse an invalid expr" `Quick (fun () ->
      let str = "fo)o" in
      let expected = Sexp.Non_opened_node 2 |> Result.error
      and computed = Sexp.from_string str in
      check Test_lib.Testable.sexp_parsed "should be equal" expected computed)
  ;;

  let parse_invalid_expr2 =
    test_case "Parse an invalid expr" `Quick (fun () ->
      let str = "(((((((((((())))) ())))(())))())))" in
      let expected = Sexp.Non_opened_node 33 |> Result.error
      and computed = Sexp.from_string str in
      check Test_lib.Testable.sexp_parsed "should be equal" expected computed)
  ;;

  let parse_invalid_expr3 =
    test_case "Parse an invalid expr" `Quick (fun () ->
      let str = "(foo bar (baz)" in
      let expected = Sexp.Non_terminated_node 13 |> Result.error
      and computed = Sexp.from_string str in
      check Test_lib.Testable.sexp_parsed "should be equal" expected computed)
  ;;

  let parse_simple_node4 =
    test_case "Parse node" `Quick (fun () ->
      let str =
        {|
    ((foo (1))
     (bar 2)
     (a_complicated_node 2)
     (baz
      ((flag true)
       (message Hello\ World)
       (message2 Hello\ World)
       (message3 Hello\ World key value (0 1 2 3 4 5 6)))))
    |}
      in
      let expected =
        Ok
          Sexp.(
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
                          ; node
                              (List.init 7 (fun i -> atom @@ string_of_int i))
                          ]
                      ]
                  ]
              ])
      and computed = Sexp.from_string str in
      check Test_lib.Testable.sexp_parsed "should be equal" expected computed)
  ;;
end

let cases =
  ( "Sexp (Parser)"
  , [ parse_empty
    ; parse_simple_atom
    ; parse_simple_node0
    ; parse_simple_node1
    ; parse_simple_node2
    ; parse_simple_node3
    ; parse_simple_node_with_complicated_escape_rules
    ; parse_simple_node_with_indent
    ; parse_invalid_expr0
    ; parse_invalid_expr1
    ; parse_invalid_expr2
    ; parse_invalid_expr3
    ; parse_simple_node4
    ] )
;;
