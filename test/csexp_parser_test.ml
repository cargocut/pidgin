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

  let parse0 =
    test_case "from_string" `Quick (fun () ->
      let str = "" in
      let expected = Ok (n [])
      and computed = Csexp.from_string str in
      check Test_lib.Testable.csexp_parsed "should be equal" expected computed)
  ;;

  let parse1 =
    test_case "from_string" `Quick (fun () ->
      let str = "3:foo" in
      let expected = Ok (a "foo")
      and computed = Csexp.from_string str in
      check Test_lib.Testable.csexp_parsed "should be equal" expected computed)
  ;;

  let parse2 =
    test_case "from_string" `Quick (fun () ->
      let str = "12:abcd efg hij" in
      let expected = Ok (a "abcd efg hij")
      and computed = Csexp.from_string str in
      check Test_lib.Testable.csexp_parsed "should be equal" expected computed)
  ;;

  let parse3 =
    test_case "from_string" `Quick (fun () ->
      let str =
        "((4:name3:msp)(5:email12:msp@mail.com)(3:age2:30)(4:lang7:haskell)(14:social_account((7:twitter3:msp)(6:github5:mspwn)(8:mastodon16:msp@cargocut.org)(6:gitlab5:mspwn)(4:bsky7:msp.org)))(3:bio61:I \
         am a functional programmer based in Italy\n\
         test (a sub node)))"
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
      and computed = Csexp.from_string str in
      check Test_lib.Testable.csexp_parsed "should be equal" expected computed)
  ;;

  let parse_invalid0 =
    test_case "from_string" `Quick (fun () ->
      let str = "6:foo" in
      let expected =
        Error.Csexp.premature_end_of_atom ~expected_length:6 ~given_length:3 4
      and computed = Csexp.from_string str in
      check Test_lib.Testable.csexp_parsed "should be equal" expected computed)
  ;;

  let parse_invalid1 =
    test_case "from_string" `Quick (fun () ->
      let str = "(3:foo))" in
      let expected = Error.Csexp.non_opened_node 7
      and computed = Csexp.from_string str in
      check Test_lib.Testable.csexp_parsed "should be equal" expected computed)
  ;;

  let parse_invalid2 =
    test_case "from_string" `Quick (fun () ->
      let str = "((3:foo)" in
      let expected = Error.Csexp.non_terminated_node 7
      and computed = Csexp.from_string str in
      check Test_lib.Testable.csexp_parsed "should be equal" expected computed)
  ;;
end

let cases =
  ( "Csexp (Parser)"
  , [ parse0
    ; parse1
    ; parse2
    ; parse3
    ; parse_invalid0
    ; parse_invalid1
    ; parse_invalid2
    ] )
;;
