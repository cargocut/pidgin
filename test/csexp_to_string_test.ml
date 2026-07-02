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

  let to_string0 =
    test_case "to_string" `Quick (fun () ->
      let expr = a "foo" in
      let expected = "3:foo"
      and computed = Csexp.to_string expr in
      check string "should be equal" expected computed)
  ;;

  let to_string1 =
    test_case "to_string" `Quick (fun () ->
      let expr = a "abcd efg hij" in
      let expected = "12:abcd efg hij"
      and computed = Csexp.to_string expr in
      check string "should be equal" expected computed)
  ;;

  let to_string2 =
    test_case "to_string" `Quick (fun () ->
      let expr =
        n
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
          ]
      in
      let expected =
        "((4:name3:msp)(5:email12:msp@mail.com)(3:age2:30)(4:lang7:haskell)(14:social_account((7:twitter3:msp)(6:github5:mspwn)(8:mastodon16:msp@cargocut.org)(6:gitlab5:mspwn)(4:bsky7:msp.org)))(3:bio61:I \
         am a functional programmer based in Italy\n\
         test (a sub node)))"
      and computed = Csexp.to_string expr in
      check string "should be equal" expected computed)
  ;;
end

let cases = "Csexp (to_string)", [ to_string0; to_string1; to_string2 ]
