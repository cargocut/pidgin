(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let sexp_to_repr0 =
    test_case "From Sexp To Repr" `Quick (fun () ->
      let expected = Repr.null ()
      and computed =
        let open Sexp in
        atom "null" |> translate_to_pidgin
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let sexp_to_repr1 =
    test_case "From Sexp To Repr" `Quick (fun () ->
      let expected = Repr.bool true
      and computed =
        let open Sexp in
        atom "true" |> translate_to_pidgin
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let sexp_to_repr2 =
    test_case "From Sexp To Repr" `Quick (fun () ->
      let expected = Repr.bool false
      and computed =
        let open Sexp in
        atom "false" |> translate_to_pidgin
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let sexp_to_repr3 =
    test_case "From Sexp To Repr" `Quick (fun () ->
      let expected = Repr.int 42
      and computed =
        let open Sexp in
        atom "42" |> translate_to_pidgin
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let sexp_to_repr4 =
    test_case "From Sexp To Repr" `Quick (fun () ->
      let expected = Repr.float 42.37
      and computed =
        let open Sexp in
        atom "42.37" |> translate_to_pidgin
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let sexp_to_repr5 =
    test_case "From Sexp To Repr" `Quick (fun () ->
      let expected = Repr.string "Hello World"
      and computed =
        let open Sexp in
        atom "Hello World" |> translate_to_pidgin
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let sexp_to_repr6 =
    test_case "From Sexp To Repr" `Quick (fun () ->
      let expected = Repr.string ""
      and computed =
        let open Sexp in
        atom "" |> translate_to_pidgin
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let sexp_to_repr7 =
    test_case "From Sexp To Repr" `Quick (fun () ->
      let expected = Repr.list_of Repr.int [ 1; 2; 3 ]
      and computed =
        let open Sexp in
        node [ atom "1"; atom "2"; atom "3" ] |> translate_to_pidgin
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let sexp_to_repr8 =
    test_case "From Sexp To Repr" `Quick (fun () ->
      let expected =
        let open Repr in
        list
          [ list [ string "foo"; list [ int 1 ] ]
          ; list [ string "bar"; int 2 ]
          ; list [ string "baz" ]
          ]
      and computed =
        let open Sexp in
        node
          [ node [ atom "foo"; node [ atom "1" ] ]
          ; node [ atom "bar"; atom "2" ]
          ; node [ atom "baz" ]
          ]
        |> translate_to_pidgin
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let sexp_to_repr9 =
    test_case "From Sexp To Repr" `Quick (fun () ->
      let expected =
        let open Repr in
        record
          [ "foo", list [ int 1 ]
          ; "bar", int 2
          ; "baz", record [ "flag", bool true; "message", string "Hello World" ]
          ]
      and computed =
        let open Sexp in
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
        |> translate_to_pidgin
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  open Test_lib.Archetypes

  let user0 =
    test_case "user" `Quick (fun () ->
      let repr =
        let open Repr in
        record
          [ "nickname", string "mspwn"
          ; "gender", string "male"
          ; "email", string "msp@domain.com"
          ; "level", int 10
          ]
        |> Sexp.translate_from_pidgin
        |> Sexp.translate_to_pidgin
      in
      let expected =
        User.make
          ~email:"msp@domain.com"
          ~level:10
          ~human:(Human.make ~nickname:"mspwn" ~gender:Gender.male ())
          ()
        |> Result.ok
      and computed = repr |> User.from_pidgin in
      check
        (Test_lib.Testable.checked User.testable)
        "should be equal"
        expected
        computed)
  ;;

  let int32 =
    test_case "From Yojson to Repr" `Quick (fun () ->
      let expected = Ok 145l
      and computed =
        Repr.int32 145l
        |> Driver.Sexp.translate_from_pidgin
        |> Driver.Sexp.translate_to_pidgin
        |> Check.int32
      in
      check
        Test_lib.Testable.(checked int32)
        "should be equal"
        expected
        computed)
  ;;

  let int64 =
    test_case "From Yojson to Repr" `Quick (fun () ->
      let expected = Ok 145L
      and computed =
        Repr.int64 145L
        |> Driver.Sexp.translate_from_pidgin
        |> Driver.Sexp.translate_to_pidgin
        |> Check.int64
      in
      check
        Test_lib.Testable.(checked int64)
        "should be equal"
        expected
        computed)
  ;;
end

let cases =
  ( "Repr <-> Sexp"
  , [ sexp_to_repr0
    ; sexp_to_repr1
    ; sexp_to_repr2
    ; sexp_to_repr3
    ; sexp_to_repr4
    ; sexp_to_repr5
    ; sexp_to_repr6
    ; sexp_to_repr7
    ; sexp_to_repr8
    ; sexp_to_repr9
    ; user0
    ; int32
    ; int64
    ] )
;;
