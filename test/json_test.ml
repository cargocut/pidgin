(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let yojson_to_repr0 =
    test_case "From Yojson to Repr" `Quick (fun () ->
      let expected = Repr.null ()
      and computed = `Null |> Driver.Yojson.translate_to_pidgin in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let ezjsonm_to_repr0 =
    test_case "From Ezjsonm to Repr" `Quick (fun () ->
      let expected = Repr.null ()
      and computed = `Null |> Driver.Ezjsonm.translate_to_pidgin in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let yojson_to_repr1 =
    test_case "From Yojson to Repr" `Quick (fun () ->
      let expected = Repr.bool true
      and computed = `Bool true |> Driver.Yojson.translate_to_pidgin in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let ezjsonm_to_repr1 =
    test_case "From Ezjsonm to Repr" `Quick (fun () ->
      let expected = Repr.bool true
      and computed = `Bool true |> Driver.Ezjsonm.translate_to_pidgin in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let yojson_to_repr2 =
    test_case "From Yojson to Repr" `Quick (fun () ->
      let expected = Repr.bool false
      and computed = `Bool false |> Driver.Yojson.translate_to_pidgin in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let ezjsonm_to_repr2 =
    test_case "From Ezjsonm to Repr" `Quick (fun () ->
      let expected = Repr.bool false
      and computed = `Bool false |> Driver.Ezjsonm.translate_to_pidgin in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let yojson_to_repr3 =
    test_case "From Yojson to Repr" `Quick (fun () ->
      let expected = Repr.int 42
      and computed = `Int 42 |> Driver.Yojson.translate_to_pidgin in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let ezjsonm_to_repr3 =
    test_case "From Ezjsonm to Repr" `Quick (fun () ->
      let expected = Repr.int 42
      and computed = `Float 42.0 |> Driver.Ezjsonm.translate_to_pidgin in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let yojson_to_repr4 =
    test_case "From Yojson to Repr" `Quick (fun () ->
      let expected = Repr.float 32.678
      and computed = `Float 32.678 |> Driver.Yojson.translate_to_pidgin in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let ezjsonm_to_repr4 =
    test_case "From Ezjsonm to Repr" `Quick (fun () ->
      let expected = Repr.float 32.678
      and computed = `Float 32.678 |> Driver.Ezjsonm.translate_to_pidgin in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let yojson_to_repr5 =
    test_case "From Yojson to Repr" `Quick (fun () ->
      let expected = Repr.string "Hello World"
      and computed =
        `String "Hello World" |> Driver.Yojson.translate_to_pidgin
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let ezjsonm_to_repr5 =
    test_case "From Ezjsonm to Repr" `Quick (fun () ->
      let expected = Repr.string "Hello World"
      and computed =
        `String "Hello World" |> Driver.Ezjsonm.translate_to_pidgin
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let yojson_to_repr6 =
    test_case "From Yojson to Repr" `Quick (fun () ->
      let expected = Repr.string ""
      and computed = `String "" |> Driver.Yojson.translate_to_pidgin in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let ezjsonm_to_repr6 =
    test_case "From Ezjsonm to Repr" `Quick (fun () ->
      let expected = Repr.string ""
      and computed = `String "" |> Driver.Ezjsonm.translate_to_pidgin in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let yojson_to_repr7 =
    test_case "From Yojson to Repr" `Quick (fun () ->
      let expected = Repr.string ""
      and computed = `String "" |> Driver.Yojson.translate_to_pidgin in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let ezjsonm_to_repr7 =
    test_case "From Ezjsonm to Repr" `Quick (fun () ->
      let expected = Repr.string ""
      and computed = `String "" |> Driver.Ezjsonm.translate_to_pidgin in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let yojson_to_repr8 =
    test_case "From Yojson to Repr" `Quick (fun () ->
      let expected =
        let open Repr in
        list
          [ list [ string "foo"; list [ int 1 ] ]
          ; list [ string "bar"; int 2 ]
          ; list [ string "baz" ]
          ]
      and computed =
        `List
          [ `List [ `String "foo"; `List [ `Int 1 ] ]
          ; `List [ `String "bar"; `Int 2 ]
          ; `List [ `String "baz" ]
          ]
        |> Driver.Yojson.translate_to_pidgin
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let ezjsonm_to_repr8 =
    test_case "From Ezjsonm to Repr" `Quick (fun () ->
      let expected =
        let open Repr in
        list
          [ list [ string "foo"; list [ int 1 ] ]
          ; list [ string "bar"; int 2 ]
          ; list [ string "baz" ]
          ]
      and computed =
        `A
          [ `A [ `String "foo"; `A [ `Float 1.0 ] ]
          ; `A [ `String "bar"; `Float 2.0 ]
          ; `A [ `String "baz" ]
          ]
        |> Driver.Ezjsonm.translate_to_pidgin
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let yojson_to_repr9 =
    test_case "From Yojson to Repr" `Quick (fun () ->
      let expected =
        let open Repr in
        record
          [ "foo", list [ int 1 ]
          ; "bar", int 2
          ; "baz", record [ "flag", bool true; "message", string "Hello World" ]
          ]
      and computed =
        `Assoc
          [ "foo", `List [ `Int 1 ]
          ; "bar", `Int 2
          ; ( "baz"
            , `Assoc [ "flag", `Bool true; "message", `String "Hello World" ] )
          ]
        |> Driver.Yojson.translate_to_pidgin
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let ezjsonm_to_repr9 =
    test_case "From Ezjsonm to Repr" `Quick (fun () ->
      let expected =
        let open Repr in
        record
          [ "foo", list [ int 1 ]
          ; "bar", int 2
          ; "baz", record [ "flag", bool true; "message", string "Hello World" ]
          ]
      and computed =
        `O
          [ "foo", `A [ `Float 1.0 ]
          ; "bar", `Float 2.0
          ; "baz", `O [ "flag", `Bool true; "message", `String "Hello World" ]
          ]
        |> Driver.Ezjsonm.translate_to_pidgin
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  open Test_lib.Archetypes

  let user_yojson0 =
    test_case "user" `Quick (fun () ->
      let repr =
        let open Repr in
        record
          [ "nickname", string "mspwn"
          ; "gender", string "male"
          ; "email", string "msp@domain.com"
          ; "level", int 10
          ]
        |> Driver.Yojson.translate_from_pidgin
        |> Driver.Yojson.translate_to_pidgin
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

  let user_ezjsonm0 =
    test_case "user" `Quick (fun () ->
      let repr =
        let open Repr in
        record
          [ "nickname", string "mspwn"
          ; "gender", string "male"
          ; "email", string "msp@domain.com"
          ; "level", int 10
          ]
        |> Driver.Yojson.translate_from_pidgin
        |> Driver.Yojson.translate_to_pidgin
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
end

let cases =
  ( "Repr <-> Json"
  , [ yojson_to_repr0
    ; ezjsonm_to_repr0
    ; yojson_to_repr1
    ; ezjsonm_to_repr1
    ; yojson_to_repr2
    ; ezjsonm_to_repr2
    ; yojson_to_repr3
    ; ezjsonm_to_repr3
    ; yojson_to_repr4
    ; ezjsonm_to_repr4
    ; yojson_to_repr5
    ; ezjsonm_to_repr5
    ; yojson_to_repr6
    ; ezjsonm_to_repr6
    ; yojson_to_repr7
    ; ezjsonm_to_repr7
    ; yojson_to_repr8
    ; ezjsonm_to_repr8
    ; yojson_to_repr9
    ; ezjsonm_to_repr9
    ; user_yojson0
    ; user_ezjsonm0
    ] )
;;
