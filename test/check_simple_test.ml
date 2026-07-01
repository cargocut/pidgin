(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let null0 =
    test_case "null" `Quick (fun () ->
      let expected = Ok ()
      and computed = Repr.null () |> Check.null in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let null1 =
    test_case "null" `Quick (fun () ->
      let repr = Repr.int 65 in
      let expected = Error (Error.unexpected_kind Kind.null repr)
      and computed = repr |> Check.null in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let bool0 =
    test_case "bool" `Quick (fun () ->
      let repr = Repr.bool true in
      let expected = Ok true
      and computed = repr |> Check.bool in
      check (Test_lib.Testable.checked bool) "should be equal" expected computed)
  ;;

  let bool1 =
    test_case "bool" `Quick (fun () ->
      let repr = Repr.bool false in
      let expected = Ok false
      and computed = repr |> Check.bool in
      check (Test_lib.Testable.checked bool) "should be equal" expected computed)
  ;;

  let bool2 =
    test_case "bool" `Quick (fun () ->
      let repr = Repr.int 65 in
      let expected = Error (Error.unexpected_kind Kind.bool repr)
      and computed = repr |> Check.bool in
      check (Test_lib.Testable.checked bool) "should be equal" expected computed)
  ;;

  let int0 =
    test_case "int" `Quick (fun () ->
      let repr = Repr.int 45 in
      let expected = Ok 45
      and computed = repr |> Check.int in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let int1 =
    test_case "int" `Quick (fun () ->
      let repr = Repr.int (-48) in
      let expected = Ok (-48)
      and computed = repr |> Check.int in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let int2 =
    test_case "int" `Quick (fun () ->
      let repr = Repr.(list_of int) [ 65 ] in
      let expected = Error (Error.unexpected_kind Kind.int repr)
      and computed = repr |> Check.int in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let float0 =
    test_case "float" `Quick (fun () ->
      let repr = Repr.float 45.7 in
      let expected = Ok 45.7
      and computed = repr |> Check.float in
      check
        (Test_lib.Testable.checked (float 0.0))
        "should be equal"
        expected
        computed)
  ;;

  let float1 =
    test_case "float" `Quick (fun () ->
      let repr = Repr.float (-48.54) in
      let expected = Ok (-48.54)
      and computed = repr |> Check.float in
      check
        (Test_lib.Testable.checked (float 0.0))
        "should be equal"
        expected
        computed)
  ;;

  let float2 =
    test_case "float" `Quick (fun () ->
      let repr = Repr.(list_of float) [ 65.76 ] in
      let expected = Error (Error.unexpected_kind Kind.float repr)
      and computed = repr |> Check.float in
      check
        (Test_lib.Testable.checked (float 0.0))
        "should be equal"
        expected
        computed)
  ;;

  let string0 =
    test_case "string" `Quick (fun () ->
      let repr = Repr.string "Hello World" in
      let expected = Ok "Hello World"
      and computed = repr |> Check.string in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let string1 =
    test_case "string" `Quick (fun () ->
      let repr = Repr.string "" in
      let expected = Ok ""
      and computed = repr |> Check.string in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let string2 =
    test_case "string" `Quick (fun () ->
      let repr = Repr.int 33 in
      let expected = Error (Error.unexpected_kind Kind.string repr)
      and computed = repr |> Check.string in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let char0 =
    test_case "char" `Quick (fun () ->
      let repr = Repr.string "H" in
      let expected = Ok 'H'
      and computed = repr |> Check.char in
      check (Test_lib.Testable.checked char) "should be equal" expected computed)
  ;;

  let char1 =
    test_case "char" `Quick (fun () ->
      let repr = Repr.int 97 in
      let expected = Ok 'a'
      and computed = repr |> Check.char in
      check (Test_lib.Testable.checked char) "should be equal" expected computed)
  ;;

  let char2 =
    test_case "char" `Quick (fun () ->
      let repr = Repr.string "Hello World" in
      let expected = Error (Error.unexpected_value ~value:repr "char expected")
      and computed = repr |> Check.char in
      check (Test_lib.Testable.checked char) "should be equal" expected computed)
  ;;
end

let cases =
  ( "Check (Simple)"
  , [ null0
    ; null1
    ; bool0
    ; bool1
    ; bool2
    ; int0
    ; int1
    ; int2
    ; float0
    ; float1
    ; float2
    ; string0
    ; string1
    ; string2
    ; char0
    ; char1
    ; char2
    ] )
;;
