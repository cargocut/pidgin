(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let unify0 =
    test_case "unify" `Quick (fun () ->
      let expected = Kind.bool
      and computed = Kind.unify [ Kind.bool ] in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let unify1 =
    test_case "unify" `Quick (fun () ->
      let expected = Kind.bool
      and computed = Kind.(unify [ bool; bool ]) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let unify2 =
    test_case "unify" `Quick (fun () ->
      let expected = Kind.any
      and computed = Kind.(unify [ bool; bool; string; any ]) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let unify3 =
    test_case "unify2" `Quick (fun () ->
      let expected = Kind.(or_ bool (or_ string (or_ int null)))
      and computed = Kind.(unify [ bool; bool; string; int; string; null ]) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let unify4 =
    test_case "unify" `Quick (fun () ->
      let expected = Kind.(unify [ bool; string; int; or_ int string ])
      and computed = Kind.(unify [ bool; bool; string; int; or_ int string ]) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let sum0 =
    test_case "sum" `Quick (fun () ->
      let expected = Kind.(branch "foo" bool)
      and computed = Kind.(sum [ "foo", bool ]) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let sum1 =
    test_case "sum" `Quick (fun () ->
      let expected = Kind.(unify [ branch "foo" bool; branch "bar" int ])
      and computed = Kind.(sum [ "foo", bool; "bar", int ]) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let sum2 =
    test_case "sum" `Quick (fun () ->
      let expected =
        Kind.(
          unify
            [ branch "foo" bool
            ; branch "bar" int
            ; branch
                "foobar"
                (record
                   [ "name", string
                   ; "policies", list (pair int (pair float string))
                   ])
            ])
      and computed =
        Kind.(
          sum
            [ "foo", bool
            ; "bar", int
            ; ( "foobar"
              , record
                  [ "name", string; "policies", list (triple int float string) ]
              )
            ])
      in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let tuple0 =
    test_case "tuple" `Quick (fun () ->
      let expected = Kind.(pair int (pair float (pair bool string)))
      and computed = Kind.(tuple [ int; float; bool; string ]) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let tuple1 =
    test_case "tuple" `Quick (fun () ->
      let expected = Kind.int
      and computed = Kind.(tuple [ int ]) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;
end

let cases =
  ( "Kind"
  , [ unify0; unify1; unify2; unify3; unify4; sum0; sum1; sum2; tuple0; tuple1 ]
  )
;;
