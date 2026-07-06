(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let is_positive0 =
    test_case "is_positive" `Quick (fun () ->
      let repr = Repr.int 16 in
      let expected = Ok 16
      and computed = Check.(int & Int.is_positive) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let is_positive1 =
    test_case "is_positive" `Quick (fun () ->
      let repr = Repr.int 0 in
      let expected = Ok 0
      and computed = Check.(int & Int.is_positive) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let is_positive2 =
    test_case "is_positive" `Quick (fun () ->
      let repr = Repr.int (-89) in
      let expected = Check.fail_with ~value:repr "`-89` is not positive"
      and computed = Check.(int & Int.is_positive) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let is_negative0 =
    test_case "is_negative" `Quick (fun () ->
      let repr = Repr.int (-16) in
      let expected = Ok (-16)
      and computed = Check.(int & Int.is_negative) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let is_negative1 =
    test_case "is_negative" `Quick (fun () ->
      let repr = Repr.int 16 in
      let expected = Check.fail_with ~value:repr "`16` is not negative"
      and computed = Check.(int & Int.is_negative) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let is_negative2 =
    test_case "is_negative" `Quick (fun () ->
      let repr = Repr.int 0 in
      let expected = Check.fail_with ~value:repr "`0` is not negative"
      and computed = Check.(int & Int.is_negative) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let is_even0 =
    test_case "is_even" `Quick (fun () ->
      let repr = Repr.int 16 in
      let expected = Ok 16
      and computed = Check.(int & Int.is_even) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let is_even1 =
    test_case "is_even" `Quick (fun () ->
      let repr = Repr.int (-16) in
      let expected = Ok (-16)
      and computed = Check.(int & Int.is_even) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let is_even2 =
    test_case "is_even" `Quick (fun () ->
      let repr = Repr.int 17 in
      let expected = Check.fail_with ~value:repr "`17` is not even"
      and computed = Check.(int & Int.is_even) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let is_even3 =
    test_case "is_even" `Quick (fun () ->
      let repr = Repr.int 0 in
      let expected = Ok 0
      and computed = Check.(int & Int.is_even) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let is_odd0 =
    test_case "is_odd" `Quick (fun () ->
      let repr = Repr.int 17 in
      let expected = Ok 17
      and computed = Check.(int & Int.is_odd) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let is_odd1 =
    test_case "is_odd" `Quick (fun () ->
      let repr = Repr.int 16 in
      let expected = Check.fail_with ~value:repr "`16` is not odd"
      and computed = Check.(int & Int.is_odd) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let is_odd2 =
    test_case "is_odd" `Quick (fun () ->
      let repr = Repr.int 0 in
      let expected = Check.fail_with ~value:repr "`0` is not odd"
      and computed = Check.(int & Int.is_odd) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;
end

let cases =
  ( "Check (Numeric guards)"
  , [ is_positive0
    ; is_positive1
    ; is_positive2
    ; is_negative0
    ; is_negative1
    ; is_negative2
    ; is_even0
    ; is_even1
    ; is_even2
    ; is_even3
    ; is_odd0
    ; is_odd1
    ; is_odd2
    ] )
;;
