(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let not_empty0 =
    test_case "not_empty" `Quick (fun () ->
      let repr = Repr.string "Hello World" in
      let expected = Ok "Hello World"
      and computed = Check.(string & String.not_empty) repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let not_empty1 =
    test_case "not_empty" `Quick (fun () ->
      let repr = Repr.string "    \t  \t   " in
      let expected = Ok "    \t  \t   "
      and computed = Check.(string & String.not_empty) repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let not_empty2 =
    test_case "not_empty" `Quick (fun () ->
      let repr = Repr.string "" in
      let expected = Check.fail_with ~value:repr "the given string is empty"
      and computed = Check.(string & String.not_empty) repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let not_blank0 =
    test_case "not_empty" `Quick (fun () ->
      let repr = Repr.string "Hello World" in
      let expected = Ok "Hello World"
      and computed = Check.(string & String.not_blank) repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let not_blank1 =
    test_case "not_empty" `Quick (fun () ->
      let repr = Repr.string "    \t  \t   " in
      let expected = Check.fail_with ~value:repr "the given string is blank"
      and computed = Check.(string & String.not_blank) repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let has_length0 =
    test_case "has_length" `Quick (fun () ->
      let repr = Repr.string "foo" in
      let expected = Ok "foo"
      and computed = Check.(string & String.has_length 3) repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let has_length1 =
    test_case "not_empty" `Quick (fun () ->
      let repr = Repr.string "foo0" in
      let expected =
        Check.fail_with ~value:repr "`foo0` has length `4` and not `3`"
      and computed = Check.(string & String.has_length 3) repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let length_between0 =
    test_case "length_between" `Quick (fun () ->
      let repr = Repr.string "foo" in
      let expected = Ok "foo"
      and computed =
        Check.(string & String.length_between ~min:2 ~max:4) repr
      in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let length_between1 =
    test_case "length_between" `Quick (fun () ->
      let repr = Repr.string "fo" in
      let expected = Ok "fo"
      and computed =
        Check.(string & String.length_between ~min:2 ~max:4) repr
      in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let length_between2 =
    test_case "length_between" `Quick (fun () ->
      let repr = Repr.string "fooo" in
      let expected = Ok "fooo"
      and computed =
        Check.(string & String.length_between ~min:2 ~max:4) repr
      in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let length_between3 =
    test_case "length_between" `Quick (fun () ->
      let repr = Repr.string "f" in
      let expected =
        Check.fail_with
          ~value:repr
          "`f` has length `1` which is not greater or equal to `2`"
      and computed =
        Check.(string & String.length_between ~min:2 ~max:4) repr
      in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let length_between4 =
    test_case "length_between" `Quick (fun () ->
      let repr = Repr.string "foooo" in
      let expected =
        Check.fail_with
          ~value:repr
          "`foooo` has length `5` which is not lower or equal to `4`"
      and computed =
        Check.(string & String.length_between ~min:2 ~max:4) repr
      in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let has_prefix0 =
    test_case "has_prefix" `Quick (fun () ->
      let repr = Repr.string "Hello World" in
      let expected = Ok "Hello World"
      and computed = Check.(string & String.has_prefix "Hello") repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let has_prefix1 =
    test_case "has_prefix" `Quick (fun () ->
      let repr = Repr.string "Hello World" in
      let expected = Ok "Hello World"
      and computed = Check.(string & String.has_prefix "") repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let has_prefix2 =
    test_case "has_prefix" `Quick (fun () ->
      let repr = Repr.string "Hello World" in
      let expected =
        Check.fail_with
          ~value:repr
          "`Hello World` does not have the prefix `Mh`"
      and computed = Check.(string & String.has_prefix "Mh") repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let has_suffix0 =
    test_case "has_suffix" `Quick (fun () ->
      let repr = Repr.string "Hello World" in
      let expected = Ok "Hello World"
      and computed = Check.(string & String.has_suffix "World") repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let has_suffix1 =
    test_case "has_suffix" `Quick (fun () ->
      let repr = Repr.string "Hello World" in
      let expected = Ok "Hello World"
      and computed = Check.(string & String.has_suffix "") repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let has_suffix2 =
    test_case "has_suffix" `Quick (fun () ->
      let repr = Repr.string "Hello World" in
      let expected =
        Check.fail_with
          ~value:repr
          "`Hello World` does not have the suffix `Mh`"
      and computed = Check.(string & String.has_suffix "Mh") repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;
end

let cases =
  ( "Check (String)"
  , [ not_empty0
    ; not_empty1
    ; not_empty2
    ; not_blank0
    ; not_blank1
    ; has_length0
    ; has_length1
    ; length_between0
    ; length_between1
    ; length_between2
    ; length_between3
    ; length_between4
    ; has_prefix0
    ; has_prefix1
    ; has_prefix2
    ; has_suffix0
    ; has_suffix1
    ; has_suffix2
    ] )
;;
