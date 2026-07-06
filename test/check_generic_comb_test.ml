(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let equal0 =
    test_case "equal" `Quick (fun () ->
      let repr = Repr.string "Hello World"
      and v = Check.equal "Hello World" in
      let expected = Ok "Hello World"
      and computed = Check.(string & v) repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let equal1 =
    test_case "equal" `Quick (fun () ->
      let repr = Repr.string "World"
      and v = Check.equal "Hello World" in
      let expected = Check.fail_with "The two values are not equal"
      and computed = Check.(string & v) repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let equal2 =
    test_case "equal" `Quick (fun () ->
      let repr = Repr.string "World"
      and v =
        Check.equal
          ~eq:Stdlib.String.equal
          ~to_repr:Repr.string
          ~to_string:Fun.id
          "Hello World"
      in
      let expected =
        Check.fail_with ~value:repr "`Hello World` is not equal to `World`"
      and computed = Check.(string & v) repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let not_equal0 =
    test_case "not_equal" `Quick (fun () ->
      let repr = Repr.string "World"
      and v = Check.not_equal "Hello World" in
      let expected = Ok "World"
      and computed = Check.(string & v) repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let not_equal1 =
    test_case "not_equal" `Quick (fun () ->
      let repr = Repr.string "Hello World"
      and v = Check.not_equal "Hello World" in
      let expected = Check.fail_with "The two values are equal"
      and computed = Check.(string & v) repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let not_equal2 =
    test_case "not_equal" `Quick (fun () ->
      let repr = Repr.string "Hello World"
      and v =
        Check.not_equal
          ~eq:Stdlib.String.equal
          ~to_repr:Repr.string
          ~to_string:Fun.id
          "Hello World"
      in
      let expected =
        Check.fail_with ~value:repr "`Hello World` is equal to `Hello World`"
      and computed = Check.(string & v) repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let gt0 =
    test_case "gt" `Quick (fun () ->
      let repr = Repr.int 42
      and v = Check.gt 40 in
      let expected = Ok 42
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let gt1 =
    test_case "gt" `Quick (fun () ->
      let repr = Repr.int 40
      and v = Check.gt 42 in
      let expected =
        Check.fail_with "The given value is not greater than the expected value"
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let gt2 =
    test_case "gt" `Quick (fun () ->
      let repr = Repr.int 40
      and v = Check.gt ~to_repr:Repr.int 42 in
      let expected = Check.fail_with ~value:repr "`40` is not greater than `42`"
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let gt3 =
    test_case "gt" `Quick (fun () ->
      let repr = Repr.int 42
      and v = Check.gt ~to_repr:Repr.int ~to_string:string_of_int 42 in
      let expected = Check.fail_with ~value:repr "`42` is not greater than `42`"
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let ge0 =
    test_case "ge" `Quick (fun () ->
      let repr = Repr.int 42
      and v = Check.ge 40 in
      let expected = Ok 42
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let ge1 =
    test_case "ge" `Quick (fun () ->
      let repr = Repr.int 42
      and v = Check.ge 42 in
      let expected = Ok 42
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let ge2 =
    test_case "ge" `Quick (fun () ->
      let repr = Repr.int 40
      and v = Check.ge 42 in
      let expected =
        Check.fail_with
          "The given value is noit greater or equal than the expected value"
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let ge3 =
    test_case "ge" `Quick (fun () ->
      let repr = Repr.int 40
      and v = Check.ge ~to_repr:Repr.int ~to_string:string_of_int 42 in
      let expected =
        Check.fail_with ~value:repr "`40` is not greater or equal than `42`"
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let lt0 =
    test_case "lt" `Quick (fun () ->
      let repr = Repr.int 30
      and v = Check.lt 40 in
      let expected = Ok 30
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let lt1 =
    test_case "lt" `Quick (fun () ->
      let repr = Repr.int 40
      and v = Check.lt 32 in
      let expected =
        Check.fail_with "The given value is not lower than the expected value"
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let lt2 =
    test_case "lt" `Quick (fun () ->
      let repr = Repr.int 40
      and v = Check.lt ~to_repr:Repr.int 32 in
      let expected = Check.fail_with ~value:repr "`40` is not lower than `32`"
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let lt3 =
    test_case "lt" `Quick (fun () ->
      let repr = Repr.int 42
      and v = Check.lt ~to_repr:Repr.int ~to_string:string_of_int 32 in
      let expected = Check.fail_with ~value:repr "`42` is not lower than `32`"
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let le0 =
    test_case "le" `Quick (fun () ->
      let repr = Repr.int 42
      and v = Check.le 50 in
      let expected = Ok 42
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let le1 =
    test_case "le" `Quick (fun () ->
      let repr = Repr.int 42
      and v = Check.le 42 in
      let expected = Ok 42
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let le2 =
    test_case "le" `Quick (fun () ->
      let repr = Repr.int 50
      and v = Check.le 42 in
      let expected =
        Check.fail_with
          "The given value is not lower or equal than the expected value"
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let le3 =
    test_case "le" `Quick (fun () ->
      let repr = Repr.int 50
      and v = Check.le ~to_repr:Repr.int ~to_string:string_of_int 42 in
      let expected =
        Check.fail_with ~value:repr "`50` is not lower or equal than `42`"
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let contains0 =
    test_case "contains0" `Quick (fun () ->
      let repr = Repr.int 30
      and v = Check.contains ~min:20 ~max:40 in
      let expected = Ok 30
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let contains1 =
    test_case "contains1" `Quick (fun () ->
      let repr = Repr.int 20
      and v = Check.contains ~min:20 ~max:40 in
      let expected = Ok 20
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let contains2 =
    test_case "contains2" `Quick (fun () ->
      let repr = Repr.int 40
      and v = Check.contains ~min:20 ~max:40 in
      let expected = Ok 40
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let contains3 =
    test_case "contains2" `Quick (fun () ->
      let repr = Repr.int 10
      and v = Check.contains ~min:20 ~max:40 in
      let expected =
        Check.fail_with "The given value is not included in the given range"
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let contains4 =
    test_case "contains2" `Quick (fun () ->
      let repr = Repr.int 41
      and v = Check.contains ~to_repr:Repr.int ~min:20 ~max:40 in
      let expected =
        Check.fail_with
          ~value:repr
          "`41` is not included in the range [`20` .. `40`]"
      and computed = Check.(int & v) repr in
      check (Test_lib.Testable.checked int) "should be equal" expected computed)
  ;;

  let one_of0 =
    test_case "one_of" `Quick (fun () ->
      let repr = Repr.string "Hello World"
      and v = Check.one_of [ "foo"; "bar"; "baz"; "Hello World" ] in
      let expected = Ok "Hello World"
      and computed = Check.(string & v) repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let one_of1 =
    test_case "one_of" `Quick (fun () ->
      let repr = Repr.string "baz"
      and v =
        Check.one_of ~to_repr:Repr.string [ "foo"; "bar"; "baz"; "Hello World" ]
      in
      let expected = Ok "baz"
      and computed = Check.(string & v) repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;

  let one_of2 =
    test_case "one_of" `Quick (fun () ->
      let repr = Repr.string "not-present"
      and v =
        Check.one_of ~to_repr:Repr.string [ "foo"; "bar"; "baz"; "Hello World" ]
      in
      let expected =
        Check.fail_with
          ~value:repr
          {|`"not-present"` is not included into `["foo"; "bar"; "baz"; "Hello World"]`|}
      and computed = Check.(string & v) repr in
      check
        (Test_lib.Testable.checked string)
        "should be equal"
        expected
        computed)
  ;;
end

let cases =
  ( "Check (Generic combinators)"
  , [ equal0
    ; equal1
    ; equal2
    ; not_equal0
    ; not_equal1
    ; not_equal2
    ; gt0
    ; gt1
    ; gt2
    ; gt3
    ; ge0
    ; ge1
    ; ge2
    ; ge3
    ; lt0
    ; lt1
    ; lt2
    ; lt3
    ; le0
    ; le1
    ; le2
    ; le3
    ; contains0
    ; contains1
    ; contains2
    ; contains3
    ; contains4
    ; one_of0
    ; one_of1
    ; one_of2
    ] )
;;
