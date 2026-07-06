(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let is_digit0 =
    test_case "is_digit" `Quick (fun () ->
      let repr = Repr.char '0' in
      let expected = Ok '0'
      and computed = Check.(char & Char.is_digit) repr in
      check (Test_lib.Testable.checked char) "should be equal" expected computed)
  ;;

  let is_digit1 =
    test_case "is_digit" `Quick (fun () ->
      let repr =
        Repr.list_of
          Repr.char
          [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]
      in
      let expected = Ok [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]
      and computed = Check.(list_of (char & Char.is_digit)) repr in
      check
        (Test_lib.Testable.checked @@ list char)
        "should be equal"
        expected
        computed)
  ;;

  let is_digit2 =
    test_case "is_digit" `Quick (fun () ->
      let repr = Repr.char 'a' in
      let expected =
        Check.fail_with
          ~value:repr
          "`a` is not included into `[0; 1; 2; 3; 4; 5; 6; 7; 8; 9]`"
      and computed = Check.(char & Char.is_digit) repr in
      check (Test_lib.Testable.checked char) "should be equal" expected computed)
  ;;

  let is_hex_digit0 =
    test_case "is_hex_digit" `Quick (fun () ->
      let repr =
        Repr.list_of
          Repr.char
          [ '0'
          ; '1'
          ; '2'
          ; '3'
          ; '4'
          ; '5'
          ; '6'
          ; '7'
          ; '8'
          ; '9'
          ; 'a'
          ; 'b'
          ; 'c'
          ; 'd'
          ; 'e'
          ; 'f'
          ; 'A'
          ; 'B'
          ; 'C'
          ; 'D'
          ; 'E'
          ; 'F'
          ]
      in
      let expected =
        Ok
          [ '0'
          ; '1'
          ; '2'
          ; '3'
          ; '4'
          ; '5'
          ; '6'
          ; '7'
          ; '8'
          ; '9'
          ; 'a'
          ; 'b'
          ; 'c'
          ; 'd'
          ; 'e'
          ; 'f'
          ; 'A'
          ; 'B'
          ; 'C'
          ; 'D'
          ; 'E'
          ; 'F'
          ]
      and computed = Check.(list_of (char & Char.is_hex_digit)) repr in
      check
        (Test_lib.Testable.checked @@ list char)
        "should be equal"
        expected
        computed)
  ;;

  let is_hex_digit1 =
    test_case "is_hex_digit1" `Quick (fun () ->
      let repr = Repr.char 'G' in
      let expected =
        Check.fail_with
          ~value:repr
          "`G` is not included into `[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; a; b; c; \
           d; e; f; A; B; C; D; E; F]`"
      and computed = Check.(char & Char.is_hex_digit) repr in
      check (Test_lib.Testable.checked char) "should be equal" expected computed)
  ;;
end

let cases =
  ( "Check (Char)"
  , [ is_digit0; is_digit1; is_digit2; is_hex_digit0; is_hex_digit1 ] )
;;
