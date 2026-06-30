(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let pair0 =
    test_case "pair" `Quick (fun () ->
      let repr = Repr.(pair int string) (42, "Hello World")
      and v = Check.(pair int string) in
      let expected = Ok (42, "Hello World")
      and computed = v repr in
      check
        (Test_lib.Testable.checked (pair int string))
        "should be equal"
        expected
        computed)
  ;;

  let pair1 =
    test_case "pair" `Quick (fun () ->
      let repr = Repr.List [ Int 42; String "Hello World" ]
      and v = Check.(pair int string) in
      let expected = Ok (42, "Hello World")
      and computed = v repr in
      check
        (Test_lib.Testable.checked (pair int string))
        "should be equal"
        expected
        computed)
  ;;

  let pair2 =
    test_case "pair" `Quick (fun () ->
      let repr = Repr.(pair int (option string)) (42, Some "Hello World")
      and v = Check.(pair int (option string)) in
      let expected = Ok (42, Some "Hello World")
      and computed = v repr in
      check
        (Test_lib.Testable.checked (pair int (option string)))
        "should be equal"
        expected
        computed)
  ;;

  let pair3 =
    test_case "pair" `Quick (fun () ->
      let repr = Repr.List [ Int 42 ]
      and v = Check.(pair int (option string)) in
      let expected = Ok (42, None)
      and computed = v repr in
      check
        (Test_lib.Testable.checked (pair int (option string)))
        "should be equal"
        expected
        computed)
  ;;

  let triple0 =
    test_case "triple" `Quick (fun () ->
      let repr = Repr.(triple int string bool) (42, "Hello World", true)
      and v = Check.(triple int string bool) in
      let expected = Ok (42, "Hello World", true)
      and computed = v repr in
      check
        (Test_lib.Testable.checked (triple int string bool))
        "should be equal"
        expected
        computed)
  ;;

  let triple1 =
    test_case "triple" `Quick (fun () ->
      let repr = Repr.List [ Int 42; String "Hello World" ]
      and v = Check.(triple int string (option bool)) in
      let expected = Ok (42, "Hello World", None)
      and computed = v repr in
      check
        (Test_lib.Testable.checked (triple int string (option bool)))
        "should be equal"
        expected
        computed)
  ;;
end

let cases = "Check (Product)", [ pair0; pair1; pair2; pair3; triple0; triple1 ]
