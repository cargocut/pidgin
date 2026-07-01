(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let req_with_opt0 =
    test_case "check if [req] on [option] works" `Quick (fun () ->
      let repr =
        let open Repr in
        record []
      in
      let v =
        let open Check in
        record (fun fields -> req fields "a_key" (option int))
      in
      let expected = Ok None
      and computed = v repr in
      check
        (Test_lib.Testable.checked (option int))
        "should be equal"
        expected
        computed)
  ;;

  let req_with_opt1 =
    test_case "check if [req] on [option] works" `Quick (fun () ->
      let repr =
        let open Repr in
        record [ "a_key", int 10; "b_key", null () ]
      in
      let v =
        let open Check in
        record (fun fields -> req fields "a_key" ~alt:[ "b_key" ] (option int))
      in
      let expected = Ok (Some 10)
      and computed = v repr in
      check
        (Test_lib.Testable.checked (option int))
        "should be equal"
        expected
        computed)
  ;;

  let req_with_opt2 =
    test_case "check if [req] on [option] works" `Quick (fun () ->
      let repr =
        let open Repr in
        record [ "b_key", int 10 ]
      in
      let v =
        let open Check in
        record (fun fields -> req fields "a_key" ~alt:[ "b_key" ] (option int))
      in
      let expected = Ok (Some 10)
      and computed = v repr in
      check
        (Test_lib.Testable.checked (option int))
        "should be equal"
        expected
        computed)
  ;;

  let req_with_opt3 =
    test_case "check if [req] on [option] works" `Quick (fun () ->
      let repr =
        let open Repr in
        record [ "a_key", null (); "b_key", int 10 ]
      in
      let v =
        let open Check in
        record (fun fields -> req fields "a_key" ~alt:[ "b_key" ] (option int))
      in
      let expected = Ok None
      and computed = v repr in
      check
        (Test_lib.Testable.checked (option int))
        "should be equal"
        expected
        computed)
  ;;
end

let cases =
  ( "Check (Corner Cases)"
  , [ req_with_opt0; req_with_opt1; req_with_opt2; req_with_opt3 ] )
;;
