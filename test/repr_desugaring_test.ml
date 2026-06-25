(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let option0 =
    test_case "option" `Quick (fun () ->
      let expected = Repr.Int 42
      and computed = Repr.(option int (Some 42)) in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let option1 =
    test_case "option" `Quick (fun () ->
      let expected = Repr.Null
      and computed = Repr.(option int None) in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let result0 =
    test_case "result" `Quick (fun () ->
      let expected = Repr.Record [ "constr", String "ok"; "value", Int 42 ]
      and computed = Repr.(result ~ok:int ~error:string @@ Ok 42) in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let result1 =
    test_case "result" `Quick (fun () ->
      let expected =
        Repr.Record
          [ "constr", String "error"; "value", String "An error occured" ]
      and computed =
        Repr.(result ~ok:int ~error:string @@ Error "An error occured")
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let either0 =
    test_case "either" `Quick (fun () ->
      let expected = Repr.Record [ "constr", String "left"; "value", Int 42 ]
      and computed = Repr.(either ~left:int ~right:string @@ Left 42) in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let either1 =
    test_case "either" `Quick (fun () ->
      let expected =
        Repr.Record
          [ "constr", String "right"; "value", String "An error occured" ]
      and computed =
        Repr.(either ~left:int ~right:string @@ Right "An error occured")
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let pair0 =
    test_case "pair" `Quick (fun () ->
      let expected = Repr.Record [ "first", Int 42; "second", Bool true ]
      and computed = Repr.(pair int bool (42, true)) in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let triple0 =
    test_case "triple" `Quick (fun () ->
      let expected =
        Repr.Record
          [ "first", Int 42
          ; ( "second"
            , Record [ "first", Bool true; "second", String "Hello World" ] )
          ]
      and computed = Repr.(triple int bool string (42, true, "Hello World")) in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let int320 =
    test_case "int32" `Quick (fun () ->
      let expected =
        Repr.Record [ "constr", String "int32"; "value", String "42" ]
      and computed = Repr.int32 42l in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let int640 =
    test_case "int64" `Quick (fun () ->
      let expected =
        Repr.Record [ "constr", String "int64"; "value", String "42" ]
      and computed = Repr.int64 42L in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;
end

let cases =
  ( "Repr (desugaring)"
  , [ option0
    ; option1
    ; result0
    ; result1
    ; either0
    ; either1
    ; pair0
    ; triple0
    ; int320
    ; int640
    ] )
;;
