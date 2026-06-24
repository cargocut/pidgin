(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let infer_null0 =
    test_case "infer" `Quick (fun () ->
      let expected = Kind.null
      and computed = Kind.infer (Repr.null ()) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let infer_bool0 =
    test_case "infer" `Quick (fun () ->
      let expected = Kind.bool
      and computed = Kind.infer (Repr.bool true) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let infer_bool1 =
    test_case "infer" `Quick (fun () ->
      let expected = Kind.bool
      and computed = Kind.infer (Repr.bool false) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let infer_int0 =
    test_case "infer" `Quick (fun () ->
      let expected = Kind.int
      and computed = Kind.infer (Repr.int 42) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let infer_float0 =
    test_case "infer" `Quick (fun () ->
      let expected = Kind.float
      and computed = Kind.infer (Repr.float 42.3) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let infer_string0 =
    test_case "infer" `Quick (fun () ->
      let expected = Kind.string
      and computed = Kind.infer (Repr.string "pidgin") in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let infer_list0 =
    test_case "infer" `Quick (fun () ->
      let expected = Kind.(list string)
      and computed = Kind.infer Repr.(list_of string [ "foo"; "bar"; "baz" ]) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let infer_list1 =
    test_case "infer" `Quick (fun () ->
      let expected =
        (* NOTE: Even if the list producer known that we are building
           a list of int. The language does not hold type ascription,
           so this why we infer [any]. *)
        Kind.(list any)
      and computed = Kind.infer Repr.(list_of string []) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let infer_list3 =
    test_case "infer" `Quick (fun () ->
      let expected = Kind.(list (list (tuple Nel.(int :: string :: [ bool ]))))
      and computed =
        Kind.infer
          Repr.(
            list_of
              (list_of (triple int string bool))
              [ [ 1, "foo", true ]; [ 2, "bar", false; 3, "foobar", true ] ])
      in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let infer_list4 =
    test_case "infer" `Quick (fun () ->
      let expected = Kind.(list (list (triple int string bool)))
      and computed =
        Kind.infer
          Repr.(
            list_of
              (list_of (triple int string bool))
              [ [ 1, "foo", true ]; [ 2, "bar", false; 3, "foobar", true ] ])
      in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let infer_pair0 =
    test_case "infer" `Quick (fun () ->
      let expected = Kind.(pair int bool)
      and computed = Kind.infer Repr.(pair int bool (1, true)) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let infer_result0 =
    test_case "infer" `Quick (fun () ->
      let expected = Kind.(branch "ok" int)
      and computed = Kind.infer Repr.(result ~ok:int ~error:bool (Ok 10)) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let infer_result1 =
    test_case "infer" `Quick (fun () ->
      let expected = Kind.(branch "error" bool)
      and computed =
        Kind.infer Repr.(result ~ok:int ~error:bool (Error true))
      in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let infer_record0 =
    test_case "infer" `Quick (fun () ->
      let expected = Kind.(record [])
      and computed = Kind.infer Repr.(record []) in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;

  let infer_record1 =
    test_case "infer" `Quick (fun () ->
      let expected =
        Kind.(
          record
            [ "foo", string
            ; "policies", list int
            ; "unified_list", list any
            ; "is_valid", bool
            ; "state", branch "ok" int
            ; ( "more"
              , record
                  [ ( "things"
                    , list
                        (or_
                           (pair int (branch "left" string))
                           (pair int (branch "right" int))) )
                  ] )
            ])
      and computed =
        Kind.infer
          Repr.(
            record
              [ "foo", string "hello"
              ; "policies", list_of int [ 1; 2; 3; 4 ]
              ; "unified_list", list []
              ; "is_valid", bool true
              ; "state", result ~ok:int ~error:string (Ok 12)
              ; ( "more"
                , record
                    [ ( "things"
                      , list_of
                          (pair int (either ~left:string ~right:int))
                          [ 1, Right 12; 2, Left "foo" ] )
                    ] )
              ])
      in
      check Test_lib.Testable.kind "should be equal" expected computed)
  ;;
end

let cases =
  ( "Kind inference"
  , [ infer_null0
    ; infer_bool0
    ; infer_bool1
    ; infer_int0
    ; infer_float0
    ; infer_string0
    ; infer_list0
    ; infer_list1
    ; infer_list3
    ; infer_list4
    ; infer_pair0
    ; infer_result0
    ; infer_result1
    ; infer_record0
    ; infer_record1
    ] )
;;
