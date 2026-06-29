(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open Alcotest

open struct
  (* Basic positive cases *)

  let null =
    test_case "|- null : Null" `Quick (fun () ->
      let repr = Repr.null () in
      let kind = Kind.null in
      let expected = Ok ()
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let bool =
    test_case "|- true : Bool" `Quick (fun () ->
      let repr = Repr.bool true in
      let kind = Kind.bool in
      let expected = Ok ()
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let int =
    test_case "|- 42 : Int" `Quick (fun () ->
      let repr = Repr.int 42 in
      let kind = Kind.int in
      let expected = Ok ()
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let float =
    test_case "|- 42.0 : Float" `Quick (fun () ->
      let repr = Repr.float 42.0 in
      let kind = Kind.float in
      let expected = Ok ()
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let string =
    test_case "|- \"hello\" : String" `Quick (fun () ->
      let repr = Repr.string "Hello" in
      let kind = Kind.string in
      let expected = Ok ()
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  (* List positive cases *)

  let list_empty =
    test_case "|- [] : List Int" `Quick (fun () ->
      let repr = Repr.list [] in
      let kind = Kind.list Kind.int in
      let expected = Ok ()
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let list_singleton =
    test_case "|- [ 42 ] : List Int" `Quick (fun () ->
      let repr = Repr.list [ Repr.int 42 ] in
      let kind = Kind.list Kind.int in
      let expected = Ok ()
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let list_multiple =
    test_case "|- [ 42; 21 ] : List Int" `Quick (fun () ->
      let repr = Repr.list [ Repr.int 42; Repr.int 21 ] in
      let kind = Kind.list Kind.int in
      let expected = Ok ()
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  (* Record positive cases *)

  let record_empty =
    test_case "|- {} : Record {} " `Quick (fun () ->
      let repr = Repr.record [] in
      let kind = Kind.record [] in
      let expected = Ok ()
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let record_singleton =
    test_case "|- { a = 42 } : Record { a : Int } " `Quick (fun () ->
      let repr = Repr.record [ "a", Repr.int 42 ] in
      let kind = Kind.record [ "a", Kind.int ] in
      let expected = Ok ()
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let record_multiple_1 =
    test_case
      "|- { a = 42, b = true } : Record { a : Int, b : Bool } "
      `Quick
      (fun () ->
         let repr = Repr.record [ "a", Repr.int 42; "b", Repr.bool true ] in
         let kind = Kind.record [ "a", Kind.int; "b", Kind.bool ] in
         let expected = Ok ()
         and computed = Check.check repr kind in
         check
           (Test_lib.Testable.checked unit)
           "should be equal"
           expected
           computed)
  ;;

  let record_multiple_2 =
    test_case
      "|- { a = 42, b = true } : Record { b : Bool, a : Int } "
      `Quick
      (fun () ->
         let repr = Repr.record [ "a", Repr.int 42; "b", Repr.bool true ] in
         let kind = Kind.record [ "b", Kind.bool; "a", Kind.int ] in
         let expected = Ok ()
         and computed = Check.check repr kind in
         check
           (Test_lib.Testable.checked unit)
           "should be equal"
           expected
           computed)
  ;;

  (* Alternatives *)

  let string_left =
    test_case "|- \"hello\" : String | Float" `Quick (fun () ->
      let repr = Repr.string "Hello" in
      let kind = Kind.or_ Kind.float Kind.string in
      let expected = Ok ()
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let string_right =
    test_case "|- \"hello\" : Float | String" `Quick (fun () ->
      let repr = Repr.string "Hello" in
      let kind = Kind.or_ Kind.float Kind.string in
      let expected = Ok ()
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;
end

open struct
  (* Denoted data type *)
  let constructor =
    test_case "|- answer 42 : #answer<Int>" `Quick (fun () ->
      let repr =
        Repr.Record [ "value", Repr.int 42; "constr", Repr.String "answer" ]
      in
      let kind = Kind.branch "answer" Kind.int in
      let expected = Ok ()
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let pair =
    test_case "|- (1, \"Hello\") : (Int * String)" `Quick (fun () ->
      let repr =
        Repr.Record [ "first", Repr.int 42; "second", Repr.String "answer" ]
      in
      let kind = Kind.pair Kind.int Kind.string in
      let expected = Ok ()
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;
end

let cases =
  ( "Accepted Check Repr Kind"
  , [ null
    ; bool
    ; int
    ; float
    ; string
    ; list_empty
    ; list_singleton
    ; list_multiple
    ; record_empty
    ; record_singleton
    ; record_multiple_1
    ; record_multiple_2
    ; string_left
    ; string_right
    ; constructor
    ; pair
    ] )
;;
