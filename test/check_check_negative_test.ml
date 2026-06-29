(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open Alcotest

open struct
  (* Basic positive cases *)

  let null =
    test_case "|- null : Int" `Quick (fun () ->
      let repr = Repr.null () in
      let kind = Kind.int in
      let expected = Error (Error.unexpected_kind kind repr)
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let bool =
    test_case "|- true : Iool" `Quick (fun () ->
      let repr = Repr.bool true in
      let kind = Kind.int in
      let expected = Error (Error.unexpected_kind kind repr)
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let int =
    test_case "|- 42 : Null" `Quick (fun () ->
      let repr = Repr.int 42 in
      let kind = Kind.null in
      let expected = Error (Error.unexpected_kind kind repr)
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let float =
    test_case "|- 42.0 : Int" `Quick (fun () ->
      let repr = Repr.float 42.0 in
      let kind = Kind.int in
      let expected = Error (Error.unexpected_kind kind repr)
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let string =
    test_case "|- \"hello\" : Int" `Quick (fun () ->
      let repr = Repr.string "Hello" in
      let kind = Kind.int in
      let expected = Error (Error.unexpected_kind kind repr)
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  (* List positive cases *)

  let list_empty =
    test_case "|- [] : Int" `Quick (fun () ->
      let repr = Repr.list [] in
      let kind = Kind.int in
      let expected = Error (Error.unexpected_kind kind repr)
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let list_singleton =
    test_case "|- [ 42 ] : List String" `Quick (fun () ->
      let repr = Repr.list [ Repr.int 42 ] in
      let kind = Kind.list Kind.string in
      let expected =
        Error
          (Error.invalid_list
             repr
             (Nel.singleton
                (0, Error.unexpected_kind Kind.string (Repr.int 42))))
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let list_multiple =
    test_case "|- [ 42; 21.0 ] : List Int" `Quick (fun () ->
      let repr = Repr.list [ Repr.int 42; Repr.float 21.0 ] in
      let kind = Kind.list Kind.int in
      let expected =
        Error
          (Error.invalid_list
             repr
             (Nel.singleton
                (1, Error.unexpected_kind Kind.int (Repr.float 21.0))))
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  (* Record positive cases *)

  let record_empty =
    test_case "|- {} : Int " `Quick (fun () ->
      let repr = Repr.record [] in
      let kind = Kind.int in
      let expected = Error (Error.unexpected_kind kind repr)
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let record_singleton =
    test_case "|- { a = 42 } : Record { a : String } " `Quick (fun () ->
      let repr = Repr.record [ "a", Repr.int 42 ] in
      let kind = Kind.record [ "a", Kind.string ] in
      let expected =
        Error
          (Error.invalid_record
             repr
             (Error.invalid_field
                "a"
                (Error.unexpected_kind Kind.string (Repr.int 42))))
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let record_multiple_1 =
    test_case
      "|- { a = 42, b = null } : Record { a : Float, b : Bool } "
      `Quick
      (fun () ->
         let repr = Repr.record [ "a", Repr.int 42; "b", Repr.bool true ] in
         let kind = Kind.record [ "a", Kind.bool; "b", Kind.int ] in
         let expected =
           Error
             (Error.invalid_record
                repr
                (Nel.append
                   (Error.invalid_field
                      "a"
                      (Error.unexpected_kind Kind.bool (Repr.int 42)))
                   (Error.invalid_field
                      "b"
                      (Error.unexpected_kind Kind.int (Repr.bool true)))))
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
         let kind = Kind.record [ "b", Kind.int; "a", Kind.bool ] in
         let expected =
           Error
             (Error.invalid_record
                repr
                (Nel.append
                   (Error.invalid_field
                      "a"
                      (Error.unexpected_kind Kind.bool (Repr.int 42)))
                   (Error.invalid_field
                      "b"
                      (Error.unexpected_kind Kind.int (Repr.bool true)))))
         and computed = Check.check repr kind in
         check
           (Test_lib.Testable.checked unit)
           "should be equal"
           expected
           computed)
  ;;

  (* Alternatives *)

  let string_choice =
    test_case "|- \"hello\" : Float | Int" `Quick (fun () ->
      let repr = Repr.string "Hello" in
      let kind = Kind.or_ Kind.float Kind.int in
      let expected = Error (Error.unexpected_kind kind repr)
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;
end

open struct
  (* Denoted data type *)
  let constructor =
    test_case "|- answer 42 : #answer<Bool>" `Quick (fun () ->
      let repr =
        Repr.Record [ "value", Repr.bool true; "constr", Repr.String "answer" ]
      in
      let kind = Kind.branch "answer" Kind.int in
      let expected =
        Error
          (Error.invalid_constructor
             repr
             (Error.unexpected_kind Kind.int (Repr.bool true)))
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;

  let pair =
    test_case "|- (true, \"Hello\") : (Int * String)" `Quick (fun () ->
      let repr =
        Repr.Record [ "first", Repr.bool true; "second", Repr.String "answer" ]
      in
      let kind = Kind.pair Kind.int Kind.string in
      let expected =
        Error
          (Error.invalid_record
             repr
             (Error.invalid_field
                "first"
                (Error.unexpected_kind Kind.int (Repr.bool true))))
      and computed = Check.check repr kind in
      check (Test_lib.Testable.checked unit) "should be equal" expected computed)
  ;;
end

let cases =
  ( "Rejected Check Repr Kind"
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
    ; string_choice
    ; constructor
    ; pair
    ] )
;;
