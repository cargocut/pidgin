(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)
  open Alcotest

  let int32_from_int =
    test_case "int32 from int" `Quick (fun () ->
      let expected = Ok 42l
      and computed = Check.int32 (Repr.int 42) in
      check
        (Test_lib.Testable.checked int32)
        "should be equal"
        expected
        computed)
  ;;

  let int32_from_int32 =
    test_case "int32 from int32" `Quick (fun () ->
      let expected = Ok 42l
      and computed = Check.int32 (Repr.int32 42l) in
      check
        (Test_lib.Testable.checked int32)
        "should be equal"
        expected
        computed)
  ;;

  let int32_from_arbitary_value =
    test_case "int32 from arbitrary value" `Quick (fun () ->
      let repr = Repr.string "foo" in
      let expected =
        Error
          (Check.Unexpected_kind
             { expected = Kind.(or_ int (branch "int32" string))
             ; value = Repr.(sum (fun () -> "foo", null ()) ())
             ; given = Kind.(branch "foo" null)
             })
      and computed = Check.int32 repr in
      check
        (Test_lib.Testable.checked int32)
        "should be equal"
        expected
        computed)
  ;;

  let int64_from_int =
    test_case "int64 from int" `Quick (fun () ->
      let expected = Ok 42L
      and computed = Check.int64 (Repr.int 42) in
      check
        (Test_lib.Testable.checked int64)
        "should be equal"
        expected
        computed)
  ;;

  let int64_from_int32 =
    test_case "int64 from int32" `Quick (fun () ->
      let expected = Ok 42L
      and computed = Check.int64 (Repr.int32 42l) in
      check
        (Test_lib.Testable.checked int64)
        "should be equal"
        expected
        computed)
  ;;

  let int64_from_int64 =
    test_case "int64 from int64" `Quick (fun () ->
      let expected = Ok 42L
      and computed = Check.int64 (Repr.int64 42L) in
      check
        (Test_lib.Testable.checked int64)
        "should be equal"
        expected
        computed)
  ;;

  let int64_from_arbitary_value =
    test_case "int64 from arbitrary value" `Quick (fun () ->
      let repr = Repr.string "foo" in
      let expected =
        Error
          (Check.Unexpected_kind
             { expected =
                 Kind.(
                   or_ int (or_ (branch "int64" string) (branch "int32" string)))
             ; value = Repr.(sum (fun () -> "foo", null ()) ())
             ; given = Kind.(branch "foo" null)
             })
      and computed = Check.int64 repr in
      check
        (Test_lib.Testable.checked int64)
        "should be equal"
        expected
        computed)
  ;;

  let int64_from_lookalike_64 =
    test_case "int64 from a look a like int64" `Quick (fun () ->
      let repr = Repr.sum (fun () -> "int64", Repr.string "foo bar baz") () in
      let expected =
        Error
          (Check.Unexpected_value
             { value = Some repr; message = "int64 expected" })
      and computed = Check.int64 repr in
      check
        (Test_lib.Testable.checked int64)
        "should be equal"
        expected
        computed)
  ;;

  let is_odd_even1 =
    test_case "check if a negative number is odd or even" `Quick (fun () ->
      let repr = Repr.[ int 12; int 33; int (-2); int (-3) ] in
      let expected =
        ( [ Ok 12
          ; Error
              (Check.Unexpected_value
                 { value = Some (Repr.int 33); message = "`33` is not even" })
          ; Ok (-2)
          ; Error
              (Check.Unexpected_value
                 { value = Some (Repr.int (-3)); message = "`-3` is not even" })
          ]
        , [ Error
              (Check.Unexpected_value
                 { value = Some (Repr.int 12); message = "`12` is not odd" })
          ; Ok 33
          ; Error
              (Check.Unexpected_value
                 { value = Some (Repr.int (-2)); message = "`-2` is not odd" })
          ; Ok (-3)
          ] )
      and computed =
        ( List.map Check.(int & Int.is_even) repr
        , List.map Check.(int & Int.is_odd) repr )
      in
      check
        (pair
           (list @@ Test_lib.Testable.checked int)
           (list @@ Test_lib.Testable.checked int))
        "should be equal"
        expected
        computed)
  ;;
end

let cases =
  ( "Check (Numeric)"
  , [ int32_from_int
    ; int32_from_int32
    ; int32_from_arbitary_value
    ; int64_from_int
    ; int64_from_int32
    ; int64_from_int64
    ; int64_from_arbitary_value
    ; int64_from_lookalike_64
    ; is_odd_even1
    ] )
;;
