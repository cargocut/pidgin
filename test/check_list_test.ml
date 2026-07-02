(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let list0 =
    test_case "list" `Quick (fun () ->
      let repr = Repr.(list_of string) [ "foo"; "bar" ] in
      let expected = Ok [ "foo"; "bar" ]
      and computed = repr |> Check.(list_of string) in
      check
        (Test_lib.Testable.checked (list string))
        "should be equal"
        expected
        computed)
  ;;

  let list1 =
    test_case "list" `Quick (fun () ->
      let repr = Repr.(list_of string) [] in
      let expected = Ok []
      and computed = repr |> Check.(list_of string) in
      check
        (Test_lib.Testable.checked (list string))
        "should be equal"
        expected
        computed)
  ;;

  let list2 =
    test_case "list" `Quick (fun () ->
      let repr = Repr.int 42 in
      let expected =
        Error
          (Check.Unexpected_kind
             { expected = Kind.(list any); given = Kind.int; value = repr })
      and computed = repr |> Check.(list_of string) in
      check
        (Test_lib.Testable.checked (list string))
        "should be equal"
        expected
        computed)
  ;;

  let list3 =
    test_case "list" `Quick (fun () ->
      let repr =
        Repr.(
          list_of
            (list_of int)
            [ [ 1 ]; [ 2; 3; 4; 5 ]; []; [ 6 ]; [ 7 ]; [ 8 ] ])
      in
      let expected = Ok [ [ 1 ]; [ 2; 3; 4; 5 ]; []; [ 6 ]; [ 7 ]; [ 8 ] ]
      and computed = repr |> Check.(list_of (list_of int)) in
      check
        (Test_lib.Testable.checked (list (list int)))
        "should be equal"
        expected
        computed)
  ;;

  let list4 =
    test_case "list" `Quick (fun () ->
      let repr = Repr.(list_of string) [ "foo"; "bar" ] in
      let expected =
        Error
          (Check.Invalid_list
             { value = repr
             ; errors =
                 Nel.from_list_exn
                   [ ( 0
                     , Check.Unexpected_kind
                         { expected = Kind.(list any)
                         ; value = Repr.string "foo"
                         ; given = Kind.string
                         } )
                   ; ( 1
                     , Check.Unexpected_kind
                         { expected = Kind.(list any)
                         ; value = Repr.string "bar"
                         ; given = Kind.string
                         } )
                   ]
             })
      and computed = repr |> Check.(list_of (list_of int)) in
      check
        (Test_lib.Testable.checked (list (list int)))
        "should be equal"
        expected
        computed)
  ;;
end

let cases = "Check (List)", [ list0; list1; list2; list3; list4 ]
