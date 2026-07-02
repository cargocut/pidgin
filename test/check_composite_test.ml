(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let result0 =
    test_case "result" `Quick (fun () ->
      let repr = Repr.(result ~ok:int ~error:string) (Ok 10)
      and v = Check.(result ~ok:int ~error:string) in
      let expected = Ok (Ok 10)
      and computed = v repr in
      check
        (Test_lib.Testable.checked (result int string))
        "should be equal"
        expected
        computed)
  ;;

  let result1 =
    test_case "result" `Quick (fun () ->
      let repr = Repr.(result ~ok:int ~error:string) (Error "Hello World")
      and v = Check.(result ~ok:int ~error:string) in
      let expected = Ok (Error "Hello World")
      and computed = v repr in
      check
        (Test_lib.Testable.checked (result int string))
        "should be equal"
        expected
        computed)
  ;;

  let result2 =
    test_case "result" `Quick (fun () ->
      let repr =
        Repr.(either ~left:int ~right:string) (Either.Right "Hello World")
      and v = Check.(result ~ok:int ~error:string) in
      let expected =
        Error
          (Check.Unexpected_kind
             { expected = Nel.(Kind.(sum (("ok", any) :: [ "error", any ])))
             ; value = repr
             ; given = Kind.(branch "right" string)
             })
      and computed = v repr in
      check
        (Test_lib.Testable.checked (result int string))
        "should be equal"
        expected
        computed)
  ;;

  let result3 =
    test_case "result" `Quick (fun () ->
      let repr = Repr.List [ String "ok"; Int 10 ]
      and v = Check.(result ~ok:int ~error:string) in
      let expected = Ok (Ok 10)
      and computed = v repr in
      check
        (Test_lib.Testable.checked (result int string))
        "should be equal"
        expected
        computed)
  ;;

  let result4 =
    test_case "result" `Quick (fun () ->
      let repr =
        Repr.(
          List
            [ List [ String "ok"; Int 10 ]
            ; List [ String "error" ]
            ; List [ String "error"; Null ]
            ; result ~ok:int ~error:null (Ok 42)
            ])
      and v = Check.(list_of (result ~ok:int ~error:null)) in
      let expected = Ok [ Ok 10; Error (); Error (); Ok 42 ]
      and computed = v repr in
      check
        (Test_lib.Testable.checked (list @@ result int unit))
        "should be equal"
        expected
        computed)
  ;;

  let sum0 =
    test_case "sum [] absurd" `Quick (fun () ->
      let repr = Repr.(result ~ok:int ~error:string) (Ok 10)
      and v = Check.(sum []) in
      let expected =
        Error
          (Check.Unexpected_kind
             { expected = Kind.record [ "absurd", Kind.any ]
             ; value = repr
             ; given = Kind.(branch "ok" int)
             })
      and computed = v repr in
      check
        (Test_lib.Testable.checked (result int string))
        "should be equal"
        expected
        computed)
  ;;
end

let cases =
  "Check (Composite)", [ result0; result1; result2; result3; result4; sum0 ]
;;
