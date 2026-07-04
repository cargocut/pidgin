(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let nel_of0 =
    test_case "nel_of" `Quick (fun () ->
      let expected = Ok Nel.[ 1; 2; 3 ]
      and computed = Check.(nel_of int) Repr.(nel_of int [ 1; 2; 3 ]) in
      check
        Test_lib.Testable.(checked @@ nel int)
        "should be equal"
        expected
        computed)
  ;;

  let nel_of1 =
    test_case "nel_of" `Quick (fun () ->
      let repr = Repr.(nel [ int 1; string "test"; int 3 ]) in
      let expected =
        let open Check in
        Error
          (Invalid_list
             { errors =
                 Nel.
                   [ ( 1
                     , Unexpected_kind
                         { expected = Kind.int
                         ; given = Kind.string
                         ; value = Repr.string "test"
                         } )
                   ]
             ; value = repr
             })
      and computed = Check.(nel_of int) repr in
      check
        Test_lib.Testable.(checked @@ nel int)
        "should be equal"
        expected
        computed)
  ;;
end

let cases = "Check (Specific)", [ nel_of0; nel_of1 ]
