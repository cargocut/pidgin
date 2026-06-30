(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

let () =
  Alcotest.run
    "Pidgin main suite"
    [ Repr_test.cases
    ; Repr_desugaring_test.cases
    ; Kind_test.cases
    ; Kind_inference_test.cases
    ; Check_simple_test.cases
    ; Check_list_test.cases
    ; Check_record_test.cases
    ; Check_corner_cases.cases
    ]
;;
