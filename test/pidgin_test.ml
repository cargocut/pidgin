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
    ; Check_corner_cases_test.cases
    ; Check_composite_test.cases
    ; Check_product_test.cases
    ; Check_numeric_test.cases
    ; Check_numeric_gen_test.cases
    ; Check_specific_test.cases
    ; Check_generic_comb_test.cases
    ; Sexp_parser_test.cases
    ; Csexp_to_string_test.cases
    ; Csexp_parser_test.cases
    ; Sexp_repr_test.cases
    ; Json_test.cases
    ; Driver_simple_round_trip_test.cases
    ]
;;
