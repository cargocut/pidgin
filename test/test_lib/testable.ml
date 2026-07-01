(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

let kind = Alcotest.testable Pp.kind Kind.equal
let repr = Alcotest.testable Pp.repr Repr.equal

let checked ok =
  let error = Alcotest.testable Pp.error_for_value Error.equal in
  Alcotest.result ok error
;;

let sexp_parsed =
  let error =
    Alcotest.testable Pp.error_for_sexp_parsing Error.equal_for_sexp_parsing
  and ok = Alcotest.testable Pp.sexp Sexp.equal in
  Alcotest.result ok error
;;
