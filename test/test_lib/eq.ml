(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

let rec check_error a b =
  match a, b with
  | Check.Unexpected_kind { expected; given; value }, Check.Unexpected_kind b ->
    Kind.equal expected b.expected
    && Kind.equal given b.given
    && Repr.equal value b.value
  | Invalid_list { errors; value }, Invalid_list b ->
    Repr.equal value b.value
    && Nel.equal
         (fun (i, err) (i2, err2) -> Int.equal i i2 && check_error err err2)
         errors
         b.errors
  | Unexpected_value { value; message }, Unexpected_value b ->
    String.equal message b.message && Option.equal Repr.equal value b.value
  | Invalid_record { errors; value }, Invalid_record b ->
    Repr.equal value b.value && Nel.equal record_error errors b.errors
  | Unexpected_kind _, _
  | Invalid_list _, _
  | Invalid_record _, _
  | Unexpected_value _, _ -> false

and record_error a b =
  match a, b with
  | Invalid_subrecord a, Invalid_subrecord b -> check_error a b
  | Missing_field a, Missing_field b -> Nel.equal String.equal a b
  | Invalid_field { field; error }, Invalid_field b ->
    Nel.equal String.equal field b.field && check_error error b.error
  | Invalid_field _, _ | Missing_field _, _ | Invalid_subrecord _, _ -> false
;;
