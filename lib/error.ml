(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

type for_value =
  | Unexpected_kind of
      { expected : Kind.t
      ; given : Kind.t
      ; value : Repr.t
      }
  | Invalid_list of
      { errors : (int * for_value) Nel.t
      ; value : Repr.t
      }
  | Invalid_record of
      { errors : for_record Nel.t
      ; value : Repr.t
      }
  | Unexpected_value of
      { value : Repr.t option
      ; message : string
      }

and for_record =
  | Invalid_field of
      { field : string Nel.t
      ; error : for_value
      }
  | Missing_field of string Nel.t
  | Invalid_subrecord of for_value

type sexp_parsing = Non_terminated_node of int

let rec equal_for_value a b =
  match a, b with
  | Unexpected_kind { expected; given; value }, Unexpected_kind b ->
    Kind.equal expected b.expected
    && Kind.equal given b.given
    && Repr.equal value b.value
  | Invalid_list { errors; value }, Invalid_list b ->
    Repr.equal value b.value
    && Nel.equal
         (fun (i, err) (i2, err2) -> Int.equal i i2 && equal_for_value err err2)
         errors
         b.errors
  | Unexpected_value { value; message }, Unexpected_value b ->
    String.equal message b.message && Option.equal Repr.equal value b.value
  | Invalid_record { errors; value }, Invalid_record b ->
    Repr.equal value b.value && Nel.equal equal_for_record errors b.errors
  | Unexpected_kind _, _
  | Invalid_list _, _
  | Invalid_record _, _
  | Unexpected_value _, _ -> false

and equal_for_record a b =
  match a, b with
  | Invalid_subrecord a, Invalid_subrecord b -> equal_for_value a b
  | Missing_field a, Missing_field b -> Nel.equal String.equal a b
  | Invalid_field { field; error }, Invalid_field b ->
    Nel.equal String.equal field b.field && equal_for_value error b.error
  | Invalid_field _, _ | Missing_field _, _ | Invalid_subrecord _, _ -> false
;;

let equal_for_sexp_parsing a b =
  match a, b with
  | Non_terminated_node a, Non_terminated_node b -> Int.equal a b
;;

let equal = equal_for_value

let unexpected_kind expected value =
  let given = Kind.infer value in
  Unexpected_kind { given; expected; value }
;;

let invalid_list value errors = Invalid_list { errors; value }

let missing_field ?(alt = []) field =
  Nel.singleton @@ Missing_field Nel.(make field alt)
;;

let invalid_field ?(alt = []) field error =
  Nel.singleton @@ Invalid_field { field = Nel.(make field alt); error }
;;

let invalid_record value errors = Invalid_record { errors; value }
let invalid_subrecord err = Nel.singleton @@ Invalid_subrecord err
let unexpected_value ?value message = Unexpected_value { value; message }
let non_terminated_node pos = Non_terminated_node pos
