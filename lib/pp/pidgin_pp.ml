(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open Pidgin

let rec repr ppf =
  let open Format in
  function
  | Repr.Null -> pp_print_string ppf "null"
  | Repr.Bool b -> pp_print_bool ppf b
  | Repr.Int i -> pp_print_int ppf i
  | Repr.Float i -> pp_print_float ppf i
  | Repr.String s -> fprintf ppf "%S" s
  | Repr.List list ->
    fprintf
      ppf
      "@[<hov 1>[%a]@]"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ",@ ") repr)
      list
  | Record fields ->
    fprintf
      ppf
      "@[<v 2>{%a}@]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ",@;")
         (fun ppf (key, value) -> fprintf ppf "@[<1>%S:@ %a@]" key repr value))
      fields
;;

let rec kind ppf =
  let open Format in
  function
  | Kind.Any -> pp_print_string ppf "any"
  | Kind.Null -> pp_print_string ppf "null"
  | Kind.Bool -> pp_print_string ppf "bool"
  | Kind.Int -> pp_print_string ppf "int"
  | Kind.Float -> pp_print_string ppf "float"
  | Kind.String -> pp_print_string ppf "string"
  | Kind.List t -> fprintf ppf "@[[%a]@]" kind t
  | Kind.Or l ->
    fprintf
      ppf
      "@[<hov>(%a)@]"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ | ") kind)
      l
  | Kind.Pair (a, b) -> fprintf ppf "@[(%a@ * %a)@]" kind a kind b
  | Kind.Branch (k, v) -> fprintf ppf "#%s<@[%a@]>" (Misc.strim k) kind v
  | Kind.Record fields ->
    fprintf
      ppf
      "@[<v 2>{%a}@]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ",@;")
         (fun ppf (key, value) -> fprintf ppf "@[<1>%S:@ %a@]" key kind value))
      fields
;;

let rec sexp ppf = function
  | Sexp.Atom x -> Format.pp_print_string ppf (Misc.escape_spaces x)
  | Sexp.Node xs -> Format.fprintf ppf "@[<hv 1>(%a)@]" sexp_list xs

and sexp_list ppf = function
  | [] -> ()
  | [ x ] -> sexp ppf x
  | x :: xs -> Format.fprintf ppf "%a@ %a" sexp x sexp_list xs
;;

let csexp ppf sexp = Format.pp_print_string ppf (Csexp.to_string sexp)
let p_string pp value = Conv.string (Format.asprintf "%a" pp value)

let rec value_error_to_repr = function
  | Check.Unexpected_value { value; message } ->
    let repr =
      let open Conv in
      record [ "value", option Fun.id value; "message", string message ]
    in
    "Unexpected_value", repr
  | Check.Unexpected_kind { expected; given; value } ->
    let repr =
      let open Conv in
      record
        [ "expected", p_string kind expected
        ; "given", p_string kind given
        ; "value", value
        ]
    in
    "Unexpected_kind", repr
  | Check.Invalid_list { errors; value } ->
    let repr =
      let open Conv in
      record
        [ ( "errors"
          , nel_of
              (fun (i, err) -> list [ int i; snd (value_error_to_repr err) ])
              errors )
        ; "value", value
        ]
    in
    "Invalid_list", repr
  | Check.Invalid_record { errors; value } ->
    let repr =
      let open Conv in
      record [ "errors", nel_of record_error_to_repr errors; "value", value ]
    in
    "Invalid_record", repr

and record_error_to_repr = function
  | Check.Invalid_subrecord error ->
    let open Conv in
    record
      [ "kind", string "invalid subrecord"
      ; "error", snd (value_error_to_repr error)
      ]
  | Check.Missing_field fields ->
    let open Conv in
    record [ "kind", string "missing field"; "field", nel_of string fields ]
  | Check.Invalid_field { field; error } ->
    let open Conv in
    record
      [ "kind", string "invalid field"
      ; "field", nel_of string field
      ; "error", snd (value_error_to_repr error)
      ]
;;

let value_error ppf err =
  let s, r = value_error_to_repr err in
  Format.fprintf ppf "%s @[%a@]" s repr r
;;
