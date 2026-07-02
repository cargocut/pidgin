(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

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
      "@[<hov 1>{%a}@]"
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
      "@[<hov 1>(%a)@]"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@;| ") kind)
      l
  | Kind.Pair (a, b) -> fprintf ppf "@[(%a@ * %a)@]" kind a kind b
  | Kind.Branch (k, v) -> fprintf ppf "#%s<@[%a@]>" (Misc.strim k) kind v
  | Kind.Record fields ->
    fprintf
      ppf
      "@[<hov 1>{%a}@]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ",@;")
         (fun ppf (key, value) -> fprintf ppf "@[<1>%S:@ %a@]" key kind value))
      fields
;;

let rec sexp ppf = function
  | Sexp.Atom x -> Format.fprintf ppf "%s" (Misc.escape_spaces x)
  | Sexp.Node x -> Format.fprintf ppf "@[<hov 1>(%a)@]" sexp_list x

and sexp_list ppf = function
  | x :: (_ :: _ as xs) ->
    let () = Format.fprintf ppf "%a@ " sexp x in
    sexp_list ppf xs
  | x :: xs ->
    let () = Format.fprintf ppf "%a" sexp x in
    sexp_list ppf xs
  | [] -> ()
;;

let field_with_alt Nel.(field :: alt) =
  match alt with
  | [] -> Repr.string field
  | _ -> Repr.(record [ "main", string field; "other", list_of string alt ])
;;

let rec error_for_value_to_repr = function
  | Error.Check.Unexpected_kind { expected; given; value } ->
    Repr.(
      record
        [ "kind", string "unexpected_kind"
        ; "value", value
        ; "expected", string @@ Format.asprintf "%a" kind expected
        ; "given", string @@ Format.asprintf "%a" kind given
        ])
  | Error.Check.Invalid_list { errors; value } ->
    Repr.(
      record
        [ "kind", string "invalid_list"
        ; "value", value
        ; ( "errors"
          , list_of
              (fun (i, error) ->
                 record [ "at", int i; "error", error_for_value_to_repr error ])
              (Nel.to_list errors) )
        ])
  | Error.Check.Invalid_record { errors; value } ->
    Repr.(
      record
        [ "kind", string "invalid_record"
        ; "value", value
        ; "errors", list_of error_for_record_to_repr (Nel.to_list errors)
        ])
  | Error.Check.Unexpected_value { value; message } ->
    Repr.(
      record
        [ "kind", string "unexpected_value"
        ; "message", string message
        ; "value", option Fun.id value
        ])

and error_for_record_to_repr = function
  | Error.Check.Missing_field field ->
    Repr.(
      record [ "kind", string "missing_field"; "field", field_with_alt field ])
  | Error.Check.Invalid_subrecord err ->
    Repr.(
      record
        [ "kind", string "invalid_subrecord"
        ; "errors", error_for_value_to_repr err
        ])
  | Error.Check.Invalid_field { field; error } ->
    Repr.(
      record
        [ "kind", string "invalid_field"
        ; "field", field_with_alt field
        ; "error", error_for_value_to_repr error
        ])
;;

let error_for_value ppf err =
  err |> error_for_value_to_repr |> Format.fprintf ppf "%a" repr
;;

let error_for_sexp_parsing_to_repr = function
  | Error.Sexp.Non_terminated_node pos ->
    Repr.(record [ "kind", string "non_terminated_node"; "position", int pos ])
;;

let error_for_sexp_parsing ppf err =
  err |> error_for_sexp_parsing_to_repr |> Format.fprintf ppf "%a" repr
;;

let result ok error =
  let open Format in
  pp_print_result
    ~ok:(fun ppf x -> fprintf ppf "Ok @[<hov 1>%a@]" ok x)
    ~error:(fun ppf x -> fprintf ppf "Error @[<hov 1>%a@]" error x)
;;

let checked_value ok = result ok error_for_value
let sexp_parsed = result sexp error_for_sexp_parsing
