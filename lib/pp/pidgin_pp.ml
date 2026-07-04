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
