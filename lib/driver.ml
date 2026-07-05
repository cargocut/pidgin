(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

module type SOURCE = sig
  type t

  val translate_to_pidgin : t -> Repr.t
end

module type TARGET = sig
  type t

  val translate_from_pidgin : Repr.t -> t
end

module Sexp = struct
  type t = Sexp.t

  let translate_from_pidgin = Sexp.translate_from_pidgin
  let translate_to_pidgin = Sexp.translate_to_pidgin
end

module Yojson = struct
  type t =
    [ `Null
    | `Bool of bool
    | `Int of int
    | `Float of float
    | `String of string
    | `Assoc of (string * t) list
    | `List of t list
    ]

  let rec translate_from_pidgin = function
    | Repr.Null -> `Null
    | Repr.Bool b -> `Bool b
    | Repr.Int i -> `Int i
    | Repr.Float f -> `Float f
    | Repr.String s -> `String s
    | Repr.List xs -> `List (List.map translate_from_pidgin xs)
    | Repr.Record xs ->
      `Assoc (List.map (fun (k, v) -> k, translate_from_pidgin v) xs)
  ;;

  let rec translate_to_pidgin = function
    | `Null -> Repr.null ()
    | `Bool b -> Repr.bool b
    | `Int i -> Repr.int i
    | `Float f -> Repr.float f
    | `String s -> Repr.string s
    | `List xs -> Repr.list_of translate_to_pidgin xs
    | `Assoc xs ->
      Repr.record (List.map (fun (k, v) -> k, translate_to_pidgin v) xs)
  ;;
end

module Ezjsonm = struct
  type t =
    [ `Null
    | `Bool of bool
    | `Float of float
    | `String of string
    | `A of t list
    | `O of (string * t) list
    ]

  let rec translate_from_pidgin = function
    | Repr.Null -> `Null
    | Repr.Bool b -> `Bool b
    | Repr.Int i -> `Float (float_of_int i)
    | Repr.Float f -> `Float f
    | Repr.String s -> `String s
    | Repr.List xs -> `A (List.map translate_from_pidgin xs)
    | Repr.Record xs ->
      `O (List.map (fun (k, v) -> k, translate_from_pidgin v) xs)
  ;;

  let classify_number f =
    (* MAYBE: Should work on Js_of_ocaml. *)
    match Float.classify_float (fst (Float.modf f)) with
    | Float.FP_zero -> Repr.int (int_of_float f)
    | FP_normal | FP_subnormal | FP_infinite | FP_nan -> Repr.float f
  ;;

  let rec translate_to_pidgin = function
    | `Null -> Repr.null ()
    | `Bool b -> Repr.bool b
    | `Float f -> classify_number f
    | `String s -> Repr.string s
    | `A xs -> Repr.list_of translate_to_pidgin xs
    | `O xs ->
      Repr.record (List.map (fun (k, v) -> k, translate_to_pidgin v) xs)
  ;;
end
