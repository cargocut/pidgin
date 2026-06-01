(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Record of (string * t) list

module Construct = struct
  let null = Null
  let bool b = Bool b
  let int i = Int i
  let float f = Float f
  let string s = String s
  let list v = List v
  let record fields = Record fields
end

module Deconstruct = struct
  let fold =
    fun ~null ~bool ~int ~float ~string ~list ~record -> function
    | Null -> null ()
    | Bool b -> bool b
    | Int i -> int i
    | Float f -> float f
    | String s -> string s
    | List l -> list l
    | Record f -> record f
  ;;

  let fold_opt =
    fun ?null ?bool ?int ?float ?string ?list ?record ~default t ->
    let or_else f1 f2 = Option.value f1 ~default:(fun _ -> default t) in
    fold
      ~null:(or_else null default)
      ~bool:(or_else bool default)
      ~int:(or_else int default)
      ~float:(or_else float default)
      ~string:(or_else string default)
      ~list:(or_else list default)
      ~record:(or_else record default)
      t
  ;;
end
