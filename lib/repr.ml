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
  type 'a to_repr = 'a -> t
  type 'a to_field = string * 'a -> string * t

  let null _ = Null
  let bool b = Bool b
  let int i = Int i
  let float f = Float f
  let string s = String s
  let list v = List v
  let list_of f v = list (List.map f v)
  let record fields = Record fields
  let record_of f v = record (List.map f v)
  let contramap f conv d = conv (f d)
end

module Deconstruct = struct
  type error =
    | Invalid_kind of
        { expecting : Kind.t
        ; given : Kind.t
        }

  type 'a from_repr = t -> ('a, error) result

  let fold ~null ~bool ~int ~float ~string ~list ~record = function
    | Null -> null ()
    | Bool b -> bool b
    | Int i -> int i
    | Float f -> float f
    | String s -> string s
    | List l -> list l
    | Record f -> record f
  ;;

  let fold_partial ?null ?bool ?int ?float ?string ?list ?record ~default t =
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

  let eta_expand f = fun x -> f x
  let invalid_kind expecting given = Invalid_kind { expecting; given }

  let rec type_of_repr t =
    fold
      ~null:(fun _ -> Kind.Nothing)
      ~bool:(fun _ -> Kind.Bool)
      ~int:(fun _ -> Kind.Int)
      ~float:(fun _ -> Kind.Float)
      ~string:(fun _ -> Kind.String)
      ~list:(fun l -> Kind.List (List.map type_of_repr l))
      ~record:(fun l ->
        Kind.Record (List.map (fun (n, v) -> n, type_of_repr v) l))
      t
  ;;

  let null =
    eta_expand
    @@ fold_partial
         ~null:(fun v -> Result.ok v)
         ~default:(fun v ->
           Result.error @@ invalid_kind Kind.Nothing (type_of_repr v))
  ;;

  let bool =
    eta_expand
    @@ fold_partial
         ~bool:(fun b -> Result.ok b)
         ~default:(fun v ->
           Result.error @@ invalid_kind Kind.Bool (type_of_repr v))
  ;;

  let int =
    eta_expand
    @@ fold_partial
         ~int:(fun v -> Result.ok v)
         ~default:(fun v ->
           Result.error @@ invalid_kind Kind.Int (type_of_repr v))
  ;;

  let float =
    eta_expand
    @@ fold_partial
         ~float:(fun v -> Result.ok v)
         ~default:(fun v ->
           Result.error @@ invalid_kind Kind.Float (type_of_repr v))
  ;;

  let string =
    eta_expand
    @@ fold_partial
         ~string:(fun v -> Result.ok v)
         ~default:(fun v ->
           Result.error @@ invalid_kind Kind.String (type_of_repr v))
  ;;

  let list =
    eta_expand
    @@ fold_partial
         ~list:(fun v -> Result.ok v)
         ~default:(fun v ->
           Result.error @@ invalid_kind (Kind.List []) (type_of_repr v))
  ;;

  let record =
    eta_expand
    @@ fold_partial
         ~record:(fun v -> Result.ok v)
         ~default:(fun v ->
           Result.error @@ invalid_kind (Kind.Record []) (type_of_repr v))
  ;;
end
