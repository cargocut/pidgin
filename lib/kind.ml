(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** {1 Types} *)

type t =
  | Nothing
  | Opt of t
  | Bool
  | Int
  | Float
  | String
  | List of t list
  | Record of (string * t) list

module Construct = struct
  type 'a to_repr = 'a -> t
  type 'a to_field = string * 'a -> string * t

  let nothing t = Nothing
  let opt t = Opt t
  let opt_of f t = Opt (f t)
  let bool _ = Bool
  let bool _ = Bool
  let int _ = Int
  let float _ = Float
  let string _ = String
  let list v = List v
  let list_of f v = list (List.map f v)
  let record fields = Record fields
  let record_of f v = record (List.map f v)
  let contramap f conv d = conv (f d)
end

module Deconstruct = struct
  let fold ~nothing ~opt ~bool ~int ~float ~string ~list ~record = function
    | Nothing -> nothing ()
    | Opt t -> opt t
    | Bool -> bool ()
    | Int -> int ()
    | Float -> float ()
    | String -> string ()
    | List l -> list l
    | Record f -> record f
  ;;

  let fold_partial
        ?nothing
        ?opt
        ?bool
        ?int
        ?float
        ?string
        ?list
        ?record
        ~default
        t
    =
    let or_else f1 f2 = Option.value f1 ~default:(fun _ -> default t) in
    fold
      ~nothing:(or_else nothing default)
      ~opt:(or_else opt default)
      ~bool:(or_else bool default)
      ~int:(or_else int default)
      ~float:(or_else float default)
      ~string:(or_else string default)
      ~list:(or_else list default)
      ~record:(or_else record default)
      t
  ;;
end
