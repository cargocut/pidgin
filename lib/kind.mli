(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** A Kind is a type of light type. It holds less information than a
    full-fledged type system and is used primarily for generating
    error messages (and perhaps, in the near future, for
    defunctionalize validation functions). Naively, they can be viewed
    as a representation that does not hold the value of a term
    described by {!type:Repr.t}. *)

(** Even though the API is user-facing, it's best to build validators
    that allow users to ignore it. *)

(** {1 Types} *)

(** The type describing a [Kind]. Unlike representations, a user does
    not want to manipulate them explicitly, which is why they remain
    private. (Not abstract for inspection) *)
type t = private
  | Any
  | Or of t list
  | Null
  | Bool
  | Int
  | Float
  | String
  | List of t
  | Branch of string * t
  | Pair of t * t
  | Record of (string * t) list

(** {1 Inference/Deduction} *)

(** [infer repr] deduce the kind of the given [repr]. *)
val infer : Repr.t -> t

(** {1 Building kinds}

    For validation function, we want to be able to create kinds for
    error reporting. (Remember that kinds are mapped to
    {!type:Repr.t}) *)

(** Describe the sad [any] kind. When a value is not unifiable or
    inferable. *)
val any : t

(** Describe the kind for {!val:Repr.null}. *)
val null : t

(** Describe the kind for {!val:Repr.bool}. *)
val bool : t

(** Describe the kind for {!val:Repr.int}. *)
val int : t

(** Describe the kind for {!val:Repr.float}. *)
val float : t

(** Describe the kind for {!val:Repr.string}. *)
val string : t

(** Describe the kind for {!val:Repr.list}. *)
val list : t -> t

(** Describe the kind for {!val:Repr.record}. *)
val record : ?normalize_keys:bool -> (string * t) list -> t

(** Describe an union of kind. *)
val or_ : t -> t -> t

(** Reduce a list of kind one kind, using {!val:or_}. *)
val unify : t Nel.t -> t

(** Describe a branch. *)
val branch : string -> t -> t

(** Describe a sum type using {!val:or_}. *)
val sum : (string * t) Nel.t -> t

(** Describe a special kind for pair. *)
val pair : t -> t -> t

(** Build a tuple, if there is
    one value, it return it, otherwise it build a pair (possibly of
    pairs). {b Non Tail-recursive}. *)
val tuple : t Nel.t -> t

(** [triple a b c] is [pair a (pair b c)]. *)
val triple : t -> t -> t -> t

(** {1 Equality and comparison} *)

(** Equality between kinds. *)
val equal : t -> t -> bool

(** Comparison between kinds. *)
val compare : t -> t -> int

(** {1 Misc} *)

(** [to_string kind] dump the given [kind] in a readable way. *)
val to_string : t -> string
