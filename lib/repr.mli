(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** Broadly speaking, Pidgin describes a {i "generic"} key-value
    language that serves as an intermediate format between several
    other key-value formats (such as JSON, TOML, YAML, etc.). [Repr]
    describes the abstract representation of this language. *)

(** {1 Types} *)

(** The abstract representation of the language is relatively
    straightforward (and compact), closely resembling the
    representation chosen to describe JSON.

    To construct terms that are more complex than those offered
    by the generic representation, the module provides combinators
    that allow for a high degree of flexibility in description. *)

(** Since type {!type:t} is non-abstract, this allows for the creation
    of data structures that may seem unusual (such as empty records);
    however, in practice, checking static invariants complicates the
    use of the library and results in a lack of flexibility. Since
    conversion functions to {!type:t}` are generally associated with
    validation functions, it is assumed that the validations will
    handle these unusual cases. *)

(** The abstract representation of language. *)
type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Record of (string * t) list

(** Describes a function that converts an arbitrary value to
    {!type:t}. Since ['a] is in contravariant position, ['a conv] is a
    contravariant functor that can be used with {!val:using}. *)
type 'a conv = 'a -> t

(** {1 Term construction}

    Combinators for constructing values in the representation
    {!type:t}. *)

(** Discard the input and returns [Null]. *)
val null : 'a conv

(** [bool] converter. *)
val bool : bool conv

(** [int] converter. *)
val int : int conv

(** [float] converter. *)
val float : float conv

(** [string] converter. *)
val string : string conv

(** [list] converter. *)
val list : t list conv

(** [list_of conv l] build a list of {!val:t} using [conv]. *)
val list_of : 'a conv -> 'a list conv

(** Build a record. If the flag [normalize_keys] is true (default value)
    keys are lowercased and trimmed. *)
val record : ?normalize_keys:bool -> (string * t) list conv

(** {2 Specific terms}

    Make use of the minimal nature of ASTs to construct more complex
    expressions. Mostly using records. *)

(** [option conv] converter for option. use {!val:null} for describing
    [None] and [conv] for [Some]. *)
val option : 'a conv -> 'a option conv

(** [sum f x] produce a sum-type for a given [x]. The function [f]
    return a couple of constructor-name and value. The result is
    wrapped into the following record:
    [{constr = "constr-name"; value = value}]. *)
val sum : ('a -> string * t) -> 'a -> t

(** [pair fst snd (a, b)] produce a product type for a given pair [a, b].
    The result is wrapped into the following record:
    [{first = a; second = b}]. *)
val pair : 'a conv -> 'b conv -> ('a * 'b) conv

(** [result ~ok ~error] is a converter for result values. It uses
    {!val:sum} under the hood. *)
val result : ok:'a conv -> error:'b conv -> ('a, 'b) result conv

(** [either ~ok ~error] is a converter for Either values. It uses
    {!val:sum} under the hood. *)
val either : left:'a conv -> right:'b conv -> ('a, 'b) Either.t conv

(** [triple ca cb cd] is a converter for triples. Under the hood,
    [triple] are {!val:pair}. *)
val triple : ('a -> t) -> ('b -> t) -> ('c -> t) -> ('a * 'b * 'c) conv

(** [int32] is a converter for [int32]. It use
    [{constr = "int32"; value = string n}] as an underlying representation. *)
val int32 : int32 conv

(** [int64] is a converter for [int64]. It use
    [{constr = "int64"; value = string n}] as an underlying representation. *)
val int64 : int64 conv

(** {1 Mapping} *)

(** [using f conv] is a contramap. If we have a function from [b] to [a],
    then we can get a conversion from [a] to [t] to [b] to [t]. *)
val using : ('b -> 'a) -> 'a conv -> 'b conv

(** {1 Equality} *)

(** Equality between terms. *)
val equal : t -> t -> bool

(** {1 Case analysis using folds}

    Enables case analysis by using folds (catamorphisms) to
    standardize access to the various branches of the AST. *)

(** Exhaustive match over {!type:t}. *)
val fold
  :  null:(unit -> 'a)
  -> bool:(bool -> 'a)
  -> int:(int -> 'a)
  -> float:(float -> 'a)
  -> string:(string -> 'a)
  -> list:(t list -> 'a)
  -> record:((string * t) list -> 'a)
  -> t
  -> 'a

(** Partial match over {!type:t}. *)
val fold_partial
  :  ?null:(unit -> 'a)
  -> ?bool:(bool -> 'a)
  -> ?int:(int -> 'a)
  -> ?float:(float -> 'a)
  -> ?string:(string -> 'a)
  -> ?list:(t list -> 'a)
  -> ?record:((string * t) list -> 'a)
  -> (t -> 'a)
  -> t
  -> 'a
