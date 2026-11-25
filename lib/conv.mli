(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** Describes converters for lifting OCaml values into {!type:Repr.t}. *)

(** {1 Types} *)

(** Describes a function that converts an arbitrary value to
    {!type:Repr.t}. Since ['a] is in contravariant position, ['a conv] is a
    contravariant functor that can be used with {!val:using}. *)
type 'a t = 'a -> Repr.t

(** {1 Signatures} *)

module type S = Repr.PROJECTABLE

(** {1 Converters} *)

val null : 'a t

(** [bool] converter. *)
val bool : bool t

(** [int] converter. *)
val int : int t

(** [float] converter. *)
val float : float t

(** [string] converter. *)
val string : string t

(** [char] converter *)
val char : char t

(** [list] converter. *)
val list : Repr.t list t

(** [list_of conv l] build a list of {!type:Repr.t} using [conv]. *)
val list_of : 'a t -> 'a list t

(** Build a record. If the flag [normalize_keys] is true (default value)
    keys are lowercased and trimmed. *)
val record : ?normalize_keys:bool -> (string * Repr.t) list t

(** {2 Specific terms}

    Make use of the minimal nature of ASTs to construct more complex
    expressions. Mostly using records. *)

(** [option conv] converter for option. use {!val:null} for describing
    [None] and [conv] for [Some]. *)
val option : 'a t -> 'a option t

(** [sum f x] produce a sum-type for a given [x]. The function [f]
    return a couple of constructor-name and value. The result is
    wrapped into the following record:
    [{constr = "constr-name"; value = value}]. *)
val sum : ('a -> string * Repr.t) -> 'a t

(** [pair fst snd (a, b)] produce a product type for a given pair [a, b].
    The result is wrapped into the following record:
    [{first = a; second = b}]. *)
val pair : 'a t -> 'b t -> ('a * 'b) t

(** [result ~ok ~error] is a converter for result values. It uses
    {!val:sum} under the hood. *)
val result : ok:'a t -> error:'b t -> ('a, 'b) result t

(** [either ~ok ~error] is a converter for Either values. It uses
    {!val:sum} under the hood. *)
val either : left:'a t -> right:'b t -> ('a, 'b) Either.t t

(** [triple ca cb cd] is a converter for triples. Under the hood,
    [triple] are {!val:pair}. *)
val triple
  :  ('a -> Repr.t)
  -> ('b -> Repr.t)
  -> ('c -> Repr.t)
  -> ('a * 'b * 'c) t

(** [int32] is a converter for [int32]. It use
    [{constr = "int32"; value = string n}] as an underlying representation. *)
val int32 : int32 t

(** [int64] is a converter for [int64]. It use
    [{constr = "int64"; value = string n}] as an underlying representation. *)
val int64 : int64 t

(** [Non empty list] converter. *)
val nel : Repr.t Nel.t t

(** [nel_of conv l] build a non-empty-list of {!type:Repr.t} using
    [conv]. *)
val nel_of : 'a t -> 'a Nel.t t

(** {2 Specific converters} *)

(** [into (module Proj)] converts value using [Proj.to_pidgin]. *)
val into : (module S with type t = 'a) -> 'a t

(** {1 Mapping} *)

(** [using f conv] is a contramap. If we have a function from [b] to [a],
    then we can get a conversion from [a] to [t] to [b] to [t]. *)
val using : ('b -> 'a) -> 'a t -> 'b t

(** [replace k] is [using (fun _ -> k)]. *)
val replace : 'b -> 'b t -> 'a t

(** {1 Infix operators} *)

module Infix : sig
  (** Some infix helpers. *)

  (** [f <$> conv] is [using f conv]. *)
  val ( <$> ) : ('b -> 'a) -> 'a t -> 'b t
end

include module type of Infix
