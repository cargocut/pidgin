(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** {b Pidgin} is agnostic data type representation *)

(** The goal of data type representation is to offer an expressive and general intermediate layer for data type management
*)

(** {1 Types} *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Record of (string * t) list

(** {1 Creating Data values} *)

module Construct : sig
  type 'a to_repr = 'a -> t
  type 'a to_field = string * 'a -> string * t

  (** [null] returns the {!type:t} for [null].*)
  val null : 'a to_repr

  (** [bool b] converts a Boolean into a {!type:t}. *)
  val bool : bool to_repr

  (** [int i] converts an integer into a {!type:t}. *)
  val int : int to_repr

  (** [float f] converts a float into a {!type:t}. *)
  val float : float to_repr

  (** [string s] converts a string into a {!type:t}. *)
  val string : string to_repr

  (** [list l] converts a list of {!type:t} into a {!type:t}. *)
  val list : t list to_repr

  (** [record f r] converts a list of {!type:t} into a {!type:t}. *)
  val record : (string * t) list to_repr

  (** [list_of f l] converts thanks to {!f} a list of {!type:'a} into a {!type:t}.
  *)
  val list_of : 'a to_repr -> 'a list to_repr

  (** [record_of f r] converts thanks to {!f}  a list of {!type:string * 'a} into a {!type:t}.
  *)
  val record_of : 'a to_field -> (string * 'a) list to_repr

  (** [contramap f v] maps over inputs thanks to {!f} since {to_repr} is contravariant *)
  val contramap : ('a -> 'b) -> 'b to_repr -> 'a to_repr
end

(** {2 Catamorphisms} *)

module Deconstruct : sig
  type error = Invalid_kind of Kind.t
  type 'a from_repr = t -> ('a, error) result

  (** [null] returns the {!type:t} for [null].*)
  val null : unit from_repr

  (** [bool b] converts a Boolean into a {!type:t}. *)
  val bool : bool from_repr

  (** [int i] converts an integer into a {!type:t}. *)
  val int : int from_repr

  (** [float f] converts a float into a {!type:t}. *)
  val float : float from_repr

  (** [string s] converts a string into a {!type:t}. *)
  val string : string from_repr

  (** [list l] converts a list of {!type:t} into a {!type:t}. *)
  val list : t list from_repr

  (** [record f r] converts a list of {!type:t} into a {!type:t}. *)
  val record : (string * t) list from_repr

  (** [fold ~null ~bool ~int ~float ~string ~list ~record d] *)
  val fold
    :  null:(unit -> 'b)
    -> bool:(bool -> 'b)
    -> int:(int -> 'b)
    -> float:(float -> 'b)
    -> string:(string -> 'b)
    -> list:(t list -> 'b)
    -> record:((string * t) list -> 'b)
    -> t
    -> 'b

  (** [fold_partial ~null ~bool ~int ~float ~string ~list ~record ~default d] *)
  val fold_partial
    :  ?null:(unit -> 'b)
    -> ?bool:(bool -> 'b)
    -> ?int:(int -> 'b)
    -> ?float:(float -> 'b)
    -> ?string:(string -> 'b)
    -> ?list:(t list -> 'b)
    -> ?record:((string * t) list -> 'b)
    -> default:(t -> 'b)
    -> t
    -> 'b
end
