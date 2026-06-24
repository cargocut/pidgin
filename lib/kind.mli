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

module Construct : sig
  type 'a to_repr = 'a -> t
  type 'a to_field = string * 'a -> string * t

  (** [nothing v] converts a Nothing into a {!type:t}. *)
  val nothing : 'a to_repr

  (** [opt t] converts an optional {!type:t} into a {!type:t}. *)
  val opt : t to_repr

  (** [opt_of f l] converts thanks to {f} an optional {!type:'a} into a {!type:t}. *)
  val opt_of : 'a to_repr -> 'a to_repr

  (** [bool b] converts a Boolean into a {!type:t}. *)
  val bool : 'a to_repr

  (** [int i] converts an integer into a {!type:t}. *)
  val int : 'a to_repr

  (** [float f] converts a float into a {!type:t}. *)
  val float : 'a to_repr

  (** [string s] converts a string into a {!type:t}. *)
  val string : 'a to_repr

  (** [list l] converts a list of {!type:t} into a {!type:t}. *)
  val list : t list to_repr

  (** [list_of f l] converts thanks to {f} a list of {!type:'a} into a {!type:t}. *)
  val list_of : 'a to_repr -> 'a list to_repr

  (** [record f r] converts a list of {!type:t} into a {!type:t}. *)
  val record : (string * t) list to_repr

  (** [record_of f r] converts thanks to {f}  a list of {!type:string * 'a} into a {!type:t}. *)
  val record_of : 'a to_field -> (string * 'a) list to_repr

  (** [contramap f v] maps over inputs since {to_repr} is contravariant *)
  val contramap : ('a -> 'b) -> 'b to_repr -> 'a to_repr
end

module Deconstruct : sig
  (** [fold ~null ~bool ~int ~float ~string ~list ~record d] *)
  val fold
    :  nothing:(unit -> 'b)
    -> opt:(t -> 'b)
    -> bool:(unit -> 'b)
    -> int:(unit -> 'b)
    -> float:(unit -> 'b)
    -> string:(unit -> 'b)
    -> list:(t list -> 'b)
    -> record:((string * t) list -> 'b)
    -> t
    -> 'b

  val fold_partial
    :  ?nothing:(unit -> 'b)
    -> ?opt:(t -> 'b)
    -> ?bool:(unit -> 'b)
    -> ?int:(unit -> 'b)
    -> ?float:(unit -> 'b)
    -> ?string:(unit -> 'b)
    -> ?list:(t list -> 'b)
    -> ?record:((string * t) list -> 'b)
    -> default:(t -> 'b)
    -> t
    -> 'b
end
