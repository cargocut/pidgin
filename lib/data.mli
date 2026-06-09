(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** {b Pidgin} is agnostic data representation mainly inspired by Json *)

(** The goal of data representation is to offer an expressive and general intermediate layer for data management
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
  type 'a to_data = 'a -> t
  type 'a to_field = string * 'a -> string * t

  (** [null] returns the {!type:t} for [null].*)
  val null : 'a to_data

  (** [bool b] converts a Boolean into a {!type:t}. *)
  val bool : bool to_data

  (** [int i] converts an integer into a {!type:t}. *)
  val int : int to_data

  (** [float f] converts a float into a {!type:t}. *)
  val float : float to_data

  (** [string s] converts a string into a {!type:t}. *)
  val string : string to_data

  (** [list v] converts a list of {!type:t} into a {!type:t}. *)
  val list : t list to_data

  (** [record f fields] converts a list of {!type:t} into a {!type:t}. *)
  val record : (string * t) list to_data

  (** [list_of f v] converts thanks to {f} a list of {!type:'a} into a {!type:t}. *)
  val list_of : 'a to_data -> 'a list to_data

  (** [record_of f fields] converts thanks to {f}  a list of {!type:string * 'a} into a {!type:t}. *)
  val record_of : 'a to_field -> (string * 'a) list to_data

  val contramap : ('a -> 'b) -> 'b to_data -> 'a to_data
end

(** {2 Catamorphisms} *)

module Deconstruct : sig
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
