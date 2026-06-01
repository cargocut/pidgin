(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** {b Pidgin} is agnostic data representation mainly inspired by Json *)

(** The goal of data representation is to offer an expressive and general intermediate layer for data management
*)

(** {1 Types} *)

type t

(** {1 Creating Data values} *)

module Construct : sig
  (** [null] returns the {!type:t} for [null].*)
  val null : t

  (** [bool b] converts a Boolean into a {!type:t}. *)
  val bool : bool -> t

  (** [int i] converts an integer into a {!type:t}. *)
  val int : int -> t

  (** [float f] converts a float into a {!type:t}. *)
  val float : float -> t

  (** [string s] converts a string into a {!type:t}. *)
  val string : string -> t

  (** [list v] converts a list of {!type:t} into a {!type:t}. *)
  val list : t list -> t

  (** [record fields] converts a list of {!type:t} into a {!type:t}. *)
  val record : (string * t) list -> t
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

  (** [fold_opt ~null ~bool ~int ~float ~string ~list ~record d] *)
  val fold_opt
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
