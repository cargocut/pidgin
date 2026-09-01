(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** A [Prism] is used to describe a codec that enables serialization
    and deserialization (it consists of a {!type:Repr.conv} and a
    {!type:Check.t} pair). *)

(** {1 Types} *)

(** The type that describes the pair of converter and validator. *)
type 'a t

(** {1 Building prisms} *)

(** [make ~conv ~check] constructs a prism for type ['a]. *)
val make : conv:'a Repr.conv -> check:'a Check.t -> 'a t

(** {1 Running prisms} *)

(** [conv prism] extract the prism conversion function. *)
val conv : 'a t -> 'a Repr.conv

(** [check prism] extract the prism validation function. *)
val check : 'a t -> 'a Check.t

(** {1 Helpers}

    A few additional tools for working with prisms. *)

(** [invmap f g prism] map both [f] and [g] on, respectively, [check]
    and [conv]. *)
val invmap : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t

(** [refine f g prism] bind [f] and map [g] on, respectively, [check]
    and [conv]. *)
val refine : ('a, 'b) Check.fn -> ('b -> 'a) -> 'a t -> 'b t
