(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** Describes all reusable interfaces. *)

(** {1 Drivers}

    A driver allows Pidgin to be used as an exchange format. It
    exposes two signatures that enable conversion to {!type:Repr.t}
    and conversion from {!type:Repr.t}, respectively. *)

module type SOURCE = sig
  (** A source is an API associated with a type {!type:t} that can be
      converted to {!type:Repr.t}. It is typically used when a file
      (for example) is dumped into a specific format (such as JSON or
      Sexp) and is read in order to be converted to Pidgin's generic
      format. *)

  (** The type that describes the data source. *)
  type t

  (** [translate_to_pidgin source] must construct a valid Pidgin value
      ({!type:Repr.t}) from a [source]. Error handling is left to the
      consumer, even though, in an ideal world, we would want to be
      able to fill any value to Pidgin constructors. *)
  val translate_to_pidgin : t Repr.conv
end

module type TARGET = sig
  (** A target is an API associated with a type {!type:t} that can be
      converted from {!type:Repr.t}. It is used when you want to dump
      data to a file in a specific format (such as JSON or Sexp). *)

  (** The type that describes the data target. *)
  type t

  (** [translate_from_pidgin repr] must construct a valid expression
      from {!type:Repr.t} to the [target]. Error handling is left to
      the consumer, even though, in an ideal world, we would want to
      be able to fill any value to the target constructors. *)
  val translate_from_pidgin : Repr.t -> t
end
