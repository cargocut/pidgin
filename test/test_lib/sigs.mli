(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

module type archetype = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val testable : t Alcotest.testable
  val to_pidgin : t Repr.conv
  val from_pidgin : t Check.t
end
