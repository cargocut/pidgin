(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** Some Alcotest testables. *)

(** A testable for [Repr]. *)
val repr : Repr.t Alcotest.testable

(** A testable for [Kind]. *)
val kind : Kind.t Alcotest.testable

(** A testable for validated values. *)
val checked : 'a Alcotest.testable -> 'a Check.value Alcotest.testable
