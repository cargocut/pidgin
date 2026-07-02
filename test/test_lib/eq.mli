(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** Some equalities function (mostly used for tests) *)

(** Equality between {!type:Check.value_error}. *)
val check_error : Check.value_error -> Check.value_error -> bool
