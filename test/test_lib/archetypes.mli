(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** Some archetypes for having data in tests. *)

module Gender : sig
  include Sigs.archetype

  val male : t
  val female : t
  val other : string -> t
end

module Human : sig
  include Sigs.archetype

  val make
    :  ?firstname:string
    -> ?lastname:string
    -> ?age:int
    -> nickname:string
    -> gender:Gender.t
    -> unit
    -> t
end

module User : sig
  include Sigs.archetype

  val make : email:string -> ?level:int -> human:Human.t -> unit -> t
end
