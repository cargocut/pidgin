(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

type 'a t =
  { conv : 'a Repr.conv
  ; check : 'a Check.t
  }

let make ~conv ~check = { conv; check }
let conv { conv; _ } = conv
let check { check; _ } = check

let invmap f g { conv; check } =
  { check = (fun x -> x |> check |> Result.map f)
  ; conv = (fun x -> x |> g |> conv)
  }
;;

let refine f g { conv; check } =
  { check = (fun x -> Result.bind (check x) f)
  ; conv = (fun x -> x |> g |> conv)
  }
;;
