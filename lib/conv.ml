(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

type 'a t = 'a -> Repr.t

module type S = Repr.PROJECTABLE

let into (type a) (module P : S with type t = a) x = P.to_pidgin x
let using f conv x = conv (f x)
let replace n = using (fun _ -> n)
let null _ = Repr.null ()
let bool = Repr.bool
let int = Repr.int
let float = Repr.float
let string = Repr.string
let list = Repr.list
let list_of = Repr.list_of
let nel = Repr.nel
let nel_of = Repr.nel_of
let char = Repr.char
let record = Repr.record
let option = Repr.option
let sum = Repr.sum
let pair = Repr.pair
let result = Repr.result
let either = Repr.either
let triple = Repr.triple
let int32 = Repr.int32
let int64 = Repr.int64

module Infix = struct
  let ( <$> ) = using
end

include Infix
