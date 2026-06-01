(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

let null_string = "null"
let list_string l = "[" ^ String.concat "," l ^ "]"
let record_string r = "{" ^ String.concat "," r ^ "}"
let attribute_string k s = k ^ ":" ^ s

let rec to_string e =
  let open Data.Deconstruct in
  fold
    ~null:(fun () -> null_string)
    ~bool:string_of_bool
    ~int:string_of_int
    ~float:string_of_float
    ~string:(fun s -> s)
    ~list:(fun l -> list_string (List.map to_string l))
    ~record:(fun r ->
      record_string
        (List.map (fun (k, e) -> attribute_string k (to_string e)) r))
    e
;;
